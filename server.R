# Define server logic
server <- function(input, output, session) {
    # Reactive values
    rv <- reactiveValues(
        data = NULL,
        isExcel = FALSE,
        sheets = NULL,
        australia_bbox = list(
            min_lat = -43.6345972634,
            max_lat = -10.6681857235,
            min_lng = 113.338953078,
            max_lng = 153.569469029
        )
    )

    # Process uploaded file
    observeEvent(input$dataFile, {
        req(input$dataFile)

        file_ext <- tools::file_ext(input$dataFile$name)

        # Check if file is Excel
        if (file_ext %in% c("xls", "xlsx")) {
            rv$isExcel <- TRUE
            rv$sheets <- readxl::excel_sheets(input$dataFile$datapath)
            updateSelectInput(session, "sheet", choices = rv$sheets)
        } else {
            rv$isExcel <- FALSE
            tryCatch({
                rv$data <- readr::read_csv(input$dataFile$datapath, show_col_types = FALSE)
            }, error = function(e) {
                showNotification(paste("Error reading file:", e$message), type = "error")
            })
        }
    })

    # Process Excel sheet selection
    observeEvent(input$sheet, {
        req(input$dataFile, rv$isExcel, input$sheet)

        tryCatch({
            rv$data <- readxl::read_excel(input$dataFile$datapath, sheet = input$sheet)
        }, error = function(e) {
            showNotification(paste("Error reading Excel sheet:", e$message), type = "error")
        })
    })

    # Update column selections when data changes
    observe({
        req(rv$data)

        cols <- names(rv$data)

        # Add blank option for optional selections
        optional_cols <- c("None" = "", cols)

        updateSelectInput(session, "latCol", choices = cols)
        updateSelectInput(session, "longCol", choices = cols)
        updateSelectInput(session, "colorCol", choices = optional_cols)
        updateSelectInput(session, "facetCol", choices = optional_cols)
    })

    # Output flags for conditional panels
    output$fileUploaded <- reactive({
        return(!is.null(rv$data))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

    output$isExcel <- reactive({
        return(rv$isExcel)
    })
    outputOptions(output, "isExcel", suspendWhenHidden = FALSE)

    # Data summary output
    output$dataSummary <- renderPrint({
        req(rv$data)
        cat("Rows:", nrow(rv$data), "\n")
        cat("Columns:", ncol(rv$data), "\n")
    })

    # Data table preview
    output$dataTable <- renderDT({
        req(rv$data)
        datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE))
    })

    # Create the plot when button is clicked
    observeEvent(input$plotBtn, {
        req(rv$data, input$latCol, input$longCol)

        # Validate data
        plot_data <- rv$data

        # Check if lat/long columns exist and contain numeric data
        if (!(input$latCol %in% names(plot_data) && input$longCol %in% names(plot_data))) {
            showNotification("Selected latitude or longitude columns don't exist", type = "error")
            return(NULL)
        }

        # Try to convert to numeric if not already
        tryCatch({
            plot_data[[input$latCol]] <- as.numeric(plot_data[[input$latCol]])
            plot_data[[input$longCol]] <- as.numeric(plot_data[[input$longCol]])
        }, warning = function(w) {
            showNotification("Latitude or longitude contains non-numeric values", type = "warning")
        }, error = function(e) {
            showNotification("Error converting latitude or longitude to numeric", type = "error")
            return(NULL)
        })

        # Filter out rows with NA in lat/long
        plot_data <- plot_data %>%
            filter(!is.na(!!sym(input$latCol)), !is.na(!!sym(input$longCol)))

        # Check if any data points remain
        if (nrow(plot_data) == 0) {
            showNotification("No valid coordinates to plot", type = "error")
            return(NULL)
        }

        # Check if coordinates are in reasonable range for Australia
        valid_coords <- plot_data %>%
            filter(
                !!sym(input$latCol) >= rv$australia_bbox$min_lat,
                !!sym(input$latCol) <= rv$australia_bbox$max_lat,
                !!sym(input$longCol) >= rv$australia_bbox$min_lng,
                !!sym(input$longCol) <= rv$australia_bbox$max_lng
            )

        if (nrow(valid_coords) < nrow(plot_data)) {
            percent_invalid <- round((1 - nrow(valid_coords) / nrow(plot_data)) * 100)
            showNotification(paste0(percent_invalid, "% of points are outside Australia and will be shown but not centered"),
                             type = "warning")
        }

        # Base map
        map <- leaflet() %>%
            addProviderTiles(input$mapType) %>%
            setView(lng = 133.7751, lat = -25.2744, zoom = 4)  # Center on Australia

        # Prepare color mapping if color column is selected
        if (input$colorCol != "") {
            # Check if the column exists
            if (!(input$colorCol %in% names(plot_data))) {
                showNotification("Selected color column doesn't exist", type = "error")
            } else {
                # Handle numeric vs. categorical coloring
                if (is.numeric(plot_data[[input$colorCol]])) {
                    pal <- colorNumeric(palette = "viridis", domain = plot_data[[input$colorCol]])

                    map <- map %>%
                        addCircleMarkers(
                            data = plot_data,
                            lng = ~get(input$longCol),
                            lat = ~get(input$latCol),
                            radius = 5,
                            color = ~pal(get(input$colorCol)),
                            stroke = FALSE,
                            fillOpacity = 0.8,
                            popup = ~paste(
                                "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                                "<strong>Long:</strong>", round(get(input$longCol), 4), "<br>",
                                "<strong>", input$colorCol, ":</strong>", get(input$colorCol)
                            )
                        ) %>%
                        addLegend(
                            position = "bottomright",
                            pal = pal,
                            values = plot_data[[input$colorCol]],
                            title = input$colorCol
                        )
                } else {
                    # Categorical coloring
                    unique_vals <- unique(plot_data[[input$colorCol]])
                    pal <- colorFactor(palette = "Set1", domain = unique_vals)

                    map <- map %>%
                        addCircleMarkers(
                            data = plot_data,
                            lng = ~get(input$longCol),
                            lat = ~get(input$latCol),
                            radius = 5,
                            color = ~pal(get(input$colorCol)),
                            stroke = FALSE,
                            fillOpacity = 0.8,
                            popup = ~paste(
                                "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                                "<strong>Long:</strong>", round(get(input$longCol), 4), "<br>",
                                "<strong>", input$colorCol, ":</strong>", get(input$colorCol)
                            )
                        ) %>%
                        addLegend(
                            position = "bottomright",
                            pal = pal,
                            values = unique_vals,
                            title = input$colorCol
                        )
                }
            }
        } else {
            # Simple points with no color mapping
            map <- map %>%
                addCircleMarkers(
                    data = plot_data,
                    lng = ~get(input$longCol),
                    lat = ~get(input$latCol),
                    radius = 5,
                    color = "#1E90FF",
                    stroke = FALSE,
                    fillOpacity = 0.8,
                    popup = ~paste(
                        "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                        "<strong>Long:</strong>", round(get(input$longCol), 4)
                    )
                )
        }

        # Handle faceting if selected
        if (input$facetCol != "") {
            # Check if the column exists
            if (!(input$facetCol %in% names(plot_data))) {
                showNotification("Selected facet column doesn't exist", type = "error")
            } else {
                # Create group selection control
                facet_vals <- sort(unique(plot_data[[input$facetCol]]))

                # Create different layer for each facet value
                for (val in facet_vals) {
                    facet_data <- plot_data %>% filter(get(input$facetCol) == val)

                    # Apply same coloring logic to each facet
                    if (input$colorCol != "" && input$colorCol %in% names(plot_data)) {
                        if (is.numeric(plot_data[[input$colorCol]])) {
                            pal <- colorNumeric(palette = "viridis", domain = plot_data[[input$colorCol]])

                            map <- map %>%
                                addCircleMarkers(
                                    data = facet_data,
                                    lng = ~get(input$longCol),
                                    lat = ~get(input$latCol),
                                    radius = 5,
                                    color = ~pal(get(input$colorCol)),
                                    stroke = FALSE,
                                    fillOpacity = 0.8,
                                    popup = ~paste(
                                        "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                                        "<strong>Long:</strong>", round(get(input$longCol), 4), "<br>",
                                        "<strong>", input$colorCol, ":</strong>", get(input$colorCol), "<br>",
                                        "<strong>", input$facetCol, ":</strong>", get(input$facetCol)
                                    ),
                                    group = as.character(val)
                                )
                        } else {
                            # Categorical coloring
                            unique_vals <- unique(plot_data[[input$colorCol]])
                            pal <- colorFactor(palette = "Set1", domain = unique_vals)

                            map <- map %>%
                                addCircleMarkers(
                                    data = facet_data,
                                    lng = ~get(input$longCol),
                                    lat = ~get(input$latCol),
                                    radius = 5,
                                    color = ~pal(get(input$colorCol)),
                                    stroke = FALSE,
                                    fillOpacity = 0.8,
                                    popup = ~paste(
                                        "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                                        "<strong>Long:</strong>", round(get(input$longCol), 4), "<br>",
                                        "<strong>", input$colorCol, ":</strong>", get(input$colorCol), "<br>",
                                        "<strong>", input$facetCol, ":</strong>", get(input$facetCol)
                                    ),
                                    group = as.character(val)
                                )
                        }
                    } else {
                        # Simple points with no color mapping
                        map <- map %>%
                            addCircleMarkers(
                                data = facet_data,
                                lng = ~get(input$longCol),
                                lat = ~get(input$latCol),
                                radius = 5,
                                color = "#1E90FF",
                                stroke = FALSE,
                                fillOpacity = 0.8,
                                popup = ~paste(
                                    "<strong>Lat:</strong>", round(get(input$latCol), 4), "<br>",
                                    "<strong>Long:</strong>", round(get(input$longCol), 4), "<br>",
                                    "<strong>", input$facetCol, ":</strong>", get(input$facetCol)
                                ),
                                group = as.character(val)
                            )
                    }
                }

                # Add layer controls for faceting
                map <- map %>%
                    addLayersControl(
                        overlayGroups = as.character(facet_vals),
                        options = layersControlOptions(collapsed = FALSE)
                    )
            }
        }

        # Render the map
        output$map <- renderLeaflet({
            map
        })
    })
}
