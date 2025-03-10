# app.R
library(shiny)
library(leaflet)
library(readxl)
library(readr)
library(dplyr)
library(DT)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sf)

# Define the UI
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    useShinyjs(),
    tags$head(
        tags$style(HTML("
      @media (max-width: 768px) {
        .col-sm-4 {
          width: 100%;
          padding-bottom: 20px;
        }
        .col-sm-8 {
          width: 100%;
        }
      }
      .leaflet-container {
        height: 70vh !important;
      }
      .well {
        margin-bottom: 15px;
      }
    "))
    ),

    # Application title
    titlePanel("Australia Map Data Plotter"),

    # Sidebar layout
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            width = 4,
            # File upload input
            wellPanel(
                fileInput("dataFile", "Upload CSV, XLS, or XLSX file",
                          accept = c(".csv", ".xls", ".xlsx")),

                # Only show after file is uploaded
                conditionalPanel(
                    condition = "output.fileUploaded",

                    # Sheet selection for Excel files
                    conditionalPanel(
                        condition = "output.isExcel",
                        selectInput("sheet", "Select Sheet:", choices = NULL)
                    ),

                    # Column selection inputs
                    selectInput("latCol", "Select Latitude Column:", choices = NULL),
                    selectInput("longCol", "Select Longitude Column:", choices = NULL),

                    # Optional mapping variables
                    selectInput("colorCol", "Color By (Optional):", choices = NULL, selected = NULL),
                    selectInput("facetCol", "Facet By (Optional):", choices = NULL, selected = NULL),

                    # Map controls
                    selectInput("mapType", "Map Type:",
                                choices = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")),

                    # Plot button
                    actionButton("plotBtn", "Plot Data", class = "btn-primary btn-block")
                )
            ),

            # Display information about data
            conditionalPanel(
                condition = "output.fileUploaded",
                wellPanel(
                    h4("Data Summary"),
                    verbatimTextOutput("dataSummary")
                )
            )
        ),

        # Main panel for displaying outputs
        mainPanel(
            width = 8,
            tabsetPanel(
                tabPanel("Map",
                         leafletOutput("map", height = "70vh"),
                         uiOutput("legendUI")),
                tabPanel("Data Preview",
                         DTOutput("dataTable"))
            )
        )
    )
)
