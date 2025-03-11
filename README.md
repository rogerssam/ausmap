# Australia Map Data Plotter

A responsive R Shiny web application for visualizing spatial data points on a map of Australia.

## Features

- **File Upload**: Import data from CSV or Excel (XLS/XLSX) files
- **Mobile-Friendly**: Responsive HTML5 design that works on all devices
- **Interactive Mapping**: Visualize point data with customizable options
- **Flexible Data Handling**:
  - Dynamic column selection
  - Support for both numeric and categorical variables
  - Data validation and error handling

## Requirements

- R (>= 4.0.0)
- Required R packages:
  - shiny
  - leaflet
  - readxl
  - readr
  - dplyr
  - DT
  - shinyjs
  - shinydashboard
  - shinythemes
  - shinyWidgets
  - sf

## Installation

```r
# Install required packages
install.packages(c("shiny", "leaflet", "readxl", "readr", "dplyr", 
                  "DT", "shinyjs", "shinydashboard", "shinythemes",
                  "shinyWidgets", "sf"))
```

## Usage

1. Clone this repository or download the app.R file
2. Open R or RStudio and set your working directory to the folder containing app.R
3. Run the following command, or alternatively, visit https://biometryhub.shinyapps.io/ausmap/ for a live version.

```r
shiny::runApp()
```

4. Upload your CSV or Excel file with latitude and longitude data
5. Select the appropriate columns using the dropdown menus
6. Click "Plot Data" to visualize your points on the map

## Data Format
Your data should include at minimum:

- A column for latitude values (numeric, in decimal degrees)
- A column for longitude values (numeric, in decimal degrees)
- Optional additional columns for colouring or faceting

## Screenshots

![image](https://github.com/user-attachments/assets/01bcea2e-4df4-47d1-959e-1e71d869b396)

## License

MIT License

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
