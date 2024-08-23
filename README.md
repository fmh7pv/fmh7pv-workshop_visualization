
# Shiny Web Application: Bar Graph of Demographics and Time 

This Shiny web application visualizes demographic data using a bar graphs. Users can filter and view data based on various criteria such as year, semester, category, title, and school. The application allows for interactive exploration of demographic information.

## Overview

The application reads a dataset (`transformed_data.csv`) and generates a bar graph of demographics based on user-selected filters. The data can be filtered by year, semester, category, workshop title, and school/organization. 

The application reads a dataset (`df_updated.csv`) and generates a bar graph of time based on user-selected filters. The data can be filtered by year, semester, category, workshop title, and school/organization. 

### Key Features:
- **Dynamic UI Elements**: Filters and options dynamically update based on user selections.
- **Interactive Plot**: The bar graph is rendered using Plotly for interactive visualization.

## Requirements

To run this Shiny application, you'll need:

- R (version 4.0 or higher)
- RStudio (optional, but recommended for development)
- The following R packages:
  - `shiny`
  - `rsconnect`
  - `dplyr`
  - `tidyr`
  - `ggplot2`
  - `plotly`
  - `lubridate`

You can install the required packages using the following R commands:

```r
install.packages(c("shiny", "rsconnect", "dplyr", "tidyr", "ggplot2", "plotly", "lubridate"))
