contextUI <- function(id) {
  tagList(
    HTML("<div style='font-size:0.9em'>"),
    htmlOutput(NS(id,"probdef1")),
    plotlyOutput(NS(id,"livemap")),
    htmlOutput(NS(id,"probdef2")),
    HTML("</div>")
  )
}



contextServer <- function(id) {
  moduleServer(id, function(input, output, session){
    renderHTMLFile <- function(filename) {
      if(file.exists(paste0("www/", filename))){
        tags$iframe(src = filename, width = "100%", height = "400px", frameborder = 0)
      } else {
        HTML(paste0("<p style='color: red;'>", filename, " not found</p>"))
      }
    }
    output$probdef1 <- renderUI(includeHTML("www/probdef1.html"))
    
    output$livemap <- renderPlotly({
      df <- read.csv("Extdata/directory_2023.csv")
      cols.to.keep <- c("UNITID", "LATITUDE", "LONGITUD", "LOCALE", "INSTNM", "CITY") # Added more columns for popup
      df_colleges <- df %>%
        dplyr::select(all_of(cols.to.keep)) 
      
      df_colleges_mainland <- df_colleges %>%
        filter(
          LONGITUD >= -125 & LONGITUD <= -66,
          LATITUDE >= 24 & LATITUDE <= 50)
      # First create a category column
      df_colleges_mainland <- df_colleges_mainland %>%
        mutate(
          LOCALE_CATEGORY = case_when(
            LOCALE >= 11 & LOCALE <= 14 ~ "City",
            LOCALE >= 21 & LOCALE <= 24 ~ "Suburb",
            LOCALE >= 31 & LOCALE <= 34 ~ "Town",
            LOCALE >= 41 & LOCALE <= 44 ~ "Rural",
            TRUE ~ "Other"
          )
        )
      
      # Define colors for each category
      color_mapping <- c(
        "City" = "#08306b",    # Darkest blue for City
        "Suburb" = "#2171b5",  # Dark blue for Suburb
        "Town" = "#6baed6",    # Medium blue for Town
        "Rural" = "#9bb9d9"    # Light blue for Rural
      )
      
      # Create plotly map
      map <- plot_geo(df_colleges_mainland, 
                      lat = ~LATITUDE, 
                      lon = ~LONGITUD) %>%  # Set specific height
        add_markers(
          color = ~LOCALE_CATEGORY,
          colors = color_mapping,
          text = ~paste("<b>", INSTNM, "</b><br>",
                        "City: ", CITY, "<br>",
                        "Locale: ", LOCALE_CATEGORY),
          hovertemplate = "%{text}<extra></extra>",
          marker = list(size = 8, opacity = 0.7)
        ) %>%
        layout(
          title = list(
            text = "US Community Colleges by Urban Density",
            x = 0.5,
            y=-0.1,
            font = list(size = 16)
          ),
          autosize = TRUE, 
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showland = TRUE,
            landcolor = 'rgb(243, 243, 243)',
            coastlinecolor = 'rgb(204, 204, 204)',
            showlakes = TRUE,
            lakecolor = 'rgb(255, 255, 255)'
          ),
          legend = list(
            title = list(text = "Urban Density"),
            x = 1,
            y = 0.02,
            xanchor = "left", 
            yanchor = "center", 
            orientation = "v"
          )
        )
      
      map
    })
    output$probdef2 <- renderUI(includeHTML("www/probdef2.html"))
    
  })
}