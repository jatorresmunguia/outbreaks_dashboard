#### Disease outbreaks 

### Packages ###
library(shiny)
library(lubridate)
library(stringi)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(sf)
library(bslib)
library(htmlwidgets)
library(shinythemes)
library(stringr)
library(reshape2)

library(shinydashboard)
library(shinyWidgets)
library(shinyjs)



library(plotly)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(sf)
library(rgdal)
library(tidyverse)
library(reshape)
library(lubridate)
library(installr)
library(DT)
library(ggiraph)
library(broom)
library(fontawesome)

### Datasets ###
# https://bootswatch.com/

# Unique DONs
# Define the GitHub raw content URL
url_api <- "https://api.github.com/repos/jatorresmunguia/disease_outbreak_news/contents/Last%20update"
last_file <- fromJSON(content(GET(url_api), as = "text"))$name[grepl(fromJSON(content(GET(url_api), as = "text"))$name, pattern = paste0("^outbreaks"))]
rdata_file <- last_file[grepl(".csv$", last_file)]
file_name <- basename(rdata_file)
outbreaks <- read.csv(paste0("https://raw.githubusercontent.com/jatorresmunguia/disease_outbreak_news/refs/heads/main/Last%20update", "/", rdata_file),
                         row.names = 1, header = TRUE)

## GIS information ##
shpsf <- read_sf("Data/world-administrative-boundaries.shp")

# https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/
# Joining diseases data with shapefile
shpsf[!is.na(shpsf$iso3) & shpsf$iso3 == "IMY", "iso3"] <- "IMN"
shpsf[!is.na(shpsf$name) & shpsf$name == "Jersey", "iso3"] <- "JEY"

knitr::opts_chunk$set(echo = TRUE)
options(prompt = "R> ", digits = 4, scipen = 999)

shpsfort <- fortify(shpsf) 

# unique levels
outbreaks$icd10n <- as.character(outbreaks$icd10n)
outbreaks$icd10n <- str_squish(outbreaks$icd10n)
outbreaks$icd10n <- as.factor(outbreaks$icd10n)

outbreaks$iso3 <- as.character(outbreaks$iso3)
outbreaks$iso3 <- str_squish(outbreaks$iso3)
outbreaks$iso3 <- as.factor(outbreaks$iso3)

outbreaks$Country <- as.character(outbreaks$Country)
outbreaks$Country <- str_squish(outbreaks$Country)
outbreaks$Country <- as.factor(outbreaks$Country)

rep_year <- length(na.omit(unique(shpsf$iso3)))
rep_country <- length(1996:2024)

# crear una base con todos los anos y todas las enfermedades para cada país
data_base <- data.frame(iso3 = rep(na.omit(unique(shpsf$iso3)), times = rep_country),
                        Year = rep(1996:2024, each = rep_year))

country_iso <- na.omit(unique(shpsf[, c("name", "iso3")]))

data_base <- data_base %>%
  left_join(country_iso, by = "iso3")

outbreaks_sub <- outbreaks %>% 
  select(iso3, Year, icd10n, DONs) %>% 
  mutate(Year = as.integer(Year)) %>%
  mutate(Ones = 1)

outbreaks104n <- outbreaks_sub %>% 
  cast(iso3 + Year ~ icd10n, value = "Ones", fun.aggregate = sum)

data_base <- data_base %>% 
  mutate(Country = name) %>%
  select(Country, iso3, Year) %>%
  left_join(outbreaks104n, by = c("iso3", "Year")) 

data_base <- data_base %>% 
  mutate_at(vars(-c(Country, iso3, Year)), ~replace(., is.na(.), 0)) %>%
  mutate(`All diseases` = rowSums(select(., -c(Country, iso3, Year)))) %>%
  pivot_longer(!c(Country, iso3, Year), names_to = "Disease", values_to = "outbreaks")

### Shiny app ###

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  useShinyjs(),
  
  theme = shinytheme("cosmo"),
  
  navbarPage(
    
    title = "A global dataset of pandemic- and epidemic-prone disease outbreaks",
    
    # Tab 1
    tabPanel(
      
      title = h4("Visualize the data", style = "font-size: 18px;"),
    
      fluidRow(
        
        # Download button with icon
        column(
          width = 12,
          style = "text-align: right;", 
          tags$a(
            href = "https://github.com/jatorresmunguia/disease_outbreak_news",
            target = "_blank",  
            download = NA,  
            style = "font-size: 36px;",  
            icon("github", class = "fa") 
          )
        )
      ),
      
      fluidRow(
        
        column(
          
          width = 3,
          
          sliderInput(
            inputId = "years_range",
            label = "Select range of years",
            min = 1996,
            max = 2024,
            value = c(1996, 2024),
            step = 1,
            sep = "",
            width = "100%",
            ticks = FALSE,
            animate = FALSE,
            dragRange = TRUE
            
          )
        ),
        
        column(
          width = 6,
          
          selectInput(
            "disease", 
            "Select a disease group (icd10n) to visualize", 
            c(levels(as.factor(data_base$Disease)))
            
          )
          
        ),
      ),  
      
      br(),
      
      fluidRow(
        
        column(
          width = 8, 
          style = "text-align: center;",
          h4(
            HTML("<strong>Number of outbreaks in the world:</strong>"),
            textOutput("map_title"),
            style = "font-size: 20px;"
          ),
          
          tags$div(
            style = "font-size: 18px; margin-top: 10px;",  
            tags$span(
              "Min: ", 
              textOutput("min_outbreaks", inline = TRUE),
              style = "color: #56B4E9; font-weight: bold;"
            ),
            tags$span(
              " - "
              ),
            tags$span(
              "Max: ", 
              textOutput("max_outbreaks", inline = TRUE),
              style = "color: #D55E00; font-weight: bold;"
            )
          )
          
        ),
        
        column(
          width = 4, 
          style = "text-align: center;",
          h4(
            HTML("<strong>Top 10 countries with the highest number of outbreaks:</strong>"),
            textOutput("bar_title"),
            style = "font-size: 20px;"
          )
        ) 
        
      ), 
      
      fluidRow(
        
        column(
          width = 8, 
          style = "text-align: center;",
          girafeOutput(
            "diseaseMap_ggplot", width = "100%", height = "400px"  
          )
        ),
        
        column(
          width = 4, 
          style = "text-align: center;",
          girafeOutput(
            "bar_plot", height = "400px"
          )
        ), 
        
      ), 

        fluidRow(
          
          tags$head(
            
            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
            
          ),
          
          # Download button with icon
          tags$a(
            
            href = "https://github.com/jatorresmunguia/disease_outbreak_news",
            target = "_blank",  
            download = NA,  
            class = "btn",
            tags$i(class = "fas fa-download"),  
            " Download data"
          ),
          
          br(),
          
          DTOutput("data_table"),
          
          br(),
          
          # Text
          htmlOutput("dataset_description")
        )
        
      ),
  )
)

## Server

server <- function(input, output, session) {

  ## Tab 1
  output$map_title <- renderText({
    
    if (input$years_range[1] == input$years_range[2]) {
      sprintf("%s\n, %d", input$disease, input$years_range[1])
    } else {
      sprintf(" %s\n, %d-%d", input$disease, input$years_range[1], input$years_range[2])
    }
    
  })
  
  output$bar_title <- renderText({
    
    if (input$years_range[1] == input$years_range[2]) {
      sprintf("%s\n, %d", input$disease, input$years_range[1])
    } else {
      sprintf("%s\n, %d-%d", input$disease, input$years_range[1], input$years_range[2])
    }
    
  })
  
  # Render the choropleth map in "Visualize the data" tabPanel
  filtered_data <- reactive({
    
    if(input$years_range[1] == input$years_range[2]) {
      # If only one year is selected, filter by that year
      data_base %>%
        filter(Year == input$years_range[1]) %>%
        filter(Disease %in% input$disease) 
      
    } else {
      # If multiple years are selected, calculate the sum by country and disease
      data_base %>%
        filter(between(Year, left = input$years_range[1], right = input$years_range[2])) %>%
        filter(Disease %in% input$disease) %>%
        group_by(Country, iso3) %>%
        summarise(outbreaks = sum(outbreaks)) 
      
    }
    
  })
  
  filtered_data_gg <- reactive({
    if (input$years_range[1] == input$years_range[2]) {
      # If only one year is selected, filter by that year
      data_base %>%
        filter(Year == input$years_range[1]) %>%
        filter(Disease %in% input$disease) %>%
        right_join(shpsf, by = "iso3") %>%
        st_as_sf()  
    } else {
      # If multiple years are selected, calculate the sum by country and disease
      data_base %>%
        filter(between(Year, left = input$years_range[1], right = input$years_range[2])) %>%
        filter(Disease %in% input$disease) %>%
        group_by(Country, iso3) %>%
        summarise(outbreaks = sum(outbreaks)) %>%
        right_join(shpsf, by = "iso3") %>%
        st_as_sf()  
    }
  })
  
  output$min_outbreaks <- renderText({
    min(filtered_data_gg()$outbreaks, na.rm = TRUE)
  })
  
  output$max_outbreaks <- renderText({
    max(filtered_data_gg()$outbreaks, na.rm = TRUE)
  })
  
  shpdatacoords <- reactive({
    coords <- st_coordinates(filtered_data_gg())
    coords <- coords[complete.cases(coords), ]  # Remove rows with missing values
    as.data.frame(coords)
  })
  
  shpdatacoords_x <- reactive({
    c(min(shpdatacoords()$X), max(shpdatacoords()$X))
  })
  
  shpdatacoords_y <- reactive({
    c(min(shpdatacoords()$Y), max(shpdatacoords()$Y))
  })
  
  output$diseaseMap_ggplot <- renderGirafe({
    
    girafe_map <- ggplot(data = filtered_data_gg()) +
      geom_sf_interactive(
        aes(
          fill = outbreaks,
          tooltip = c(paste(Country, "<br>", outbreaks, " outbreaks")),
          data_id = iso3
        )
      ) +
      coord_sf(
        xlim = shpdatacoords_x(),
        ylim = shpdatacoords_y(),
        datum = NA,
        expand = FALSE
      ) +
      scale_fill_continuous(
        low = "#56B4E9",
        high = "#D55E00",
        guide = guide_legend(
          direction = 'horizontal',
          title.position = 'top',
          title.hjust = .5,
          label.hjust = .5,
          label.position = 'bottom',
          keywidth = 3,
          keyheight = .5
        )
      ) +
      labs(fill = "outbreaks") +
      theme_base() +
      theme(legend.position = "bottom")  # Add legend to the bottom
    
    girafe(
      ggobj = girafe_map,
      options = list(
        opts_hover(css = ''),
        opts_sizing(rescale = TRUE),
        opts_hover_inv(css = "opacity:0.3;")
      ),
      height_svg = 400,
      width_svg = 1000
    )
    
  })
  
  filtered_data_top10 <- reactive({
    
    filtered_data() %>% 
      as_tibble() %>%
      arrange(desc(outbreaks)) %>%
      slice(1:10)
    
  })
  
  output$bar_plot <- renderGirafe({
    
    girafe_bar <- ggplot(
      data = filtered_data_top10(),
      aes(
        x = reorder(iso3, -outbreaks),
        y = outbreaks,
        fill = outbreaks,
        click = I(paste("highlightCountry('", iso3, "')"))
      )
    ) +
      geom_bar_interactive(
        aes(
          tooltip = c(paste(Country, "<br>", outbreaks, " outbreaks")),
          data_id = iso3
        ),
        stat = "identity",
        show.legend = FALSE
      ) +
      xlab("Country") +
      ylab("Total frequency of outbreaks") +
      scale_fill_gradient(
        low = "#D55E00", high = "#D55E00"
      ) +
      theme(panel.background = element_rect(fill='transparent'),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            line = element_line(colour = "black", 
                                lineend = "round", linetype = "solid"), 
            rect = element_rect(fill = "white",
                                colour = "black", linetype = "solid"), 
            text = element_text(colour = "black", 
                                face = "plain", family = "", size = 16, 
                                vjust = 0.5, hjust = 0.5, lineheight = 1), 
            panel.grid = element_blank(), 
            strip.background = element_rect(colour = NA), legend.key = element_rect(colour = NA), 
            title = element_text(size = rel(1)), 
            plot.title = element_text(size = rel(1.2), 
                                      face = "bold"), 
            strip.text = element_text(), 
            axis.ticks.length = unit(0.5, "lines"))
    
    girafe(ggobj = girafe_bar, options = list(
      opts_hover(css = ''),
      opts_sizing(rescale = TRUE),
      opts_hover_inv(css = "opacity:0.3;")
    ))
    
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

