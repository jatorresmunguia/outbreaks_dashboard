#### Disease outbreaks 

### Packages ###
library(rfigshare)
library(shiny)
library(shinythemes)
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
library(reshape2)
library(lubridate)
library(installr)
library(httr)
library(DT)
library(ggiraph)
library(broom)
library(fontawesome)

### Datasets ###
# https://bootswatch.com/

# Unique DONs
# Define the GitHub raw content URL
ghurl <- "https://github.com/jatorresmunguia/disease_outbreak_news/raw/main/Last%20update/outbreaks_30092024.RData"

# Load the RData file directly from the URL
load(url(ghurl))

# DONs raw
# Define the GitHub raw content URL
ghurl_raw <- "https://github.com/jatorresmunguia/disease_outbreak_news/raw/main/Last%20update/dons_raw_30092024.csv"

# Load the csv file directly from the URL
DONsRaw <- read.csv(url(ghurl_raw))

## GIS information ##
shpsf <- read_sf("Data/world-administrative-boundaries.shp")
shpOGR <- fortify(readOGR("Data/world-administrative-boundaries.shp", 
                          stringsAsFactors = F))

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

outbreaks <- outbreaks %>%
  mutate(iso3 = case_when(Country == "Bonaire Sint Eustatius and Saba" ~ "BES",
                          TRUE ~ iso3))

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
      
      title = h4("About", style = "font-size: 18px;"),
      
      fluidRow(
        
        fluidRow(
          column(
            width = 3,

          ),
          
          column(
            width = 9,
            
            h3(
              HTML("<strong>'A global dataset of pandemic- and epidemic-prone disease outbreaks'</strong> by Torres Munguía, J.A.; Badarau, F.C.; Díaz Pavez, L.R.; Martínez-Zarzoso, I.; & Wacker, Konstantin M."),
              style = "color: black;"  # Highlight the text in blue
            ),
            h4("Here you can find a new dataset of infectious disease outbreaks collected from the Disease Outbreak News and the Coronavirus Dashboard produced by the World Health Organization. The dataset contains information on 70 infectious diseases and 2,227 public health events that occurred from January 1996 to March 2022 in 233 countries and territories worldwide."),
            h4("This dataset is the result of collaborative work by a team of researchers from the University of Göttingen, the University of Groningen, and the University of Bordeaux."),
            h4("The project was made possible through financial support from the ENLIGHT network, the German Academic Exchange Service (DAAD), and the Federal Ministry of Education and Research (BMBF) in Germany."),
            h4("The findings of the first version of the dataset have been published in Springer Nature’s Scientific Data. ", 
               tags$a(href = "https://www.nature.com/articles/s41597-022-01797-2", target = "_blank", "Read the paper by clicking here!")),
            h4("Additionally, the data, metadata, and the code to replicate the first version of this dataset are publicly available on Figshare. ", 
               tags$a(href = "https://figshare.com/articles/dataset/A_global_dataset_of_pandemic-_and_epidemic-prone_disease_outbreaks/17207183", target = "_blank", "You can download them by clicking here!")),
            br(),
            h3(HTML("<strong>IMPORTANT NOTE</strong>")),
            h4("From October 2024, this project is being updated by Dr. Juan Armando Torres Munguía. In case of questions, requests, or collaborations, you can contact me via",
               tags$a(href = "https://github.com/jatorresmunguia", target = "_blank", "GitHub, "),  
               tags$a(href = "https://x.com/jtorresmunguia", target = "_blank", "X, "), 
               "or", 
               tags$a(href = "https://juan-torresmunguia.netlify.app/contact/", target = "_blank", "here.")),
            h4("The last version of the dataset was updated on 30/09/2024 and contains information on 3050 outbreaks, associated with 85 infectious diseases that occurred from 01/01/1996 to 30/09/2024 in 236 countries and territories worldwide."),
            h4("The last version of the dataset can be found here: ", 
               tags$a(href = "https://github.com/jatorresmunguia/disease_outbreak_news", 
                      target = "_blank", 
                      icon("github", class = "fa")))
          )
        )
      )
    ),
    
    # Tab 2
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
      
#      br(),
      
#      fluidRow(
        
#        column(
#          width = 12, 
#          style = "text-align: left;",
#          h4(
#            htmlOutput("dons_raw_text")
#          )
#        ),
        
#        DTOutput("dons_raw_table"),
        
        # Download button with icon
#        tags$a(
          
#          href = "https://figshare.com/articles/dataset/A_global_dataset_of_pandemic-_and_epidemic-prone_disease_outbreaks/17207183",
#          target = "_blank",  
#          download = NA,  
#          class = "btn",
#          tags$i(class = "fas fa-download"),  
#          " Download data"
          
#        ),
        
#      )

    
    # Tab 3
    tabPanel(
      
      title = h4("Citation", style = "font-size: 18px;"),
      
      fluidRow(
        column(
          width = 12,
          h4("Torres Munguía, J.A., Badarau, F.C., Díaz Pavez, L.R. et al. A global dataset of pandemic- and epidemic-prone disease outbreaks. Sci Data 9, 683 (2022). ", 
             tags$a(href = "https://doi.org/10.1038/s41597-022-01797-2", target = "_blank", "https://doi.org/10.1038/s41597-022-01797-2")))
      )
    )
  )
)

## Server

server <- function(input, output, session) {
  
  total_outbreaks <- reactiveVal(nrow(outbreaks))
  total_countries <- reactiveVal(length(unique(outbreaks$iso3)))
  total_diseases <- reactiveVal(length(unique(outbreaks$icd104n)))
  
  ## Tab 2
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
  
  # Generate description of the raw dons
#  output$dons_raw_text <- renderUI({
    
#    HTML(paste(
#      "<strong>Disease Outbreak News (DONs)</strong><br>",
#      "<strong>Diseases included:</strong> ", paste(input$disease, collapse = ", "), "<br>",
#      "<strong>Country coverage:</strong> ",
#      if (is.null(input$diseaseMap_ggplot)) "worldwide" else input$diseaseMap_ggplot, "<br>",
#      "<strong>Period:</strong> ",
#      if (input$years_range[1] == input$years_range[2]) {
#        paste(input$years_range[1])
#      } else {
#        paste(input$years_range[1], "-", input$years_range[2])
#      }))
    
#  })
  
#  output$dons_raw_table <- renderDT({
    
#    datatable(
      
#      DONsRaw, 
#      options = list(
#        dom = 'Bfrtip',
        #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#        pageLength = 3,
        #lengthMenu = c(10, 25, 50),
#        responsive = TRUE,
#        columnDefs = list(
#          list(visible = TRUE, targets = c(2, 4, 3)),  # visible columns
#          list(visible = FALSE, targets = c(1, 5))  # hidden columns
          
#        )
#      )
#    )
#  })
  
  ## Tab 3

  filtered_table_1 <- reactive({
    
    if(input$years_range[1] == input$years_range[2]) {
      # If only one year is selected, filter by that year
      outbreaks %>%
        mutate(Year = as.integer(Year)) %>%
        filter(Year == input$years_range[1]) 
      
    } else {
      # If multiple years are selected, calculate the sum by country and disease
      outbreaks %>%
        mutate(Year = as.integer(Year)) %>%
        filter(between(Year, left = input$years_range[1], right = input$years_range[2])) 
      
    }
    
  })
  
  filtered_table_2 <- reactive({
    
    if(input$disease == "All diseases") {
      # If only one year is selected, filter by that year
      filtered_table_1() 
      
    } else {
      # If multiple years are selected, calculate the sum by country and disease
      filtered_table_1() %>%
        filter(icd10n %in% input$disease) 
      
    }
    
  })
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_table_2(),
      options = list(
        dom = 'Bfrtip',
        #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        #lengthMenu = c(10, 25, 50),
        responsive = TRUE,
        columnDefs = list(
          list(visible = TRUE, targets = c(1, 3, 4, 5, 7, 8, 10, 19)),  # visible columns
          list(visible = FALSE, targets = c(2, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18))  # hidden columns
        )
      )
    )
  })
  
  # Generate dataset description with total number of rows
  output$dataset_description <- renderUI({
    
    total_outbreaks_val <- total_outbreaks()
    total_countries_val <- total_countries()
    total_diseases_val <- total_diseases()
    
    HTML(paste("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px;'>
                <strong>Description:</strong><br>
                Dataset containing", total_outbreaks_val, "observations (unique disease outbreaks), occurred in a total of", total_countries_val, "countries and territories from 1996, and associated with", total_diseases_val, "specific infectious diseases (icd104n). A unique outbreak happens when a country has at least one case of a specific disease during a particular year.<br><br>
                
                <strong>Data records:</strong><br>
                <em>Country</em>: Name of the country where the outbreak occurred.<br>
                <em>iso3</em>: Alpha-3 country code from ISO 3166.<br>
                <em>Year</em>: Year of occurrence of the outbreak.<br>
                <em>icd10n</em>: Name of the type of disease according to ICD-10.<br>
                <em>icd104n</em>: Name of the disease according to ICD-10.<br>
                <em>icd10c</em>: Code of the name of the type of disease according to ICD-10.<br>
                <em>icd104c</em>: Code of the name of the disease according to ICD-10.<br>
                <em>Definition</em>: Definition of the disease according to ICD-11.
                </div>"))
    
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

