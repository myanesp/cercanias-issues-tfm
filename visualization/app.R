library(shinydashboard)
library(shiny)
library(leaflet)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)

setwd("/app/visualization")

data <- read_csv("../data/final_data/cercanias_madrid_issues.csv", col_types = cols(Tweet_Id = col_character())) %>%
  select(2:9) %>% 
  mutate(Year = year(Datetime),
         Month = month(Datetime),
         Day = day(Datetime),
         Lines = toupper(Lines)) %>% 
  mutate(Lines = case_when(
    str_detect(Lines, "A$") ~ str_replace(Lines, "A$", ""),
    str_detect(Lines, "B$") ~ str_replace(Lines, "B$", ""),
    TRUE ~ Lines
  )) 

last_data_date <- max(data$Datetime)



cercanias_colors <- c(
  "C1" = "#56a8e2",
  "C2" = "#66CCFF",
  "C3" = "#88009a",
  "C4" = "#002d9b",
  "C5" = "#fbb800",
  "C7" = "#d9001d",
  "C8" = "#6d6e71",
  "C9" = "#f95900",
  "C10" = "#90bf00"
)


ui <- dashboardPage(
  dashboardHeader(),
  # dashboardPage(skin = "black"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", icon = icon("th"), tabName = "map",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Methodology & Credits", tabName ="meth", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content"),
              
              fluidRow(
                box(
                  width = 2,
                  selectInput("year", "Select Year", choices = unique(data$Year), selected = year(Sys.Date()))
                )
              ),
              fluidRow(
                infoBoxOutput("affectedLine"),
                infoBoxOutput("affectedMonth"),
                infoBoxOutput("affectedDay")
              ),
              fluidRow(
                box(
                  width = 2,
                  uiOutput("dateInput")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  title = "Box title",
                  status = "primary",
                  tableOutput("result")
                ),
                box(
                  width = 4,
                  title = "Source",
                  status = "primary",
                  solidHeader = T,
                  div(
                    style = "overflow-y: scroll; max-height: 300px;",
                    tableOutput("detailed_table")
                  )
                ),
                box(
                  width = 4,
                  title = "where",
                  status = "warning",
                  plotlyOutput("myPlot")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  box(
                    width = NULL,
                    background = "black",
                    "A box with a solid black background"
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Title 3",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "warning",
                    "Box content"
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Title 2",
                    width = NULL,
                    solidHeader = TRUE,
                    "Box content"
                  ),
                  box(
                    title = "Title 6",
                    width = NULL,
                    background = "maroon",
                    "A box with a solid maroon background"
                  )
                )
              )
      ),
      tabItem(tabName = "map",
              h2("Map")
      ),
      tabItem(tabName = "meth",
              h2("Credits"))
    )
  )
)

server <- function(input, output) {
  
  date_range <- reactive({
    min_date <- as.Date(paste0(input$year, "-01-01"))
    max_date <- as.Date(paste0(input$year, "-12-31"))
    if (input$year == 2023) {
      max_date <- last_data_date
    }
    list(min_date = min_date, max_date = max_date)
  })
  
  output$dateInput <- renderUI({
    date_range_values <- date_range()
    dateInput(
      "date",
      "Selecciona el día:",
      value = if (input$year == 2023){ date_range_values$max_date } else date_range_values$min_date,
      min = date_range_values$min_date,
      max = date_range_values$max_date,
      format = "dd-mm-yyyy",
      startview = "month",
      weekstart = 1
    )
  })

  most_affected_line <- reactive({
    data %>%
      filter(Year == input$year) %>%
      group_by(Lines) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 8 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      group_by(Lines) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      select(1) %>%
      slice(1) %>%
      pull()
  })
  
  most_affected_day <- reactive({
    result <- data %>%
      filter(Year == input$year) %>%
      mutate(Date = as.Date(Datetime)) %>% 
      group_by(Lines, Date) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 8 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Date) %>% 
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1)
    
    result
  })
  
  most_issues_month <- reactive({
    result <- data %>%
      filter(Year == input$year) %>%
      mutate(Date = as.Date(Datetime)) %>% 
      unique() %>% 
      group_by(Lines, Date) %>% 
      # group_by(Lines) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 8 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Month) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      mutate(Month = case_when(
        Month == 1 ~ "January",
        Month == 2 ~ "February",
        Month == 3 ~ "March",
        Month == 4 ~ "April",
        Month == 5 ~ "May",
        Month == 6 ~ "June",
        Month == 7 ~ "July",
        Month == 8 ~ "August",
        Month == 9 ~ "September",
        Month == 10 ~ "October",
        Month == 11 ~ "November",
        Month == 12 ~ "December",
        TRUE ~ "Invalid month"
      )) %>% 
      slice(1)
    
    result
  })

  select_date <- reactive({
    data %>%
      mutate(Date = as.Date(Datetime)) %>%
      filter(Date == input$date) %>%
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Lines, Date) %>%
      ungroup() %>% 
      group_by(Lines) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 8 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      count() %>%
      rename(Issues = n) %>% 
      arrange(desc(Issues))
  })

  output$myPlot <- renderPlotly({
    data %>%
      mutate(Date = format(data$Datetime, "%Y-%m")) %>%
      filter(Year == input$year) %>%
      group_by(Lines, Date) %>%
      summarise(n = n()) %>%
      plot_ly(x = ~Date, y = ~n, color = ~Lines, colors = cercanias_colors, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Observation Count"),
             title = paste0("Issues in lines in ", input$year),
             hovermode = "closest")
  })

  issues_current_month <- data %>%
    mutate(Month = sprintf("%02d", data$Month)) %>%
    filter(Year == format(Sys.Date(), "%Y") &
             Month == format(Sys.Date(), "%m")) %>%
    nrow()




  output$result <- renderTable({
    incidencias <- select_date()
    if (nrow(incidencias) == 0) {
      # No incidents found for the selected day
      data.frame("No se encontraron incidencias para el día seleccionado.") %>%
        setNames("¡Oops!")
    } else {
      incidencias
    }
  })

  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("train"),
      color = "purple"
    )
  })

  output$affectedLine <- renderValueBox({
    most_affected_line_text <- most_affected_line()
    color <- cercanias_colors[most_affected_line_text]
    
    infoBox(
      "Most affected line", paste0("Line ", most_affected_line()),  input$year, "Most affected line", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$affectedMonth <- renderInfoBox({
    most_affected_m <- most_issues_month()
    infoBox(
      "Month with more issues", most_affected_m$Month, paste0("Issues: ", most_affected_m$n), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$affectedDay <- renderInfoBox({
    most_affected <- most_affected_day()
    infoBox(
      "Day with more issues", most_affected$Date, paste0("Issues: ", most_affected$n), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  output$detailed_table <- renderTable({
    incidencias <- select_date()
    if (nrow(incidencias) > 0) {
      filtered_data <- data %>%
        filter(as.Date(Datetime) == input$date) %>%
        select(Text, Lines, Tweet_Id, Datetime) %>%
        mutate(`Enlace original` = paste0("<a href='https://twitter.com/CercaniasMadrid/status/", Tweet_Id, "'>Link al tweet</a>")) %>%
        rename("Enlace original" = `Enlace original`) %>%
        rename(`Líneas` = Lines) %>%
        mutate(Hora = format(Datetime, "%H:%M")) %>%
        select(Hora, Text, `Líneas`, "Enlace original")

      filtered_data
    }
  }, sanitize.text.function = function(x) {
    if (is.character(x)) {
      x
    } else {
      htmltools::HTML(as.character(x))
    }
  })
}

runApp(list(ui = ui, server = server), port = 3838, host = "0.0.0.0")



 # stations <- read_csv("../data/madrid-cercanias-stations.csv")
# 
# stations$LATITUD <- gsub(",", ".", stations$LATITUD)
# stations$LONGITUD <- gsub(",", ".", stations$LONGITUD)
# 
# stations$LATITUD <- as.numeric(stations$LATITUD)
# stations$LONGITUD <- as.numeric(stations$LONGITUD)

