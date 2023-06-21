library(shinydashboard)
library(shiny)
library(leaflet)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)

# setwd("/app/visualization")

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
last_updated_df <- read_csv("../data/processed_data/tweets_cercanias_madrid.csv")
last_updated <- last_updated_df %>% 
  select(Datetime) %>% 
  tail(n = 1) %>% 
  pull()

random_colors <- c(
  "red", "yellow", "aqua", "blue", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black"
)


cercanias_colors <- c(
  "C1" = "aqua",
  "C2" = "aqua",
  "C3" = "purple",
  "C4" = "navy",
  "C5" = "orange",
  "C7" = "red",
  "C8" = "grey",
  "C9" = "orange",
  "C10" = "green"
)


cercanias_original_colors <- c(
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
  skin = "black", 
  dashboardHeader(title = "Cercanías Issues"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", icon = icon("th"), tabName = "map",
               badgeLabel = "available soon!", badgeColor = "yellow"),
      menuItem("Methodology & Credits", tabName ="meth", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h3(strong("Explore Cercanías Madrid issues by line"), align = "center"),
              h5(paste0("Last updated: ", as.Date(last_updated + days(1), 
                                                  format = "%Y %m %D") ), align = "right"),
              h5(em("Note: 2023 data is not comparable to the rest of the years", br(),
                    "as a noted in the methodology (sidebar menu)"), align = "right"),
              h4("Select a year to obtain ", br(), "detailed information about the",
                 br(), "number of issues and more stats!"),
              fluidRow(
                box(
                  width = 2,
                  align = "center",
                  selectInput("year", "Select Year", choices = unique(data$Year), selected = year(Sys.Date()))
                ),
                # fluidRow(
                #   
                # ),
              ),
              fluidRow(
                infoBoxOutput("affectedLine"),
                infoBoxOutput("affectedMonth"),
                infoBoxOutput("affectedDay")
              ),
              fluidRow(
                box(
                  width = 2,
                  align = "center",
                  uiOutput("dateInput")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  title = "Number of incidences",
                  status = "primary",
                  tableOutput("result")
                ),
                box(
                  width = 4,
                  title = "Detailed information and source",
                  status = "primary",
                  solidHeader = F,
                  div(
                    style = "overflow-y: scroll; max-height: 300px;",
                    tableOutput("detailed_table")
                  )
                ),
                box(
                  width = 4,
                  # title = "where",
                  status = "primary",
                  plotlyOutput("myPlot")
                )
              ),
              h2("Historical data", align = "center"),
              fluidRow(
                infoBoxOutput("meanYear"),
                infoBoxOutput("meanMonth"),
                infoBoxOutput("meanDay")
              ),
              h3("Tables", align = "center"),
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Historical data: incidences per line and year",
                    width = NULL,
                    solidHeader = T,
                    status = "success",
                    collapsible = T,
                    div(
                      style = "overflow-y: scroll; overflow-x: auto;",
                      tableOutput("aggregatedTable")
                    )
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Historical data: Incidences per month",
                    width = NULL,
                    status = "success",
                    collapsible = T,
                    solidHeader = TRUE,
                    div(
                      style = "overflow-y: scroll; overflow-x: auto;",
                      tableOutput("aggregatedMonth")
                    )
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Historical data: Incidences per year and line",
                    width = NULL,
                    status = "success",
                    collapsible = T,
                    solidHeader = TRUE,
                    tableOutput("aggregatedYear")
                  ),
                )
              ),
              h4("Dashboard and data harvesting and cleaning done by", 
                 a("Mario Yanes", href = "https://twitter.com/myanesp"), 
                 align = "left"),
              h4("This work was conducted as the final thesis for the", a("Master
                 Degree in Computational Social Sciences (UC3M)", 
                href = "https://www.uc3m.es/master/computational-social-science"), br(),
                 "under the supervision of prof.", align = "left"),
              h4("You can find the source code hosted on ", a("GitHub", 
                                                       href = "https://github.com/myanesp/cercanias-madrid-tfm"),
                 align = "left", icon("github", lib = "font-awesome"))
      ),
      tabItem(tabName = "map",
              h2("Map"),
              fluidRow(
                leafletOutput("map")
              ),
      ),
      tabItem(tabName = "meth",
              h2("Methodology"),
              fluidRow(
                box(
                  p("The data showed in the main dashboard is obtained automatically 
                       every morning at 3am from the @CercaniasMadrid Twitter account.
                    Every single script used for this project is publicly available at",
                    a("the GitHub repository", href = "https://github.com/myanesp/cercanias-issues-tfm"),
                    "and you can review it and see how it works."),
                  br(),
                  p(strong("How does it work?"), br(), "A Python script downloads all the tweets published by the account, 
                    and scrap from a local",
                    a("Nitter instance", href = "https://github.com/zedeus/nitter"),
                    "(a frontend for Twitter) the original tweet if the tweet by @CercaniasMadrid is 
                    a reply to another user."),
                  p("Then, a R scripts process and clean the data, to prepare for
                  this visualisation."),
                  p("Take into account that the data for 2023 is more exhaustive
                    than the rest. This is because the data for 2015 to 2022 
                    (inclusive) was download using the API before Twitter removed
                    the free tier, and the tweets obtained was only those with
                    hashtags. From 2023, all the tweets has been downloaded
                    with Python library", a("snscsrape", 
                                            href = "https://github.com/JusAnotherArchivist/snscrape"),
                    " and now also counts issues if any line is mentioned 
                    on the original tweet to which 
                    the Cercanías account is replying to."),
                  p(strong("How are the issues counted?"), br(), "The script analyses the tweet
                    for each day, extract from the tweets the line/s and count as an issue.", br(),
                    br(),
                    "To avoid inflating the data, because the account may tweet 
                    another time about the same issue, or answer a user
                    for an delay that has already been posted into the main profile,
                    the script checks the timestamp of the tweets and only store,
                    for the same line, one issue each 6 hours. This threshold was a
                    discretional measure, balancing between regular delays and 
                    serious incidents"), br(),
                  p("As a final note, «child» lines, like C4a or C3b are counted
                    as part of its «parent» line, in this case, C4 an C3. This 
                    decision was made because of the low number of appearances
                    that breaks the tables and plots, and, at the end, they are
                    subdivision of a same."),
                  status = "primary"
                ),
              ),
              h2("Replication"),
              fluidRow(
                box(
                  p("All the code is open sourced and it is available in",
                    a("the GitHub repository", href = "https://github.com/myanesp/cercanias-issues-tfm"),
                    "and the only thing you need for replicate this exactly dashboard
      with the same data is having a machine able to run", 
                    a("Docker.", href = "https://docs.docker.com/engine/"),
                    "Further instructions are in the README.md of the repository.
      The Docker containers you will run are two for hosting Nitter
      to scrap the original tweets and one,", 
                    a("built on top of a custom Docker image", href = "https://github.com/myanesp/baseimage-pyr"),
                    "that contains all the libraries and packages needed without
                    messing with your system dependencies, for harvesting and visualising the data."),
                  status = "success"
                )
              ),
              h3("Acknowledgments"),
              fluidRow(
                box(
                  p("All the project is built on top of open source software.
                  The scripts are programmed in Python and R, and the framework
                  that powers this dashboard is Shiny, another library for R.",
                    br(),
                    "")
                )
              ),
              h5("Dashboard and data harvesting and cleaning done by", 
                 a("Mario Yanes", href = "https://twitter.com/myanesp"), 
                 align = "left"),
              h5("This work was conducted as the final thesis for the", a("Master
                 Degree in Computational Social Sciences (UC3M)",
                 href = "https://www.uc3m.es/master/computational-social-science"), br(),
                 "under the supervision of prof. María Medina",
                 align = "left"),
              h5("You can find the source code hosted on ", a("GitHub", 
                                                              href = "https://github.com/myanesp/cercanias-madrid-tfm"),
                 align = "left", icon("github", lib = "font-awesome"))
      )
    )
  )
)

server <- function(input, output) {
  
  selected_year <- reactive({
    selected_year <- input$year
    
    selected_year
  })
    
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
      "Select day:",
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
      mutate(Date = as.Date(Datetime)) %>% 
      group_by(Lines, Date) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Lines) %>% 
      count() %>%
      ungroup() %>% 
      arrange(desc(n)) %>%
      slice(1) 
  })

  
  most_affected_day <- reactive({
    result <- data %>%
      filter(Year == input$year) %>%
      mutate(Date = as.Date(Datetime)) %>% 
      group_by(Lines, Date) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
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
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
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
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
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
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>%
      group_by(Lines, Date) %>% 
      summarise(n = n()) %>%
      plot_ly(x = ~Date, y = ~n, color = ~Lines, colors = cercanias_original_colors, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Number of issues"),
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
      data.frame("No issues were found for selected day.") %>%
        setNames("Oops!")
    } else {
      incidencias
    }
  })
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })

  output$affectedLine <- renderValueBox({
    
    m_line <- most_affected_line()
    if (m_line$Lines == "C1") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines ]
      )
    } else if (m_line$Lines == "C2") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines ]
      )
    } else if (m_line$Lines == "C3") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C4") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C5") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C7") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C8") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C9") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    } else if (m_line$Lines == "C10") {
      infoBox(
        paste0("Most affected line in ", input$year),
        paste0("Line ", m_line$Lines),
        paste0("Issues: ", m_line$n),
        icon = icon("train", lib = "font-awesome"),
        color = cercanias_colors[m_line$Lines]
      )
    }

  })
  
  output$affectedMonth <- renderInfoBox({
    most_affected_m <- most_issues_month()
    infoBox(
      paste0("Month with more issues in ", input$year), most_affected_m$Month, paste0("Issues: ", most_affected_m$n), icon = icon("calendar", lib = "glyphicon"),
      color = sample(random_colors, 1)
    )
  })
  stations <- read_csv("../data/madrid-cercanias-stations.csv")
  
  output$affectedDay <- renderInfoBox({
    most_affected <- most_affected_day()
    infoBox(
      paste0("Day with more issues in ", input$year), most_affected$Date, paste0("Issues: ", most_affected$n), icon = icon("fire", lib = "font-awesome"),
      color = sample(random_colors, 1)
    )
  })
  
  output$aggregatedTable <- renderTable({
    total <- data %>%
      mutate(Date = as.Date(Datetime)) %>%
      group_by(Lines) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>%
      group_by(Lines, Year) %>% 
      count() %>%
      pivot_wider(names_from = Year, values_from = n) %>% 
      replace_na(list(`2015` = 0, `2016` = 0, `2017` = 0, `2018` = 0, 
                      `2019` = 0, `2020` = 0, `2021` = 0, `2022` = 0, 
                      `2023` = 0)) %>% 
      select(Lines, `2023`, `2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`) %>% 
      mutate(Total = rowSums(across(`2023`:`2015`))) %>% 
      select(Lines, Total, everything()) %>% 
      mutate(Total = as.integer(Total)) %>% 
      arrange(desc(Total))
  })
  
  output$aggregatedYear <- renderTable({
    y <- data %>%
      mutate(Date = as.Date(Datetime)) %>% 
      group_by(Lines, Date) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Lines, Year) %>% 
      count() %>%
      pivot_wider(names_from = Lines, values_from = n) %>% 
      replace_na(list(`C1` = 0, `C2` = 0, `C3` = 0, `C4` = 0, 
                      `C5` = 0, `C7` = 0, `C8` = 0, `C9` = 0, 
                      `C10` = 0)) %>% 
      select(Year, `C1`, `C2`, `C3`, `C5`, `C7`, `C8`, `C9`, `C10`) %>% 
      mutate(Total = rowSums(across(C1:C10))) %>% 
      select(Year, Total, everything()) %>% 
      mutate(Total = as.integer(Total)) %>% 
      arrange(desc(Total))
    y$Year <- as.integer(y$Year)
    y
  })
  

  
  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")
  
  output$aggregatedMonth <- renderTable({
    agg_month <- data %>%
      mutate(Date = as.Date(Datetime)) %>% 
      unique() %>% 
      group_by(Lines, Date) %>% 
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>% 
      ungroup() %>% 
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
      group_by(Lines, Month) %>% 
      count() %>%
      pivot_wider(names_from = Month, values_from = n) %>% 
      replace_na(list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, 
                      `5` = 0, `6` = 0, `7` = 0, `8` = 0, 
                      `9` = 0, `10` = 0, `11` = 0, `12` = 0)) %>% 
      select(Lines, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`) %>% 
      mutate(Total = rowSums(across(`1`:`12`))) %>% 
      rename_with(~ month_names, .cols = `1`:`12`) %>% 
      select(Lines, Total, everything()) %>% 
      arrange(desc(Total)) %>% 
      slice(1)
    agg_month$Total <- as.integer(agg_month$Total)
    agg_month
  })
  
  total_years <- data %>%
      mutate(Date = as.Date(Datetime)) %>%
      unique() %>%
      group_by(Lines, Date) %>%
      mutate(Rank = dense_rank(Datetime),
             Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
             Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
      distinct(Lines, Group_ID, .keep_all = TRUE) %>%
      ungroup() %>%
      distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>%
      group_by(Year) %>%
      count() %>%
      ungroup() %>%
      summarise(sum(n)) %>%
      pull()

  mean_by_year <- total_years / length(unique(data$Year))

  output$meanYear <- renderInfoBox({
    infoBox(
      "Mean of issues by year", trunc(mean_by_year, 2), icon = icon("calendar", lib = "glyphicon"),
      color = sample(random_colors, 1)
    )
  })
 
  total_day <- data %>%
    mutate(Date = as.Date(Datetime)) %>%
    unique() %>%
    group_by(Lines, Date) %>%
    mutate(Rank = dense_rank(Datetime),
           Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
           Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
    distinct(Lines, Group_ID, .keep_all = TRUE) %>%
    ungroup() %>%
    distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>% 
    count()
  
  mean_by_day <- total_day / trunc(as.numeric(difftime(max(data$Datetime), min(data$Datetime), 
                                      units = "days")), 0)
  

  output$meanDay <- renderInfoBox({
    infoBox(
      "Mean of issues by day", round(mean_by_day$n, 2), icon = icon("calendar", lib = "glyphicon"),
      color = sample(random_colors, 1)
    )
  })
  
  total_month <- data %>%
    mutate(Date = as.Date(Datetime)) %>%
    unique() %>%
    group_by(Lines, Date) %>%
    mutate(Rank = dense_rank(Datetime),
           Time_Diff_Prev = difftime(Datetime, lag(Datetime), units = "hours"),
           Group_ID = cumsum(Time_Diff_Prev > 6 | is.na(Time_Diff_Prev))) %>%
    distinct(Lines, Group_ID, .keep_all = TRUE) %>%
    ungroup() %>%
    distinct(Tweet_Id, Datetime, Lines, .keep_all = TRUE) %>%
    group_by(Month) %>%
    count() %>%
    mutate(mean = n / length(unique(data$Year)))
  
  mean_by_month <- sum(total_month$mean) / 12
  
  output$meanMonth <- renderInfoBox({
    infoBox(
      "Mean of issues by month", round(mean_by_month, 2), icon = icon("calendar", lib = "glyphicon"),
      color = sample(random_colors, 1)
    )
  })

  
  output$detailed_table <- renderTable({
    incidencias <- select_date()
    if (nrow(incidencias) > 0) {
      filtered_data <- data %>%
        filter(as.Date(Datetime) == input$date) %>%
        select(Text, Lines, Tweet_Id, Datetime) %>%
        mutate(Link = paste0("<a href='https://twitter.com/CercaniasMadrid/status/", Tweet_Id, "'>Link to the tweet</a>")) %>%
        mutate(Hour = format(Datetime, "%H:%M")) %>%
        select(Hour, Text, Lines, Link)

      filtered_data
    }
  }, sanitize.text.function = function(x) {
    if (is.character(x)) {
      x
    } else {
      htmltools::HTML(as.character(x))
    }
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(
        data = stations, 
        lng = ~LONGITUD,
        lat = ~LATITUD,  
        color = "blue" 
      )
  })
}

runApp(list(ui = ui, server = server), port = 3838, host = "0.0.0.0")


