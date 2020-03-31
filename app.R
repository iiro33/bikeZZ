library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)

ui <- fluidPage(
  titlePanel("Helsinki citybike visualization app"),
  fluidRow(
    column(3,
           HTML("<p>Copy and paste your ride data from: <a href='https://kaupunkipyorat.hsl.fi/fi/activity'>https://kaupunkipyorat.hsl.fi/fi/activity</a></p>"),
           HTML("<p>And click analyze!</p>"),
           textAreaInput(inputId = "bike_data",
                         label = "Insert your data",
                         value = "",
                         resize = "none"),
           checkboxGroupInput(inputId = "years",
                              label = "Include years",
                              choices = c("2016", "2017", "2018", "2019", "2020"),
                              selected = c("2016", "2017", "2018", "2019", "2020")),
           submitButton(text = "Analyze")
           ),
    column(5,
           tableOutput(outputId = "summary_table")
           ),
    column(4,
           plotOutput(outputId = "trip_table")
           )
    ),
  fluidRow(
    column(3,
           plotOutput(outputId = "min_graph")
           ),
    column(3,
           plotOutput(outputId = "dist_graph")
           ),
    column(6,
           plotOutput(outputId = "corr_graph")
           )
    ),
  fluidRow(
    column(6,
           fluidRow(
             column(12,
                    plotOutput(outputId = "hour_graph", height = "350px")
             )
           ),
           fluidRow(
             column(12,
                    plotOutput(outputId = "week_graph", height = "350px")
             )
           )
    ),
    column(6,
           plotOutput(outputId = "loc_graph", height = "700px")
    )
  )
)

server <- function(input, output) {
  df <- reactive({
    lines <- unlist(strsplit(input$bike_data, "\n"))
    temp <- data.frame()
    to_add <- c()
    if (length(lines) > 10) {
      for (i in 1:length(lines)) {
        to_add <- c(to_add, lines[i])
        if (i %% 3 == 0) {
          temp <- rbind(temp, to_add, stringsAsFactors = FALSE)
          to_add <- c()
        }
      }
      colnames(temp) <- c("locations", "times", "distance")
      temp <- separate(temp, 1, c("from", "to"), sep = "-")
      temp <- separate(temp, "times", c("depart_date", "depart_time", "arrive_date", "arrive_time"), sep = " ")
      temp <- separate(temp, "distance", c("minutes", "distance"), sep = " min ")
      temp <- separate(temp, "distance", c("distance_km", "area"), sep = " km ")
      temp$depart_date <- as.Date(temp$depart_date, format = "%d.%m.%Y")
      temp$arrive_date <- as.Date(temp$arrive_date, format = "%d.%m.%Y")
      temp$minutes <- as.integer(temp$minutes)
      temp$distance_km <- as.numeric(temp$distance_km)
      temp <- filter(temp, year(depart_date) %in% input$years)
    }
    temp
  })
  
  output$min_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- ggplot(df(), aes(y = minutes)) +
        geom_boxplot() +
        theme_tufte() +
        theme(axis.text.x = element_blank()) +
        labs(title = "Minutes per ride",
             y = "Minutes") +
        scale_y_continuous(breaks = seq(0, 30, 5),
                           labels = seq(0, 30, 5))
      print(p)
    }
  })
  
  output$dist_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- ggplot(df(), aes(y = distance_km)) +
        geom_boxplot() +
        theme_tufte() +
        theme(axis.text.x = element_blank()) +
        labs(title = "Kilometers per ride",
             y = "Kilometers")
      print(p)
    }
  })
  
  output$summary_table <- renderTable({
    if (nrow(df()) > 0) {
      p <- df() %>% 
        group_by(area) %>% 
        summarize(`Total rides` = n(),
                  `Total minutes` = sum(minutes),
                  `Total kilometers` = sum(distance_km))
      print(p)
    }
  })
  
  output$trip_table <- renderPlot({
    if (nrow(df()) > 0) {
      p <- df() %>% 
        group_by(year = as.integer(year(depart_date))) %>% 
        count(year, name = "trips") %>% 
        ggplot(aes(x = year, y = trips)) +
        geom_bar(stat = "identity") +
        theme_tufte() +
        labs(title = paste0("Rides by year"),
             y = "Rides",
             x = "Year")
      print(p)
    }
  })
  
  output$corr_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- ggplot(df(), aes(x = minutes, y = distance_km)) +
        geom_point() +
        geom_smooth(method = "lm") +
        theme_tufte() +
        labs(title = "Minutes vs kilometers",
             x = "Minutes",
             y = "Kilometers")
      print(p)
    }
  })
  
  output$loc_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- df() %>% 
        count(from, sort = TRUE, name = "from_n") %>% 
        head(10) %>% 
        full_join(df() %>% count(to, sort = TRUE, name = "to_n") %>% head(10), by = c("from" = "to")) %>% 
        rename(location = from) %>% 
        mutate(from_n = replace_na(from_n, 0),
               to_n = replace_na(to_n, 0)) %>% 
        pivot_longer(-location, names_to = "direction", values_to = "value") %>% 
        ggplot(aes(x = reorder(location, value, sum), y = value, fill = direction)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() + 
        theme_tufte() +
        labs(title = "Top10 most popular departure and destination locations",
             x = "Location",
             y = "Rides",
             fill = "From or to")
      print(p)
    }
  })
  
  output$hour_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- df() %>% 
        separate(depart_time, c("hour", "minute"), sep = ":", convert = TRUE) %>% 
        count(hour) %>% 
        ggplot(aes(x = hour, y = n)) +
        geom_bar(stat = "identity") +
        scale_x_continuous(breaks = seq(0, 23, 1),
                           labels = seq(0, 23, 1)) +
        theme_tufte() +
        labs(title = "Rides per hour of day",
             x = "Hour",
             y = "Rides")
      print(p)
    }
  })
  
  output$week_graph <- renderPlot({
    if (nrow(df()) > 0) {
      p <- df() %>% 
        group_by(week_day = wday(depart_date, week_start = 1, abbr = TRUE, label = TRUE)) %>% 
        summarize(n = n()) %>% 
        ggplot(aes(x = week_day, y = n)) +
        geom_bar(stat = "identity") +
        theme_tufte() +
        labs(title = "Rides per day of week",
             x = "Weekday",
             y = "Rides")
      print(p)
    }
  })
  
}
shinyApp(ui = ui, server = server)
