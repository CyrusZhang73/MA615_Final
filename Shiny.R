library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)

stop_time<-read.csv("stop_time.txt")
trips<-read.csv("trips.csv")
stop<-read.csv("stops.txt")


stop<-stop%>%select(stop_name,stop_lat,stop_lon)
stop<-stop[!duplicated(stop[,c("stop_name")]),]
names(stop)[1]="checkpoint_name"
stops<-inner_join(stop,stop_time)%>%distinct()



lines<-c("Orange","Red","Green-B","Green-C","Green-D","Green-E","Mattapan")
trips<-trips%>%select(route_id,trip_id)
combine_data<-inner_join(stops,trips)
raw_data<-combine_data%>%filter(route_id %in%lines)
data<-raw_data%>%select(arrival_time,
                        departure_time,
                        checkpoint_name,
                        Season,stop_lat,stop_lon,route_id)


Season <- unique(data$Season)
Transport <- unique(data$route_id)
Time <- unique(data$arrival_time)

ui <- fluidPage(
  titlePanel("Massachusetts Bay Transportation Authority"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Transport", "Which transport do you want to take?",Transport),
      br(),
      selectInput("Time", "When do you want to leave?",Time),
      br(),
      checkboxGroupInput("Season","Which season of transportation?",Season),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Table",tableOutput("table"))
      )
    )
  )
)

server <- function(input,output){
  newdf <- reactive({
    data %>% filter(Season%in%input$Season, route_id%in%input$Transport, arrival_time%in%input$Time)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = newdf()$stop_lat, lng = newdf()$stop_lon,
                 popup= newdf()$checkpoint_name)
  })
  output$table <- renderTable({newdf()})
}

shinyApp(ui = ui, server = server)



