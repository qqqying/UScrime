library(shiny)
library(shinyWidgets)
library(shinythemes)
library(dslabs)
library(tidyverse)
library(ggplot2)
library(plotly)
library(urbnmapr)


load("uscrime.rda")

usstates <- urbnmapr::states
usstates$state_name <-  gsub(' ', '', usstates$state_name) %>% toupper


ui <- navbarPage("Crime in the US",
                 
                 tabPanel("Line Chart",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("crimeInput", label=strong("Crime"),
                                             choices=list("Violent Crime"=
                                                            c(`All Violent Crimes`='Violent.crime',
                                                              `Murder and Non-negligent Manslaughter`='Murder.and.nonnegligent.manslaughter',
                                                              'Rape', 'Robbery'),
                                                          "Property Crime"=
                                                            c(`All Property Crime`='Property.crime',
                                                              'Burglary', `Larceny Theft`='Larceny.theft',
                                                              `Motor Vehicle Theft`='Motor.vehicle.theft')),
                                             selected="Violent.crime"),
                              
                              selectizeInput("stateInput", label=strong("State"),
                                             choices=unique(uscrime$State),  
                                             selected="CONNECTICUT", multiple=TRUE), 
                              
                              sliderInput("yearInput", label=strong("Year"), min=1995, max=2019, 
                                          value=c(1995, 2019), sep=""),   
                              
                            ),
                            
                            mainPanel(
                              br(),
                              plotlyOutput("crimeplot", width=900, height=500),
                              br(), br(),
                              uiOutput("message")
                            )
                          )
                 ),
                 
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("crime2Input", label=strong("Crime"),
                                             choices=list("Violent Crime"=
                                                            c(`All Violent Crimes`='Violent.crime',
                                                              `Murder and Non-negligent Manslaughter`='Murder.and.nonnegligent.manslaughter',
                                                              'Rape', 'Robbery'),
                                                          "Property Crime"=
                                                            c(`All Property Crime`='Property.crime',
                                                              'Burglary', `Larceny Theft`='Larceny.theft',
                                                              `Motor Vehicle Theft`='Motor.vehicle.theft')),
                                             selected="Violent.crime"),
                              
                              
                              selectizeInput("mapyInput", label=strong("Year"),
                                             choices=1995:2019, selected=2019),
                              
                              tags$small(
                                br(), br(),
                                strong("Data Source:"), 
                                a(href="https://ucr.fbi.gov/crime-in-the-u.s",
                                  "FBI's Crime Data"))
                              
                            ),
                            
                            mainPanel(
                              br(),
                              plotOutput("crimemap", width=920, height=520)
                            )
                          )
                 )
                 
)


server <- function(input, output){
  
  ## map data
  dtmap <- reactive({
    uscrime %>% 
      right_join(usstates, by=c("State"="state_name")) %>%
      filter(Year==input$mapyInput) %>% 
      select(State, input$crime2Input, long, lat, group, state_abbv) %>%
      rename(Crime=input$crime2Input)
  }) 
  
  ## line plot data
  dta <- reactive({
    uscrime %>%
      filter(State %in% input$stateInput,
             Year>=input$yearInput[1], Year<=input$yearInput[2]) %>% 
      select(State, input$crimeInput, Year) %>% 
      rename(Crime=input$crimeInput)
  }) 
  
  ## waning message data
  warn <- reactive({
    dta() %>%
      modify_if(is.character, as.factor) %>%
      count(Year, State, .drop = FALSE) %>% as.data.frame %>% filter(n==0)
  })
  
  ## map
  output$crimemap <- renderPlot({
    
    ggplot(dtmap(), aes(long, lat, group=group, fill=Crime)) + 
      geom_polygon(color="#ffffff", size=0.25) + 
      coord_map(projection="albers", lat0=39, lat1=45) + 
      labs(fill="Rate\nper\n100,000\ninhabitants") +
      xlab("") +
      ylab("") +
      ggtitle(paste0(input$crime2Input, " Rates in ", input$mapyInput)) +
      theme_minimal()
    
  })
  
  ## line plot
  output$crimeplot <- renderPlotly({
    
    out <- ggplot(dta(), aes(x=Year, y=Crime, color=State)) +
      geom_line() + 
      geom_point() +
      xlab("Year") +
      ylab(input$crimeInput) +
      ggtitle(paste0("Crime Rates by State, ", input$yearInput[1], " to ", input$yearInput[2],
                     "<br>",
                     "<sup>", "Rate per 100,000 inhabitants", "</sup>")) +
      theme_classic()
    
    out %>% 
      ggplotly %>% 
      layout(annotations= 
               list(x=1, y=-0.11, 
                    text=paste0("Data Source: ", "<a href='https://ucr.fbi.gov/crime-in-the-u.s'>FBI's Crime Data</a>"), 
                    showarrow=F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=12, color="steelblue"))
      )
    
  })
  
  ## waning message
  output$message <- renderUI({
    if(nrow(warn())!=0)
      HTML(paste("Warning message: Missing data in ", warn()$State, " in ", warn()$Year, br()))
  })
  
  
  
  
}

shinyApp(ui=ui, server=server)


# Data Source: https://ucr.fbi.gov/crime-in-the-u.s
# rsconnect::deployApp("/Users/jessicahuang/Desktop/R/ShinyWebApp/UScrime")

