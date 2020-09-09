# Importing libraries
#install.packages("rlang")
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(xts)
library(moments)
library(forecast)
library(fpp)
library(smooth) 
library(tseries)
library(readxl)
library(MLmetrics) 
library(TSstudio)
library(TTR)
library(rlang)
library(shiny)
library(tsbox)
library(lubridate)
library(tidyverse)
library(plotly)
library(shinythemes)
#library(shinyalert)

################### Domestic Datasets #######################

#Importing for Domestic datasets
airlines_dom <- read.csv("C:/Users/Anupama Raghavendra/Desktop/ExcelR/PROJECT-P23-26-05-2020/P23_DEPLOYMENT/AirfareDomesticDataset.csv")
airlines_dom$InvoiceDate <- as.character(airlines_dom$InvoiceDate)
airlines_dom$InvoiceDate <- as.Date(airlines_dom$InvoiceDate)
airlines_dom <-airlines_dom[,c(1,2)]
#sum(is.na(airlines_dom$InvoiceDate))
#names(airlines_dom)

# Graphical Representation
#airlines_dom%>%ggplot(aes(InvoiceDate,AvgNetFare ))+geom_line(col="Red", size=0.5)+
# geom_point(alpha = 0.5,  shape=20,size=2) +
#     subtitle = "Data from April, 2018 to June, 2019") 

#boxplot(airlines_dom$AvgNetFare)

## Create a time series object
airlines_dom_seq <- seq(as.Date("2018-04-01"), as.Date("2019-06-10"), by = "day")
set.seed(25)
airlines_dom_ts <- ts(airlines_dom$AvgNetFare,     # random data
                      start = c(2018, as.numeric(format(airlines_dom_seq[1], "%j"))),
                      frequency = 365)




# Domestic Model Building
new_model_domestic<-arima(airlines_dom_ts,c(0,0,2),seasonal = list(order=c(0,0,2),period=7))

####################### INTERNATIONAL DATASET ###########################

# Importing for International datasets

airlines_inter <- read.csv("C:/Users/Anupama Raghavendra/Desktop/ExcelR/PROJECT-P23-26-05-2020/P23_DEPLOYMENT/AirfareInternatioalDataset.csv")
airlines_inter$InvoiceDate <- as.character(airlines_dom$InvoiceDate)
airlines_inter$InvoiceDate <- as.Date(airlines_dom$InvoiceDate)
airlines_inter <-airlines_inter[,c(1,2)]
#sum(is.na(airlines_inter$InvoiceDate))
#names(airlines_inter)

# Graphical Representation

#airlines_inter%>%ggplot(aes(InvoiceDate,AvgNetFare ))+geom_line(col="Red", size=0.5)+
#geom_point(alpha = 0.5,  shape=20,size=2) +
#labs(title = "NetFare Vs Time", x = "Time", y = "Netfare",
#     subtitle = "Data from April, 2018 to June, 2019") 

#boxplot(airlines_inter$AvgNetFare)

# Converting to time series 

airlines_inter_seq <- seq(as.Date("2018-04-01"), as.Date("2019-06-10"), by = "day")
set.seed(25)
airlines_inter_ts <- ts(airlines_inter$AvgNetFare,     # random data
                        start = c(2018, as.numeric(format(airlines_inter_seq[1], "%j"))),
                        frequency = 365)




# International Model building
new_model_international <-arima(airlines_inter_ts,c(0,0,2),seasonal = list(order=c(0,0,2),period=7))

# Deployment

ui<- fluidPage(
  
  theme = shinytheme("darkly"),
  #useShinyalert(),
  #themeSelector(),
  titlePanel(title= h3(tags$b("Air Fare  Prediction"), align="center", style='background-color:#4682B4;',style='font-style: italic;')),
  br(),
  br(),
  sidebarLayout(sidebarPanel(  h4("Hello! Welcome to Airlines", style = "color: Purple"),
                               
                               br(),
                               br(),
                               fluidRow( column( 12, dateInput(inputId = "date", label = "Please Select Date" ,value = "2019-06-11", min = "2019-06-11", max = "2019-07-10",
                                                               width="150px", format = "dd/mm/yyyy"))),
                               br(),
                               
                               fluidRow(column(12, radioButtons(inputId = "ItineraryType", label = "Please Select The Itinerary Type",
                                                                choices =c("Domestic", "International"), selected = "Domestic", inline = TRUE))),
                               br(),
                               
                               fluidRow( column(12,  submitButton(text="Submit",icon = NULL, width=NULL ))),
                               
                               style = "border: 1px solid coral"
                               
  ),
  #style= " border: 2px solid coral"
  
  mainPanel(  
    
    br(),
    # fluidRow(column( 12, tags$b(tags$i(verbatimTextOutput( "NetFare"  ))), style="color:coral"), style="font-size: 20px" ),
    # style= " border: 2px solid coral"),
    
    fluidRow(column(12,  verbatimTextOutput("NetFare"))),
    tags$style((HTML("#NetFare{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid coral;}"))),
    #border-radius: 4px 4px 4px 4px;
    
    
    br(),
    br(),
    br(),
    fluidRow(column(12,
                    tabsetPanel(type="tab",
                                
                                tabPanel(tags$b(tags$i("GRAPH"))  ,plotlyOutput("Plot1" )),
                                
                                tabPanel(tags$b(tags$i("DETAILS")), tags$i(tableOutput("Details") ))
                                
                    ))),
    
    style= " border: 1px solid coral"
    
    
  )
  ), style= " border: 2px solid blue"
  
  
)
server<- function(input, output, session){
  date_sel <- reactive({
    format(input$date,"%d/%m/%y" )
    
  })
  
  type_sel <- reactive({
    input$ItineraryType
  })
  
  output$date <- renderText({
    paste("You have selected : ", date_sel())
  })
  output$type <- renderText({
    paste("You have selected : ",type_sel())   
  })
  
  model_building<- reactive({
    
    if (type_sel()=="Domestic"){
      # Domestic
      #new_model_domestic <- HoltWinters(airlines_dom_ts,gamma = FALSE)
      
      forecast_value <- forecast(new_model_domestic,h=31)
      time_fare <- ts_df(forecast_value$mean)
      time_fare$Date <-   as.Date(time_fare$time)
      time_fare$Netfare <- time_fare$value
      time_fare<- time_fare[,c(3,4)]
      time_fare
      
      
    }
    else
    {
      #International
      
      #new_model_international=  arima(log(airlines_inter_ts), c(2,1,1), seasonal = list(order=c(2,1,1), period=7))
      
      
      forecast_value <- forecast(new_model_international,h=31)
      forecast_value
      time_fare <- ts_df(forecast_value$mean)
      time_fare$Date <-  as.Date(time_fare$time)
      time_fare$Netfare <- time_fare$value
      time_fare<- time_fare[,c(3,4)]
      time_fare
      
    }
    
  }) # model buildig ends here
  
  output$NetFare <- renderText({
    #print("Hi")
    time_fare <- model_building()
    netfare <- as.numeric( select(filter(time_fare, format(Date,"%d/%m/%y") == date_sel()), Netfare))
    paste("Average NetFare for", date_sel(), ":",  paste("Rs ",round(netfare,0)))
    
    
  }) 
  
  output$Plot1 <- renderPlotly({
    
    time_fare <- model_building()
    
    graph <- time_fare%>%ggplot(aes(x=Date, y= Netfare , group =1, text = paste("Date : ", format(Date,"%d/%m/%y"),
                                                                                "<br>NetFare : Rs ", round(Netfare,0)) ), height = 200,
                                width = 400) +
      geom_line(col="Blue", aes(Date, Netfare))+
      geom_point(aes(Date, Netfare),alpha = 0.5,  shape=20,size=2, col="#CC0000") +
      labs( x = "Date", y = "Netfare", face="bold") +
      theme(axis.title.x = element_text(size = 14, face = "bold.italic",margin=margin(t=0,r=0,b=0,l=0) ),
            axis.title.y = element_text(size = 14, face = "bold.italic",margin=margin(t= 0,r =20,b=0,l=0)),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            plot.background = element_rect(
              fill = "white",
              colour = "black",
              size = 1 ),
            #axis.line = element_line(color = "Black", size=2),
            panel.border = element_rect(colour = "#CC0001", fill=NA, size=1)
            
      )
    ggplotly(graph, tooltip = "text")
    
    
  })
  
  output$Details<- renderTable({
    time_fare <- model_building()
    netfare <- as.numeric( select(filter(time_fare, format(Date,"%d/%m/%y") == date_sel()), Netfare))
    netfare <- paste("Rs ", round(netfare,0))
    df <- data.frame( c("Itinerary Type", "Date","Average Net Fare"), c(type_sel(),date_sel(), netfare ))
    
    colnames(df) <- c("Parameters","Value")
    df
  })
  
}
#output$caption <- renderText( {
# paste(" Plot between Netfare and Date for 30 days (",input$type_sel() ," )")
#})
shinyApp(ui=ui,server = server)

