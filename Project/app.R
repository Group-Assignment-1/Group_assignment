library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape)
library(reshape2)
library(patchwork)
library(DT)

ui<- dashboardPage(
  dashboardHeader(
    title="Crime Analytics"
  ),
  dashboardSidebar(
    selectInput(inputId="Crime",label="Select:",
                choices = c("CRIMES_AGAINST_THE _PERSON","CONTACT_RELATED_CRIMES",
                            "PROPERTY_RELATED_CRIMES","OTHER_SERIOUS_CRIMES",
                            "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION",
                            "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")),
    sliderInput("slider","Select A Number:",min=100,max=500,value=100)
  ),
  dashboardBody(
    column(width=7,
           plotOutput("crime_plot2",height=350)),
    column(width=5,
           plotOutput("crime_plot1",height=350)),
    DT::dataTableOutput("crime_table")
    )
  )


server<-function(input,output){
  
  df<-read.csv("crime_data.csv")
  class<-df$classes
  df2<-df
  df2$X=NULL
  df_melt<-melt(df,id.vars="classes",measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009",
                                    "April_2009_to_March_2010","April_2010_to_March_2011",
                                    "April_2011_to_March_2012","April_2012_to_March_2013",
                                    "April_2013_to_March_2014","April_2014_to_March_2015",
                                    "April_2015_to_March_2016"))
  
  df_melt$value[which(is.na(df_melt$value))]<-0
  
  df_melt_ave<-aggregate(value~variable+classes,df_melt,sum)
  df_melt_ave2<-aggregate(value~variable,df_melt,sum)
  
  data_crime2<-reactive({
    d2<-df_melt_ave2 %>% filter(value>input$slider)
    return(d2)
  })
  
  data_crime<-reactive({
    d<-df_melt_ave %>% filter(classes==input$Crime)
    return(d)
  })
  table1<-reactive({
    d3<-df2 %>% filter(classes==input$Crime)
    return(d3)
  })
  
  output$crime_plot2<-renderPlot({
    data_crime() %>% 
      ggplot(aes(y=reorder(variable,value),x=value,fill=variable))+
      geom_col()+
      labs(x="number of cases per year",y="",title="Crime statistics per class")
  })
  
  output$crime_plot1<-renderPlot({
    data_crime2() %>%
      ggplot(mapping=aes(y=reorder(variable,value),x=value,fill=variable))+
      geom_col()+
      labs(x="Number of cases per Year",y="",title="Total Crimes Cases")
  })
  
  output$crime_table=DT::renderDataTable({
    table1()
  })
}
shinyApp(ui = ui, server = server)
