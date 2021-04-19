# Title: POSC-3410-DAL8
# Author: Curtis DeBell
# Author's Email: cdebell@clemson.edu
# Date Created: 2021-04-14

# Purpose:

# Set Up####
# Libraries
library(shiny)
library(tidyverse)

# UI####
ui <- fluidPage(
  # Title Panel 
  titlePanel("POSC 3410 Research Project"),

    # Side bar controls 
  sidebarLayout(
    sidebarPanel(
      h3("Figure 1 Controls"),
      sliderInput("years",
                  "Select years for analysis:",
                  min=min(gtd_shiny$iyear, na.rm=TRUE),
                  max=max(gtd_shiny$iyear, na.rm=TRUE),
                  value=c(min(gtd_shiny$iyear, na.rm=TRUE), max(gtd_shiny$iyear, na.rm=TRUE)),
                  sep=""),
    ),
    
    #Main panel
    mainPanel(
      h1("Analyses"),
      h3("Terrorist Attacks by Target Type over Time"),
      plotOutput("time_plot"),
      p(""),
      br(),
      h3("GTD"),
      plotOutput("gtd_plot"),
    )
  )
  
)

# Server####
server<- function(input, output, session) {
  #Figure 1 Plot - Terrorist Attacks over Time by Target Type
  output$time_plot <- renderPlot({
    #Figure 1 Plot Data Set Up
    data <- gtd_df %>% 
      filter(iyear>=input$years[1] & iyear<=input$years[2]) %>%
      group_by(iyear, targtype1_txt) %>% 
      count()
    #Figure 1 plot call 
    data %>% 
      ggplot(aes(x=iyear, y=n, fill=targtype1_txt)) +
      geom_bar(stat="identity")
  })
}  
# App####
shinyApp(ui=ui, server=server)

# Copyright (c) Grant Allard, 2021
