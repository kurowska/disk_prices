library(shiny)

#Create User Interface part of the application
shinyUI(fluidPage(
        titlePanel("Computer disk price history: 1955 - 2017"),
        hr(),
        sidebarLayout(
                mainPanel(
                        plotOutput('plot1')
                ),
                sidebarPanel(
                        h3("Toggle data:"),
                        h4("Drive Type:"),
                        checkboxInput("HDD","HDD", value=TRUE), 
                        checkboxInput("Flash","Flash",value=TRUE),
                        checkboxInput("SSD","SSD", value=TRUE),
                        h4("Display fit to data:"),
                        checkboxInput("HDDfit","Fit to HDD data",value=TRUE),
                        checkboxInput("Flashfit","Fit to Flash Drive data",value=TRUE),
                        checkboxInput("SSDfit","Fit to SSD Drive data",value=TRUE)
                )
        )
        
))