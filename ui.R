library(shiny)

#Create User Interface part of the application
shinyUI(fluidPage(

        tagList(
                tags$head(
                        tags$script(type="text/javascript", src = "disable.js")
                )
        ),
        titlePanel("Computer disk price history: 1956 - 2016"),
        
        hr(),
        absolutePanel(
                mainPanel(
                        fluidRow(
                                column(8, offset=2,
                                       plotOutput('plot1')
                                )
                        )
                ), top=80, left = 0, bottom=500, right =200
        ),
        absolutePanel(
        fluidRow(
                column(9,
                       radioButtons("tv",
                                    h4("Explore data by toggling buttons:"),
                                    c("See all data", "HDD data", "Flash Drive data", "SSD data", "Flash Drive & SSD data"),
                                    inline = T
                       ),
                       column(10,offset=3,
                              checkboxGroupInput("check2","",
                                          #c("HDD","Flash Drive","SSD"),
                                          #c("HDD","Flash Drive","SSD"),
                                          inline = T
                        )
                ),
                h4("Display fit to data:"),
                checkboxInput("HDDfit","Fit to HDD data",value=TRUE),
                checkboxInput("Flashfit","Fit to Flash Drive data",value=TRUE)
                )
        ), top=550, left=100, bottom=700, right=50
        )
        
))