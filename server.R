library(shiny)
library(RColorBrewer)

shinyServer(function(input,output,session){
        #load file and split into needed subsets
        diskfile <- read.csv(file = "disk_prices.csv", sep=",", 
                             colClasses=c("Date","factor","numeric","numeric","factor","numeric"))
        diskfile <- diskfile[complete.cases(diskfile), ]
        hdd <- diskfile[diskfile$Kind=="HDD",]
        flash <- diskfile[diskfile$Kind=="Flash Drive",]
        ssd <- diskfile[diskfile$Kind=="SSD",]
        ssdflash <- diskfile[(diskfile$Kind=="SSD" | diskfile$Kind=="Flash Drive"),]
                
        #some udeful variables
        manu <- diskfile$Manufacturer
        manuhdd <- hdd$Manufacturer
        manussd <- ssd$Manufacturer
        manuflash <- flash$Manufacturer
        manussdflash <- ssdflash$Manufacturer
        yhdd <- hdd$ppMB
        xhdd <- hdd$Date
        yflash <- flash$ppMB
        xflash <- flash$Date
        yssd <- ssd$ppMB
        xssd <- ssd$Date
        
        ##toggle options for rendering plot and buttons         
        observe({
                ###########Flash Drive data & SSD data####################
                if(input$tv == "Flash Drive & SSD data") {
                        updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL,
                                                 selected = c("Flash Drive","SSD"))
                        output$plot1 <-renderPlot({
                                palette(brewer.pal(n=12,name = "Paired"))
                                par(mar=c(5,6,4,0.5))
                                plot(log10(yflash) ~ xflash, pch=23, col=manussdflash, bg=manussdflash, 
                                     yaxt='n', xlab="Date",ylab="")
                                title(ylab="Price per 1GB in US Dollars", line=5)
                                
                                #adding SSD data
                                points(log10(yssd) ~ xssd,pch=22, col=manussdflash, bg=manussdflash, yaxt='n')
                                
                                #fancier axis
                                axis(2, at=-2:3, labels=c("$0.01","$0.1","$1","$10","$100",""),las = 1)
                                
                                #fit2
                                if (input$Flashfit) {
                                        fit2 <- lm(log10(yflash) ~ poly(xflash,3))
                                        predicted.intervals2 <- predict(fit2,data.frame(x=xflash),interval='confidence',
                                                                        level=0.99)
                                        lines(xflash,predicted.intervals2[,1],col='red',lwd=3)
                                }
                                #legend
                                if (!input$Flashfit) {
                                        legend("topright", c("Flash Drive","SSD"), pch=c(19, 22), 
                                               pt.cex=c(1.2,1.2), col=c("black","black"), 
                                               pt.bg=c("black","black"))
                                }
                                if (input$Flashfit) {
                                        legend("topright", c("Flash Drive", "SSD","Fit to Flash Drive prices"), 
                                               pch=c(19, 22,20), pt.cex=c(1.2,1.2,0.1), 
                                               col=c("black","black","red"), pt.bg=c("black","black","red"), 
                                               lty=c(0,0,1), lwd=c(0,0,3))
                                }
                              
                                legend("bottomleft", legend=levels(manussdflash), pch=19, col=unique(manussdflash))
                                
                                #grid
                                grid(nx = 19, col = "lightgray", lty = "dotted",
                                     lwd = par("lwd"), equilogs = TRUE)
                        }, height = 480, width = 700 )
                }
                
                ###########SSD data####################
                if(input$tv == "SSD data") {
                        updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL,
                                                 selected = c("SSD"))
                        output$plot1 <-renderPlot({
                                palette(brewer.pal(n=12,name = "Paired"))
                                par(mar=c(5,6,4,0.5))
                                plot(log10(yssd) ~ xssd, pch=23, col=manussd, bg=manussd, 
                                     yaxt='n', xlab="Date",ylab="",ylim=c(-1,0))
                                title(ylab="Price per 1GB in US Dollars", line=5)
                                
                                #fancier axis
                                axis(2, at=-2:1, labels=c("$0.01","$0.1","$1",""),las = 1)
                                
                                #legend
                                legend("topright", c("SSD data"), pch=c(22), pt.cex=c(1.2), col=c("black"), 
                                               pt.bg=c("black"))
                                legend("bottomleft", legend=levels(manussd), pch=19, col=unique(manussd))
                                
                                #grid
                                grid(nx = 19, col = "lightgray", lty = "dotted",
                                     lwd = par("lwd"), equilogs = TRUE)
                        }, height = 480, width = 700 )        
                }
                
                ###########Flash Drive data####################
                if(input$tv == "Flash Drive data") {
                        updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL,
                                                 selected = c("Flash Drive"))
                        
                        output$plot1 <-renderPlot({
                                palette(brewer.pal(n=12,name = "Paired"))
                                par(mar=c(5,6,4,0.5))
                                plot(log10(yflash) ~ xflash, pch=23, col=manuflash, bg=manuflash, 
                                     yaxt='n', xlab="Date",ylab="")
                                title(ylab="Price per 1GB in US Dollars", line=5)
                                
                                #fancier axis
                                axis(2, at=-2:3, labels=c("$0.01","$0.1","$1","$10","$100",""),las = 1)
                                
                                #fit2
                                if (input$Flashfit) {
                                        fit2 <- lm(log10(yflash) ~ poly(xflash,3))
                                        predicted.intervals2 <- predict(fit2,data.frame(x=xflash),interval='confidence',
                                                                        level=0.99)
                                        lines(xflash,predicted.intervals2[,1],col='red',lwd=3)
                                }
                                #legend
                                if (!input$Flashfit){
                                        legend("topright", c("Flash Drive"), pch=c(19), 
                                               pt.cex=c(1.2), col=c("black"), 
                                               pt.bg=c("black"))
                                }
                                if (input$Flashfit){
                                        legend("topright", c("Flash Drive", "Fit to Flash Drive prices"), 
                                               pch=c(19,20), pt.cex=c(1.2,0.1), 
                                               col=c("black","red"), pt.bg=c("black","red"), 
                                               lty=c(0,1), lwd=c(0,3))
                                }
                                
                                legend("bottomleft", legend=levels(manuflash), pch=19, col=unique(manuflash))
                                
                                #grid
                                grid(nx = 19, col = "lightgray", lty = "dotted",
                                     lwd = par("lwd"), equilogs = TRUE)
                        }, height = 480, width = 700 )
                }
                
                ###########HDD Data####################
                if(input$tv == "HDD data") {
                        updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL,
                                                 selected = c("HDD"))
                        
                        output$plot1 <-renderPlot({
                                palette(brewer.pal(n=12,name = "Paired"))
                                par(mar=c(5,6,4,0.5))
                                plot(log10(yhdd) ~ xhdd, pch=23, col=manuhdd, bg=manuhdd, 
                                     yaxt='n', xlab="Date",ylab="")
                                title(ylab="Price per 1GB in US Dollars", line=5)
                                
                                #fit1
                                if (input$HDDfit) {
                                        fit1 <- lm(log10(yhdd) ~ poly(xhdd,6))
                                        predicted.intervals1 <- predict(fit1,data.frame(x=xhdd),interval='confidence',
                                                                        level=0.99)
                                        lines(xhdd,predicted.intervals1[,1],col='green',lwd=3)
                                }
                                      
                                #fancier axis
                                axis(2, at=-1:8, labels=c("$0.1","$1","$10","$100","$1,000","$10,000","$100,000",
                                                          "$1000,000","$10,000,000",""),las = 1)
                                
                                #legend
                                if (input$HDDfit) {
                                        legend("topright", c("HDD","Fit to HDD prices"), pch=c(23,20), 
                                               pt.cex=c(1.2,0.1), col=c("black","green"), 
                                               pt.bg=c("black","green"), lty=c(0,1), lwd=c(0,3))
                                }
                                if (!input$HDDfit){
                                        legend("topright", c("HDD"), 
                                               pch=c(23), pt.cex=c(1.2), 
                                               col=c("black"), pt.bg=c("black"))
                                }
                                
                                legend("bottomleft", legend=levels(manuhdd), pch=19, col=unique(manuhdd))
                                
                                #grid
                                grid(nx = 19, col = "lightgray", lty = "dotted",
                                     lwd = par("lwd"), equilogs = TRUE)
                        }, height = 480, width = 700 )
                }
                
                ###########All Data####################
                if(input$tv == "See all data") {
                        updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL,
                                                 selected = c("HDD","Flash Drive","SSD"))
                        
                        output$plot1 <-renderPlot({
                                palette(brewer.pal(n=12,name = "Paired"))
                                par(mar=c(5,6,4,0.5))
                                plot(log10(yhdd) ~ xhdd, pch=23, col=manu, bg=manu, 
                                     yaxt='n', xlab="Date",ylab="")
                                title(ylab="Price per 1GB in US Dollars", line=5)
                                
                                #fit1
                                if (input$HDDfit) {
                                        fit1 <- lm(log10(yhdd) ~ poly(xhdd,6))
                                        predicted.intervals1 <- predict(fit1,data.frame(x=xhdd),interval='confidence',
                                                                        level=0.99)
                                        lines(xhdd,predicted.intervals1[,1],col='green',lwd=3)
                                }
                                #adding more data
                                points(log10(yflash) ~ xflash,pch=19, col=manu, bg=manu, yaxt='n')
                                points(log10(yssd) ~ xssd,pch=22, col=manu, bg=manu, yaxt='n')
                                
                                #fancier axis
                                axis(2, at=-1:8, labels=c("$0.1","$1","$10","$100","$1,000","$10,000","$100,000",
                                                          "$1000,000","$10,000,000",""),las = 1)
                                
                                #fit2
                                if (input$Flashfit) {
                                        fit2 <- lm(log10(yflash) ~ poly(xflash,3))
                                        predicted.intervals2 <- predict(fit2,data.frame(x=xflash),interval='confidence',
                                                                        level=0.99)
                                        lines(xflash,predicted.intervals2[,1],col='red',lwd=3)
                                }
                                #legend
                                if (input$HDDfit & !input$Flashfit){
                                        legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices"), pch=c(23, 19, 22,20), 
                                               pt.cex=c(1.2,1.2,1.2,0.1), col=c("black","black","black","green"), 
                                               pt.bg=c("black","black","black","green"), lty=c(0,0,0,1), lwd=c(0,0,0,3))
                                }
                                if (!input$HDDfit & input$Flashfit){
                                        legend("topright", c("HDD","Flash Drive", "SSD","Fit to Flash Drive prices"), 
                                               pch=c(23, 19, 22,20), pt.cex=c(1.2,1.2,1.2,0.1), 
                                               col=c("black","black","black","red"), pt.bg=c("black","black","black","red"), 
                                               lty=c(0,0,0,1), lwd=c(0,0,0,3))
                                }
                                if (!input$HDDfit & !input$Flashfit){
                                        legend("topright", c("HDD","Flash Drive", "SSD"), pch=c(23, 19, 22), 
                                               pt.cex=c(1.2,1.2,1.2), col=c("black","black","black"), 
                                               pt.bg=c("black","black","black"), lty=c(0,0,0), lwd=c(0,0,0))
                                }
                                if (input$HDDfit & input$Flashfit){        
                                        legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices", 
                                                             "Fit to Flash Drive prices"), pch=c(23, 19, 22,20,20), 
                                               pt.cex=c(1.2,1.2,1.2,0.1,0.1), col=c("black","black","black","green","red"), 
                                               pt.bg=c("black","black","black","green","red"), lty=c(0,0,0,1,1), lwd=c(0,0,0,3,3)) 
                                }
                                legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                                
                                #grid
                                grid(nx = 19, col = "lightgray", lty = "dotted",
                                     lwd = par("lwd"), equilogs = TRUE)
                        }, height = 480, width = 700 )
                }
                
                })
                
})
                