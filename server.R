library(shiny)
library(RColorBrewer)

shinyServer(function(input,output){
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
 
        output$plot1 <-renderPlot({
                palette(brewer.pal(n=12,name = "Paired"))
                par(mar=c(5,6,0,0.5))
                ########### all data ###############
                if (input$HDD & input$Flash & input$SSD) { 
                        palette(brewer.pal(n=12,name = "Paired"))
                        
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
                        #fit3
                        if (input$SSDfit) {
                                fit3 <- lm(log10(yssd) ~ poly(xssd,2))
                                predicted.intervals3 <- predict(fit3,data.frame(x=xssd),interval='confidence',
                                                                level=0.99)
                                lines(xssd,predicted.intervals3[,1],col='blue',lwd=3)
                        }
                        #legend
                        if (!input$HDDfit & !input$Flashfit & !input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD"), 
                                       pch=c(23, 19, 22), 
                                       pt.cex=c(1.2,1.2,1.2), 
                                       col=c("black","black","black"), 
                                       pt.bg=c("black","black","black"))
                        }
                        if (!input$HDDfit & !input$Flashfit & input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to SSD prices"), 
                                       pch=c(23, 19, 22,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1), 
                                       col=c("black","black","black","blue"), 
                                       pt.bg=c("black","black","black","blue"), 
                                       lty=c(0,0,0,1), lwd=c(0,0,0,3)) 
                        }
                        if (!input$HDDfit & input$Flashfit & !input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to Flash Drive prices"), 
                                       pch=c(23, 19, 22,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1), 
                                       col=c("black","black","black","red"), 
                                       pt.bg=c("black","black","black","red"), 
                                       lty=c(0,0,0,1), lwd=c(0,0,0,3)) 
                        }
                        if (!input$HDDfit & input$Flashfit & input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD", 
                                                     "Fit to Flash Drive prices", "Fit to SSD prices"), 
                                       pch=c(23, 19, 22,20,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1,0.1), 
                                       col=c("black","black","black","red","blue"), 
                                       pt.bg=c("black","black","black","red","blue"), 
                                       lty=c(0,0,0,1,1), lwd=c(0,0,0,3,3)) 
                        }
                        if (input$HDDfit & !input$Flashfit & !input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices"), 
                                       pch=c(23, 19, 22,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1), 
                                       col=c("black","black","black","green"), 
                                       pt.bg=c("black","black","black","green"), 
                                       lty=c(0,0,0,1), lwd=c(0,0,0,3)) 
                        }
                        if (input$HDDfit & !input$Flashfit & input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices", 
                                                     "Fit to SSD prices"), 
                                       pch=c(23, 19, 22,20,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1), 
                                       col=c("black","black","black","green","blue"), 
                                       pt.bg=c("black","black","black","green","blue"), 
                                       lty=c(0,0,0,1,1), lwd=c(0,0,0,3,3)) 
                        }
                        if (input$HDDfit & input$Flashfit & !input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices", 
                                                     "Fit to Flash Drive prices"), 
                                       pch=c(23, 19, 22,20,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1), 
                                       col=c("black","black","black","green","red"), 
                                       pt.bg=c("black","black","black","green","red"), 
                                       lty=c(0,0,0,1,1), lwd=c(0,0,0,3,3)) 
                        }
                        if (input$HDDfit & input$Flashfit & input$SSDfit){        
                                legend("topright", c("HDD","Flash Drive", "SSD","Fit to HDD prices", 
                                                     "Fit to Flash Drive prices", "Fit to SSD prices"), 
                                       pch=c(23, 19, 22,20,20,20), 
                                       pt.cex=c(1.2,1.2,1.2,0.1,0.1,0.1), 
                                       col=c("black","black","black","green","red","blue"), 
                                       pt.bg=c("black","black","black","green","red","blue"), 
                                       lty=c(0,0,0,1,1,1), lwd=c(0,0,0,3,3,3)) 
                        }
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                
                ########### HDD & Flash ###############
                if (input$HDD & input$Flash & !input$SSD) { 
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
                        if (!input$HDDfit & !input$Flashfit){        
                                legend("topright", c("HDD","Flash Drive"), 
                                       pch=c(23, 19), 
                                       pt.cex=c(1.2,1.2), 
                                       col=c("black","black"), 
                                       pt.bg=c("black","black"))
                        }
                        if (!input$HDDfit & input$Flashfit){        
                                legend("topright", c("HDD","Flash Drive","Fit to Flash Drive prices"), 
                                       pch=c(23, 19,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","red"), 
                                       pt.bg=c("black","black","red"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$HDDfit & !input$Flashfit){        
                                legend("topright", c("HDD","Flash Drive","Fit to HDD prices"), 
                                       pch=c(23, 19,20), 
                                       pt.cex=c(1.2,1.2,0.1), 
                                       col=c("black","black","green"), 
                                       pt.bg=c("black","black","green"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$HDDfit & input$Flashfit){        
                                legend("topright", c("HDD","Flash Drive","Fit to HDD prices", 
                                                     "Fit to Flash Drive prices"), 
                                       pch=c(23, 19,20,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","green","red"), 
                                       pt.bg=c("black","black","green","red"), 
                                       lty=c(0,0,1,1), lwd=c(0,0,3,3)) 
                        }
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                } 
                
                ########### HDD & SSD ###############
                if (input$HDD & !input$Flash & input$SSD) { 
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
                        points(log10(yssd) ~ xssd,pch=22, col=manu, bg=manu, yaxt='n')
                        
                        #fancier axis
                        axis(2, at=-1:8, labels=c("$0.1","$1","$10","$100","$1,000","$10,000","$100,000",
                                                  "$1000,000","$10,000,000",""),las = 1)
                        
                        #fit3
                        if (input$SSDfit) {
                                fit3 <- lm(log10(yssd) ~ poly(xssd,2))
                                predicted.intervals3 <- predict(fit3,data.frame(x=xssd),interval='confidence',
                                                                level=0.99)
                                lines(xssd,predicted.intervals3[,1],col='blue',lwd=3)
                        }
                        #legend
                        if (!input$HDDfit & !input$SSDfit){        
                                legend("topright", c("HDD", "SSD"), 
                                       pch=c(23, 22), 
                                       pt.cex=c(1.2,1.2), 
                                       col=c("black","black"), 
                                       pt.bg=c("black","black"))
                        }
                        if (!input$HDDfit & input$SSDfit){        
                                legend("topright", c("HDD", "SSD","Fit to SSD prices"), 
                                       pch=c(23, 22,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","blue"), 
                                       pt.bg=c("black","black","blue"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$HDDfit & !input$SSDfit){        
                                legend("topright", c("HDD", "SSD","Fit to HDD prices"), 
                                       pch=c(23, 22,20), 
                                       pt.cex=c(1.2,1.2,0.1), 
                                       col=c("black","black","green"), 
                                       pt.bg=c("black","black","green"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$HDDfit & input$SSDfit){        
                                legend("topright", c("HDD", "SSD","Fit to HDD prices", 
                                                     "Fit to SSD prices"), 
                                       pch=c(23, 22,20,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","green","blue"), 
                                       pt.bg=c("black","black","green","blue"), 
                                       lty=c(0,0,1,1), lwd=c(0,0,3,3)) 
                        }
                        
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                
                ########### HDD ###############
                if (input$HDD & !input$Flash & !input$SSD) { 
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
                        
                        #fancier axis
                        axis(2, at=-1:8, labels=c("$0.1","$1","$10","$100","$1,000","$10,000","$100,000",
                                                  "$1000,000","$10,000,000",""),las = 1)
                        
                        #legend
                        if (!input$HDDfit){        
                                legend("topright", c("HDD"), 
                                       pch=c(23), 
                                       pt.cex=c(1.2), 
                                       col=c("black"), 
                                       pt.bg=c("black"))
                        }
                       
                        if (input$HDDfit){        
                                legend("topright", c("HDD","Fit to HDD prices"), 
                                       pch=c(23,20), 
                                       pt.cex=c(1.2,0.1), 
                                       col=c("black","green"), 
                                       pt.bg=c("black","green"), 
                                       lty=c(0,1), lwd=c(0,3)) 
                        }
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                ########### Flash && SSD ###############
                if (!input$HDD & input$Flash & input$SSD) { 
                        plot(log10(yflash) ~ xflash, pch=19, col=manussdflash, bg=manussdflash, 
                             yaxt='n', xlab="Date",ylab="")
                        title(ylab="Price per 1GB in US Dollars", line=5)
                        
                        points(log10(yssd) ~ xssd,pch=22, col=manu, bg=manu, yaxt='n')
                        
                        #fancier axis
                        axis(2, at=-2:4, labels=c("$0.01","$0.1","$1","$10","$100","$1,000",""),las = 1)
                        
                        #fit2
                        if (input$Flashfit) {
                                fit2 <- lm(log10(yflash) ~ poly(xflash,3))
                                predicted.intervals2 <- predict(fit2,data.frame(x=xflash),interval='confidence',
                                                                level=0.99)
                                lines(xflash,predicted.intervals2[,1],col='red',lwd=3)
                        }
                        #fit3
                        if (input$SSDfit) {
                                fit3 <- lm(log10(yssd) ~ poly(xssd,2))
                                predicted.intervals3 <- predict(fit3,data.frame(x=xssd),interval='confidence',
                                                                level=0.99)
                                lines(xssd,predicted.intervals3[,1],col='blue',lwd=3)
                        }
                        #legend
                        if (!input$Flashfit & !input$SSDfit){        
                                legend("topright", c("Flash Drive", "SSD"), 
                                       pch=c(19, 22), 
                                       pt.cex=c(1.2,1.2), 
                                       col=c("black","black"), 
                                       pt.bg=c("black","black"))
                        }
                        if (!input$Flashfit & input$SSDfit){        
                                legend("topright", c("Flash Drive", "SSD","Fit to SSD prices"), 
                                       pch=c(19, 22,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","blue"), 
                                       pt.bg=c("black","black","blue"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$Flashfit & !input$SSDfit){        
                                legend("topright", c("Flash Drive", "SSD","Fit to Flash Drive prices"), 
                                       pch=c(19, 22,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1), 
                                       col=c("black","black","red"), 
                                       pt.bg=c("black","black","red"), 
                                       lty=c(0,0,1), lwd=c(0,0,3)) 
                        }
                        if (input$Flashfit & input$SSDfit){        
                                legend("topright", c("Flash Drive", "SSD", 
                                                     "Fit to Flash Drive prices", "Fit to SSD prices"), 
                                       pch=c(19, 22,20,20), 
                                       pt.cex=c(1.2,1.2,0.1,0.1,0.1), 
                                       col=c("black","black","red","blue"), 
                                       pt.bg=c("black","black","red","blue"), 
                                       lty=c(0,0,1,1), lwd=c(0,0,3,3)) 
                        }
                
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                ############# Flash only ###############
                if (!input$HDD & input$Flash & !input$SSD) { 
                        plot(log10(yflash) ~ xflash, pch=19, col=manussdflash, bg=manussdflash, 
                             yaxt='n', xlab="Date",ylab="")
                        title(ylab="Price per 1GB in US Dollars", line=5)
                        
                        #fancier axis
                        axis(2, at=-2:4, labels=c("$0.01","$0.1","$1","$10","$100","$1,000",""),las = 1)
                        
                        #fit2
                        if (input$Flashfit) {
                                fit2 <- lm(log10(yflash) ~ poly(xflash,3))
                                predicted.intervals2 <- predict(fit2,data.frame(x=xflash),interval='confidence',
                                                                level=0.99)
                                lines(xflash,predicted.intervals2[,1],col='red',lwd=3)
                        }
                        
                        #legend
                        if (!input$Flashfit){        
                                legend("topright", c("Flash Drive"), 
                                       pch=c(19), 
                                       pt.cex=c(1.2), 
                                       col=c("black"), 
                                       pt.bg=c("black"))
                        }
                        if (input$Flashfit){        
                                legend("topright", c("Flash Drive","Fit to Flash Drive prices"), 
                                       pch=c(19,20), 
                                       pt.cex=c(1.2,0.1), 
                                       col=c("black","red"), 
                                       pt.bg=c("black","red"), 
                                       lty=c(0,1), lwd=c(0,3)) 
                        }
                        
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                ########### SSD only ###############
                if (!input$HDD & !input$Flash & input$SSD) { 
                        plot(log10(yssd) ~ xssd, pch=22, col=manussdflash, bg=manussdflash, 
                             yaxt='n', xlab="Date",ylab="",ylim=c(-1,0))
                        title(ylab="Price per 1GB in US Dollars", line=5)
                        
                        #fancier axis
                        axis(2, at=-2:1, labels=c("$0.01","$0.1","$1",""),las = 1)
                        
                        #fit3
                        if (input$SSDfit) {
                                fit3 <- lm(log10(yssd) ~ poly(xssd,2))
                                predicted.intervals3 <- predict(fit3,data.frame(x=xssd),interval='confidence',
                                                                level=0.99)
                                lines(xssd,predicted.intervals3[,1],col='blue',lwd=3)
                        }
                        #legend
                        if (!input$SSDfit){        
                                legend("topright", c("SSD"), 
                                       pch=c(22), 
                                       pt.cex=c(1.2), 
                                       col=c("black"), 
                                       pt.bg=c("black"))
                        }
                        if (input$SSDfit){        
                                legend("topright", c("SSD","Fit to SSD prices"), 
                                       pch=c(22,20), 
                                       pt.cex=c(1.2,0.1), 
                                       col=c("black","blue"), 
                                       pt.bg=c("black","blue"), 
                                       lty=c(0,1), lwd=c(0,3)) 
                        }
                        
                        legend("bottomleft", legend=levels(manu), pch=19, col=unique(manu))
                        
                        #grid
                        grid(nx = 19, col = "lightgray", lty = "dotted",
                             lwd = par("lwd"), equilogs = TRUE)
                }
                
        }, height = 480, width = 700 )
})
                