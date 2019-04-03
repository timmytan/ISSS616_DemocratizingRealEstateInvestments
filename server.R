shinyServer(function(input, output,session) {
      # median transaction value in 2018
      output$median2018Box <- renderValueBox({
            median <- median(year2018$TransactedPrice)
            valueBox(
                  paste0("$", median), paste("Median Home Value in 2018"),
                  icon = NULL, color = "red")
      })
      
      # annualized change in median home values from 2014 to 2018
      output$annualizedChangeInMedianBox <- renderValueBox({
            median2018 <- median(year2018$TransactedPrice)
            median2014 <- median(year2014$TransactedPrice)
            overallChange <- (median2018-median2014)/median2014
            annualizedChange <- (1+overallChange)^(1/5) - 1
            valueBox(
                  paste0(round(annualizedChange * 100, 4), "%"), paste("Annualized Price Change from 2014 - 2018"),
                  icon = NULL, color = "blue")
      })
      
      # top 10 planning areas by annual growth rate 
      output$top10PlanningArea <- renderChart({
            med2018 <- aggregate(year2018$TransactedPrice ~ year2018$PlanningArea, FUN = median)
            med2014 <- aggregate(year2014$TransactedPrice ~ year2014$PlanningArea, FUN = median)
            combine <- merge(med2014, med2018, by.x = "year2014$PlanningArea", by.y = "year2018$PlanningArea")
            combine$overallChange <- (combine$`year2018$TransactedPrice` - combine$`year2014$TransactedPrice`)/combine$`year2014$TransactedPrice`
            combine$annualizedChange <- round(((1 + combine$overallChange)^(1/5) - 1) * 100, 4)
            top10 <- arrange(combine, desc(annualizedChange))[1:10,]
            p <- nPlot(annualizedChange ~ `year2014$PlanningArea`, data = top10, type = "discreteBarChart", dom = "top10PlanningArea")
            p$xAxis(axisLabel = "Plannning Region")
            p$yAxis(axisLabel = "Annual Growth Rate (%)", width = 40)
            p$set(height = 300, width = 1300)
            return(p)
      })
      
      #########################################################################################
      #                              Market Selection Functions                               #
      #########################################################################################
      
      output$areaUI <- renderUI({
            area <- sort(unique(year2018$PlanningArea))
            selectInput("planningArea", label = "Planning Area:", choices = c(Choose ='', as.character(area)), selected = dflt$planningArea, selectize = FALSE)
      })
      
      output$tenureUI <- renderUI({
            tenure <- sort(unique(year2018$Tenure))
            selectInput("tenure", label = "Tenure:", choices = c(Choose = '', as.character(tenure)), selected = dflt$tenure, selectize = FALSE)
      })
      
      output$minChildcareDist <- renderText(input$childcareDist[1])
      output$maxChildcareDist <- renderText(input$childcareDist[2])
      output$minMrtDist <- renderText(input$mrtDist[1])
      output$maxMrtDist <- renderText(input$mrtDist[2])
      
      
      #########################################################################################
      #                              Value Analysis Functions                               #
      #########################################################################################
      
      # get and screen data based upon mrt distance
      ## to input childcare distance after loading in new data
      screenData <- function(){
            d <- realis
            
            minMrt <- as.numeric(input$mrtDist[1])
            maxMrt <- as.numeric(input$mrtDist[2])
            
            d <- subset(d, MrtDist >= minMrt & MrtDist <= maxMrt)
            
            return(d)
      }
      
      getAreaData <- function() {
            d <- screenData()
            
            # filter by planning area
            if (!is.null(input$planningArea) & (input$planningArea != "")) {
                  d <- d[which(d$PlanningArea == input$planningArea),]
            }
            
            return(d)
      }
      
      getData <- eventReactive(input$analyze, {
            d <- getAreaData()
            
            validate(
                  need(nrow(d) > 0, "No historical transactions meet your criteria.")
            )
            
            return(d)
            
      }, ignoreNULL = FALSE)
      
      
      #########################################################################################
      #                                  Value Analysis Outputs                               #
      #########################################################################################
      
      output$medPriceTS <- renderChart({
            withProgress(message = "Rendering Time-Series for Your Selected Criteria", {
                  d <- getData()
                  d <- data.frame(d$TransactedPrice, d$SaleDate)
                  colnames(d) <- c("TransactedPrice", "SaleDate")
                  z <- xts(d$TransactedPrice, as.Date(d$SaleDate, "%Y-%m-%d"))
                  medts <- apply.monthly(z, median)
                  medtsDF <- data.frame(date=index(medts), coredata(medts))
                  colnames(medtsDF) <- c("Time", "TransactionValue")
                  medtsDF$Time <- as.Date.character(medtsDF$Time)
                  
                  p <- nPlot(TransactionValue ~ Time, type = "lineChart", data = medtsDF, dom = "medPriceTS", height = 400, width = 680)
                  p$xAxis(
                        axisLabel = "Time",
                        tickFormat = 
                              "#!
                        function(d){
                        f =  d3.time.format.utc('%b-%y');
                        return f(new Date( d*24*60*60*1000 ));
                        }
                        !#"
                  )
                  p$chart(showLegend = FALSE)
                  
                  return(p)
            })
      })
      
})
