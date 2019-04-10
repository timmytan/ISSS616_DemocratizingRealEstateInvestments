shinyServer(function(input, output,session) {
      # median transaction value in 2018
      output$median2018Box <- renderValueBox({
            median <- median(year2018$TransactedPrice)
            valueBox(
                  paste0("$", median), h4("Median Home Value in 2018"),
                  icon = NULL, color = "red")
      })
      
      # annualized change in median home values from 2014 to 2018
      output$annualizedChangeInMedianBox <- renderValueBox({
            median2018 <- median(year2018$TransactedPrice)
            median2014 <- median(year2014$TransactedPrice)
            overallChange <- (median2018-median2014)/median2014
            annualizedChange <- (1+overallChange)^(1/5) - 1
            valueBox(
                  paste0(round(annualizedChange * 100, 4), "%"), h4("Annualized Price Change - 2014 to 2018"),
                  icon = NULL, color = "blue")
      })
      
      # number of transactions in 2018
      output$volume2018Box <- renderValueBox({
            count <- nrow(year2018)
            valueBox(
                  paste0(count), h4("Transactions in 2018"),
                  icon = NULL, color = "green")
      })
      
      # mean PSF in 2018
      output$meanPSF2018Box <- renderValueBox({
            mean <- round(mean(year2018$PricePSF),0)
            valueBox(
                  paste0("$", mean), h4("Average PSF in 2018"),
                  icon = NULL, color = "yellow")
      })
      
      # top 10 planning areas by annual growth rate 
      output$top10PlanningArea <- renderChart({
            med2018 <- aggregate(year2018$TransactedPrice ~ year2018$PlanningArea, FUN = median)
            med2014 <- aggregate(year2014$TransactedPrice ~ year2014$PlanningArea, FUN = median)
            combine <- merge(med2014, med2018, by.x = "year2014$PlanningArea", by.y = "year2018$PlanningArea")
            combine$overallChange <- (combine$`year2018$TransactedPrice` - combine$`year2014$TransactedPrice`)/combine$`year2014$TransactedPrice`
            combine$annualizedChange <- round(((1 + combine$overallChange)^(1/5) - 1) * 100, 4)
            top10 <- arrange(combine, desc(annualizedChange))[1:10,]
            p <- nPlot(annualizedChange ~ `year2014$PlanningArea`, data = top10, type = "discreteBarChart", dom = "top10PlanningArea", height = 300, width = 1300)
            p$xAxis(axisLabel = "Plannning Region")
            p$yAxis(axisLabel = "Annual Growth Rate (%)", width = 40)
            return(p)
      })
      
      # top 10 planning areas time series
      output$top10TS <- renderChart({
            med2018 <- aggregate(year2018$TransactedPrice ~ year2018$PlanningArea, FUN = median)
            med2014 <- aggregate(year2014$TransactedPrice ~ year2014$PlanningArea, FUN = median)
            combine <- merge(med2014, med2018, by.x = "year2014$PlanningArea", by.y = "year2018$PlanningArea")
            combine$overallChange <- (combine$`year2018$TransactedPrice` - combine$`year2014$TransactedPrice`)/combine$`year2014$TransactedPrice`
            combine$annualizedChange <- round(((1 + combine$overallChange)^(1/5) - 1) * 100, 4)
            top10 <- arrange(combine, desc(annualizedChange))[1:10,]
            top10 <- data.frame(top10[1:10,1])
            colnames(top10) <- "PlanningArea"
            
            d <- realis
            d$Quarter <- as.yearqtr(d$SaleDate, "%Y-%m-%d")
            d <- aggregate(d$PricePSF ~ d$PlanningArea + d$Quarter, FUN = median)
            colnames(d) <- c("PlanningArea","Quarter","PricePSF")
            top10psf <- merge(top10, d, by = "PlanningArea")
            top10psf <- tidyr::spread(top10psf, PlanningArea, PricePSF)
            top10psf <- melt(top10psf, id = "Quarter")
            names(top10psf) <- c("Quarter", "PlanningArea", "PricePSF")
            top10psf$Quarter <- as.Date(as.yearqtr(top10psf$Quarter, format = "y %Q"), frac = 1)
            p <- nPlot(PricePSF ~ Quarter, group = "PlanningArea", type = "lineChart", data = top10psf, dom = "top10TS", height = 550, width = 1300)
            p$xAxis(
                  axisLabel = "Time",
                  tickFormat = 
                        "#!
                  function(d){
                  f =  d3.time.format.utc('%b %y');
                  return f(new Date( d*24*60*60*1000 ));
                  }
                  !#"
            )
            p$yAxis(axisLabel = "Median Price (PSF)", width = 40)
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
      #                              Value Analysis Functions                                 #
      #########################################################################################
      
      # get and screen data based upon mrt distance
      ## to input childcare distance after loading in new data
      screenData <- function(){
            d <- realis
            
            minMrt <- as.numeric(input$mrtDist[1])
            maxMrt <- as.numeric(input$mrtDist[2])
            minChildcare <- as.numeric(input$childcareDist[1])
            maxChildcare <- as.numeric(input$childcareDist[2])
            minPropertyAge <- as.numeric(input$propertyAge[1])
            maxPropertyAge <- as.numeric(input$propertyAge[2])
            
            d <- subset(d, MrtDist >= minMrt & 
                              MrtDist <= maxMrt & 
                              ChildcareDistance_m >= minChildcare & 
                              ChildcareDistance_m <= maxChildcare &
                              AgeOfProperty >= minPropertyAge &
                              AgeOfProperty <= maxPropertyAge)
            
            return(d)
      }
      
      getAreaData <- function() {
            d <- screenData()
            
            # filter by planning area
            if (!is.null(input$planningArea) & (input$planningArea != "")) {
                  d <- d[which(d$PlanningArea == input$planningArea),]
            }
            
            # filter by tenure
            if (!is.null(input$tenure) & (input$tenure != "")) {
                  d <- d[which(d$Tenure == input$tenure),]
            }
            
            return(d)
      }
      
      getData <- eventReactive(input$analyze, {
            d <- getAreaData()
            
            return(d)
            
      }, ignoreNULL = FALSE)
      
      #########################################################################################
      #                                     Heat Map                                          #
      #########################################################################################
      
      # color palette & test binning
      bins <- c(0, 1, 1.25, 1.50, 1.75, 2, 2.5, 3, Inf)
      
      #pal for palette
      pal <- colorNumeric("plasma",domain = planning_area_map@data$n_transaction, NULL)
      
      labels<-sprintf(
            "<strong>%s</strong><br/>Mean $%.3g million<br/>Median $%.3g million<br/>Max $%.3g million<br/>%g transaction</sup>",
            planning_area_map@data$name,planning_area_map@data$mean_price,planning_area_map@data$median_price,
            planning_area_map@data$max_price,planning_area_map@data$n_transaction) %>% lapply(htmltools::HTML)
      
      #map output
      output$map <- renderLeaflet({
            leaflet(planning_area_map) %>%
                  setView(103.80835, 1.360365,zoom=11) %>%
                  addTiles() %>%
                  addPolygons(fillColor=~pal(planning_area_map@data$n_transaction), stroke=FALSE, smoothFactor = 0.3, fillOpacity = 1,dashArray =1,
                              highlight=highlightOptions(
                                    weight=5,
                                    color="#666",
                                    dashArray =3,
                                    fillOpacity=0.7,
                                    bringToFront=TRUE
                              ),
                              label=labels,
                              labelOptions=labelOptions(
                                    style=list("font-weight"="normal",padding="3px 8px"),
                                    textsize="15px",
                                    direction="auto"
                              )
                  ) %>%
                  leaflet::addLegend(pal=pal,values=planning_area_map@data$n_transaction, opacity=0.7, title=NULL, position="bottomright")
      })
      
      
      #########################################################################################
      #                                  Value Analysis Outputs                               #
      #########################################################################################
      
      output$medPriceTS <- renderChart({
            withProgress(message = "Rendering Time-Series for Your Selected Criteria", {
                  d <- getData()
                  shiny::validate(
                        need(nrow(d) > 1, "Not enough data. Please adjust parameters.")
                  )
                  d <- data.frame(d$TransactedPrice, d$SaleDate)
                  colnames(d) <- c("TransactedPrice", "SaleDate")
                  z <- xts(d$TransactedPrice, as.Date(d$SaleDate, "%Y-%m-%d"))
                  medts <- apply.monthly(z, median)
                  medtsDF <- data.frame(date=index(medts), coredata(medts))
                  colnames(medtsDF) <- c("Time", "TransactionValue")
                  medtsDF$Time <- as.Date.character(medtsDF$Time)
                  
                  p <- nPlot(TransactionValue ~ Time, type = "lineChart", data = medtsDF, dom = "medPriceTS", height = 400, width = 600)
                  p$xAxis(
                        axisLabel = "Time",
                        tickFormat = 
                              "#!
                        function(d){
                        f =  d3.time.format.utc('%b %y');
                        return f(new Date( d*24*60*60*1000 ));
                        }
                        !#"
                  )
                  p$chart(showLegend = FALSE)
                  
                  return(p)
            })
      })# end of renderchart
      
      output$meanPsfTS <- renderChart({
            withProgress(message = "Rendering Time-Series for Your Selected Criteria", {
                  d <- getData()
                  shiny::validate(
                        need(nrow(d) > 1, "Not enough data. Please adjust parameters.")
                  )
                  d <- data.frame(d$PricePSF, d$SaleDate)
                  colnames(d) <- c("PricePSF", "SaleDate")
                  z <- xts(d$PricePSF, as.Date(d$SaleDate, "%Y-%m-%d"))
                  meanPsfTS <- apply.monthly(z, mean)
                  meanPsfTS <- round(meanPsfTS, 0)
                  meanPsfTSDF <- data.frame(date=index(meanPsfTS), coredata(meanPsfTS))
                  colnames(meanPsfTSDF) <- c("Time", "PricePSF")
                  meanPsfTSDF$Time <- as.Date.character(meanPsfTSDF$Time)
                  
                  p <- nPlot(PricePSF ~ Time, type = "lineChart", data = meanPsfTSDF, dom = "meanPsfTS", height = 400, width = 600)
                  p$xAxis(
                        axisLabel = "Time",
                        tickFormat = 
                              "#!
                        function(d){
                        f =  d3.time.format.utc('%b %y');
                        return f(new Date( d*24*60*60*1000 ));
                        }
                        !#"
                  )
                  p$chart(showLegend = FALSE)
                  
                  return(p)
      })
})# end of renderchart
      
})
