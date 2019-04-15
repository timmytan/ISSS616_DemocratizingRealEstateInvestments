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
      
      #########################################################################################
      #                             CI Functions                                              #
      #########################################################################################
      output$areaUICI_1 <- renderUI({
        area <- sort(unique(year2018$PlanningArea))
        selectInput("planningAreaCI_1", label = "Planning Area 1:", choices = c(Choose ='', as.character(area)), selected = dflt$planningArea, selectize = FALSE)
      })
      
      output$areaUICI_2 <- renderUI({
        area <- sort(unique(year2018$PlanningArea))
        selectInput("planningAreaCI_2", label = "Planning Area 2:", choices = c(Choose ='', as.character(area)), selected = dflt$planningArea, selectize = FALSE)
      })
      
      
      CI_Data<-eventReactive(input$analyzeCI,{
        d<-realis
        areaCIname_1<-as.character(input$planningAreaCI_1)
        areaCIname_2<-as.character(input$planningAreaCI_2)
        
        d <- subset(d, PlanningArea==areaCIname_1 | PlanningArea==areaCIname_2,
                    select=c(PlanningArea,PricePSF))
        
        model<-lm(d$PricePSF~d$PlanningArea,data=d)
        anova_model<-aov(model)
        tukey_model<-TukeyHSD(x=anova_model,'d$PlanningArea',conf.level=input$conf_level)
        
        CIdiff<-tukey_model$`d$PlanningArea`[1,1]
        CIlwr<-tukey_model$`d$PlanningArea`[1,2]
        CIupr<-tukey_model$`d$PlanningArea`[1,3]
        CIpadj<-tukey_model$`d$PlanningArea`[1,4]
        
        if(CIpadj>=0.5*(1-input$conf_level)){
          
          CIstatement<-sprintf(
            "The mean difference between the two planning areas is %.4g.<br/> The lower and upper limits of the mean difference (at %.3g confidence interval) are %.4g and %.4g respectively.<br/> 
            P-value is %.3g > %.3g. Hence, the difference between the mean price (PSF) of properties in %s and properties in %s is not significantly different.",
            CIdiff,input$conf_level,CIlwr,CIupr,CIpadj,0.5*(1-input$conf_level),areaCIname_1,areaCIname_2,input$conf_level)%>% lapply(htmltools::HTML)
        }
        
        else{
          
          CIstatement<-sprintf(
                "- The mean difference between the two planning areas is %.4g.<br/><br/>- The lower and upper limits of the mean difference (at %.3g confidence interval) are %.4g and %.4g respectively.<br/><br/> 
           - P-value is %.3g < %.3g.<br/><br/>- Hence, the difference between the mean price(PSF) of properties in %s and properties in %s is significantly different.",
                CIdiff,input$conf_level,CIlwr,CIupr,CIpadj,0.5*(1-input$conf_level),areaCIname_1,areaCIname_2,input$conf_level)%>% lapply(htmltools::HTML)
        }
        
        return(CIstatement)
        
        
      }, ignoreNULL = FALSE)
      
      CI_boxplot<-eventReactive(input$analyzeCI,{
        d<-realis
        areaCIname_1<-as.character(input$planningAreaCI_1)
        areaCIname_2<-as.character(input$planningAreaCI_2)
        
        d1 <- subset(d, PlanningArea==areaCIname_1,
                     select=c(PlanningArea,PricePSF))
        
        d2 <- subset(d, PlanningArea==areaCIname_2,
                     select=c(PlanningArea,PricePSF))
        
        a <- boxplot(d1$PricePSF,d2$PricePSF,ylab="Price PSF ($)",
                     names =c(areaCIname_1,areaCIname_2),xlab="Planning Area",
                     col=c('blue','red')
        )
        
        
        return(a)
        
        
      }, ignoreNULL = FALSE)
      
      CI_boxplot2<-eventReactive(input$analyzeCI,{
        d<-realis
        areaCIname_1<-as.character(input$planningAreaCI_1)
        areaCIname_2<-as.character(input$planningAreaCI_2)
        
        d <- subset(d, PlanningArea==areaCIname_1 | PlanningArea==areaCIname_2,
                    select=c(PlanningArea,PricePSF))
        
        model<-lm(d$PricePSF~d$PlanningArea,data=d)
        anova_model<-aov(model)
        tukey_model<-TukeyHSD(x=anova_model,'d$PlanningArea',conf.level=input$conf_level)
        
        a <-plot(tukey_model,las=3,col="blue")
        
        return(a)
        
      }, ignoreNULL = FALSE)
      
      #########################################################################################
      #                             CI Output                                                 #
      #########################################################################################
      
      output$CItest<-renderUI({
        CI_Data()
      })
      
      output$CIplot<-renderPlot({
        CI_boxplot()
      })
      
      output$CIplot2<-renderPlot({
        CI_boxplot2()
      })
      
      #########################################################################################
      #                              Price Estimate Functions                               #
      #########################################################################################
      
      output$RegionUIMLR <- renderUI({
            area <- sort(unique(year2018$PlanningRegion))
            selectInput("PlanningRegionMLR", label = "Planning Region:", choices = c(Choose ='', as.character(area)), selected = dflt$planningRegion, selectize = FALSE)
      })
      
      output$tenureUIMLR <- renderUI({
            tenure <- sort(unique(year2018$Tenure))
            selectInput("tenureMLR", label = "Tenure:", choices = c(Choose = '', as.character(tenure)), selected = dflt$tenure, selectize = FALSE)
      })
      
      output$PropertyTypeUIMLR <- renderUI({
            tenure <- sort(unique(year2018$PropertyType))
            selectInput("PropertyTypeMLR", label = "Property Type:", choices = c(Choose = '', as.character(tenure)), selected = dflt$PropertyType, selectize = FALSE)
      })
      
      output$saletypeUIMLR <- renderUI({
            tenure <- sort(unique(year2018$SaleType))
            selectInput("SaleTypeMLR", label = "Sale Type:", choices = c(Choose = '', as.character(tenure)), selected = dflt$PropertyType, selectize = FALSE)
      })
      
      output$minMrtDist <- renderText(input$mrtDist[1])
      
      output$minMrtDist <- renderText(input$mrtDist[1])
      
      
      #Output for Expected Value
      
      
      output$MLR<- renderText({
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        #MLR Model
        #Linear model via backwards,forward and stepwise yield the same result, model and all independent variables are significant 
        
        #backwards multivariate-linear regression test
        #model = lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
        #          , data=realisMLR)
        
        #model=stepAIC(model, direction ="backwards")
        #summary(model)
        
        #forward multivariate-linear regression test
        #model = lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
        #           , data=realisMLR)
        
        #model=stepAIC(model, direction ="forward")
        #summary(model)
        
        #sideways multivariate-linear regression test
        #model = lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
        #           , data=realisMLR)
        
        #model=stepAIC(model, direction ="both")
        #summary(model)
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        #matching categorical variables to derive coefficient
        PlanningRegion=paste('PlanningRegion',input$PlanningRegionMLR,sep='')
        ColforPlanningRegion<-match(PlanningRegion,coefficientnames)
        PlanningRegionCoefficient=unname(summaryMLR)[ColforPlanningRegion]
        
        if(input$PlanningRegionMLR=="Central Region"){
          PlanningRegionCoefficient<-0
        }
        
        Tenure=paste('Tenure',input$tenureMLR,sep='')
        ColforTenure<-match(Tenure,coefficientnames)
        TenureCoefficient=unname(summaryMLR)[ColforTenure]
        
        if(input$tenureMLR=="99 Yrs Leasehold"){
          TenureCoefficient<-0
        }
        
        SaleType=paste('SaleType',input$SaleTypeMLR,sep='')
        ColforSaleType<-match(SaleType,coefficientnames)
        SaleTypeCoefficient=unname(summaryMLR)[ColforSaleType]
        
        if(input$SaleTypeMLR=="New Sale"){
          SaleTypeCoefficient<-0
        }
        
        Intercept<-unname(summaryMLR)[1]
        AreaSQMCoefficient<-unname(summaryMLR)[2]
        PropertyAgeCoefficient<-unname(summaryMLR)[3]
        FloorCoefficient<-unname(summaryMLR)[4]
        StopsfromCityCentreCoefficient<-unname(summaryMLR)[5]
        ChilcareDistCoefficient<-unname(summaryMLR)[6]
        MrtDistCoefficient<-unname(summaryMLR)[7]
        
        
        #Expected Property Value based on user input (only Floor level for now)
        ExpectedValue<-round((input$AreaSQM*AreaSQMCoefficient+input$propertyAgeMLR*PropertyAgeCoefficient+input$FloorLevel*FloorCoefficient +input$ChildcareDistanceMLR*ChilcareDistCoefficient+input$MrtDistMLR*MrtDistCoefficient
                              +PlanningRegionCoefficient+TenureCoefficient+SaleTypeCoefficient+StopsfromCityCentreCoefficient*input$NoofStopsAwayFromCityCentre+Intercept),0)
        
        
        sprintf(
          "- %i to %i stops from the City Centre (Raffles Place MRT Station) in the %s .<br /><br />
          - Tenure: %s <br /><br />
          - Sales Type: %s <br /><br />
          - Floor Level: %i to %i <br /><br />
          - Size of Unit : %i to %i Square Meters<br /><br />
          - Distance from Nearest ChildCare Centre: %i to %i m<br /><br />
          - Distance from MRT: %i to %i m" ,
          input$NoofStopsAwayFromCityCentre[1],input$NoofStopsAwayFromCityCentre[2], input$PlanningRegionMLR, input$tenureMLR, input$SaleTypeMLR, input$FloorLevel[1], input$FloorLevel[2],input$AreaSQM[1],input$AreaSQM[2], input$ChildcareDistanceMLR[1],input$ChildcareDistanceMLR[2],input$MrtDistMLR[1],input$MrtDistMLR[2]
        )
      })
      
      
      
      output$MLR2<- renderText({
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        #matching categorical variables to derive coefficient
        PlanningRegion=paste('PlanningRegion',input$PlanningRegionMLR,sep='')
        ColforPlanningRegion<-match(PlanningRegion,coefficientnames)
        PlanningRegionCoefficient=prettyNum(round(unname(summaryMLR)[ColforPlanningRegion]),big.mark=",",scientific=FALSE)
        
        if(input$PlanningRegionMLR=="Central Region"){
          PlanningRegionCoefficient<-0
        }
        
        Tenure=paste('Tenure',input$tenureMLR,sep='')
        ColforTenure<-match(Tenure,coefficientnames)
        TenureCoefficient=prettyNum(round(unname(summaryMLR)[ColforTenure],0),big.mark=",",scientific=FALSE)
        
        
        if(input$tenureMLR=="99 Yrs Leasehold"){
          TenureCoefficient<-0
        }
        
        SaleType=paste('SaleType',input$SaleTypeMLR,sep='')
        ColforSaleType<-match(SaleType,coefficientnames)
        SaleTypeCoefficient=prettyNum(round(unname(summaryMLR)[ColforSaleType]),big.mark=",",scientific=FALSE)
        
        if(input$SaleTypeMLR=="New Sale"){
          SaleTypeCoefficient<-0
        }
        
        
        Intercept<-prettyNum(round(unname(summaryMLR)[1],0),big.mark=",",scientific=FALSE)
        AreaSQMCoefficient<-prettyNum(round(unname(summaryMLR)[2],0),big.mark=",",scientific=FALSE)
        PropertyAgeCoefficient<-prettyNum(round(unname(summaryMLR)[3],0),big.mark=",",scientific=FALSE)
        FloorCoefficient<-prettyNum(round(unname(summaryMLR)[4],0),big.mark=",",scientific=FALSE)
        StopsfromCityCentreCoefficient<-prettyNum(round(unname(summaryMLR)[5],0),big.mark=",",scientific=FALSE)
        ChilcareDistCoefficient<-prettyNum(round(unname(summaryMLR)[6],0),big.mark=",",scientific=FALSE)
        MrtDistCoefficient<-prettyNum(round(unname(summaryMLR)[7],0),big.mark=",",scientific=FALSE)
        
        AreaSQMMinValue<-prettyNum(round(unname(summaryMLR)[2]*input$AreaSQM[1],0),big.mark=",",scientific=FALSE)
        AreaSQMMaxValue<-prettyNum(round(unname(summaryMLR)[2]*input$AreaSQM[2],0),big.mark=",",scientific=FALSE)
        PropertyAgeMinValue<-prettyNum(round(unname(summaryMLR)[3]*input$propertyAgeMLR[1],0),big.mark=",",scientific=FALSE)
        PropertyAgeMaxValue<-prettyNum(round(unname(summaryMLR)[3]*input$propertyAgeMLR[2],0),big.mark=",",scientific=FALSE)
        FloorMinValue<-prettyNum(round(unname(summaryMLR)[4]*input$FloorLevel[1],0),big.mark=",",scientific=FALSE)
        FloorMaxValue<-prettyNum(round(unname(summaryMLR)[4]*input$FloorLevel[2],0),big.mark=",",scientific=FALSE)
        StopsfromCityCentreMinValue<-prettyNum(round(unname(summaryMLR)[5]*input$NoofStopsAwayFromCityCentre[1],0),big.mark=",",scientific=FALSE)
        StopsfromCityCentreMaxValue<-prettyNum(round(unname(summaryMLR)[5]*input$NoofStopsAwayFromCityCentre[2],0),big.mark=",",scientific=FALSE)
        ChilcareDistMinValue<-prettyNum(round(unname(summaryMLR)[6]*input$ChildcareDistanceMLR[1],0),big.mark=",",scientific=FALSE)
        ChilcareDistMaxVAlue<-prettyNum(round(unname(summaryMLR)[6]*input$ChildcareDistanceMLR[2],0),big.mark=",",scientific=FALSE)
        MrtDistMinValue<-prettyNum(round(unname(summaryMLR)[7]*input$MrtDistMLR[1],0),big.mark=",",scientific=FALSE)
        MrtDistMaxValue<-prettyNum(round(unname(summaryMLR)[7]*input$MrtDistMLR[2],0),big.mark=",",scientific=FALSE)
        
        
        #Expected Property Value based on user input (only Floor level for now)
        
        
        sprintf(
          "Model: <br /><br />
          Estimated Price = <br /> <br />
          %s * AreaSQM <br /> <br />
          %s * Property Age <br /> <br />
          + %s * Floor Level <br /> <br />
          %s * Stops from City Centre <br /> <br />
          + %s * Distance from nearest Childcare Centre  <br /><br />
          %s * Distance from nearest MRT Station <br /><br />
          + %s ( %s ) <br /><br />
          %s (%s) <br /><br />
          + %s (%s) <br /><br />
          + %s (intercept)" ,
          AreaSQMCoefficient,
          PropertyAgeCoefficient,
          FloorCoefficient,
          StopsfromCityCentreCoefficient,
          ChilcareDistCoefficient, 
          MrtDistCoefficient, 
          SaleTypeCoefficient, input$SaleTypeMLR, PlanningRegionCoefficient, input$PlanningRegionMLR, TenureCoefficient, input$tenureMLR, Intercept)
      })
      
      
      output$MLR3<- renderValueBox({
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        adj_r_squared=round(summary(model)$adj.r.squared,2)
        
        valueBox(
          paste0(adj_r_squared), h4("Adjusted R Square of Model"), 
          icon = NULL, color = "navy")
      })
      
      output$MLR4<- renderValueBox({
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        #matching categorical variables to derive coefficient
        PlanningRegion=paste('PlanningRegion',input$PlanningRegionMLR,sep='')
        ColforPlanningRegion<-match(PlanningRegion,coefficientnames)
        PlanningRegionCoefficient=unname(summaryMLR)[ColforPlanningRegion]
        
        if(input$PlanningRegionMLR=="Central Region"){
          PlanningRegionCoefficient<-0
        }
        
        Tenure=paste('Tenure',input$tenureMLR,sep='')
        ColforTenure<-match(Tenure,coefficientnames)
        TenureCoefficient=unname(summaryMLR)[ColforTenure]
        
        if(input$tenureMLR=="99 Yrs Leasehold"){
          TenureCoefficient<-0
        }
        
        SaleType=paste('SaleType',input$SaleTypeMLR,sep='')
        ColforSaleType<-match(SaleType,coefficientnames)
        SaleTypeCoefficient=unname(summaryMLR)[ColforSaleType]
        
        if(input$SaleTypeMLR=="New Sale"){
          SaleTypeCoefficient<-0
        }
        
        Intercept<-unname(summaryMLR)[1]
        AreaSQMCoefficient<-unname(summaryMLR)[2]
        PropertyAgeCoefficient<-unname(summaryMLR)[3]
        FloorCoefficient<-unname(summaryMLR)[4]
        StopsfromCityCentreCoefficient<-unname(summaryMLR)[5]
        ChilcareDistCoefficient<-unname(summaryMLR)[6]
        MrtDistCoefficient<-unname(summaryMLR)[7]
        
        #Expected Property Value based on user input (only Floor level for now)
        ExpectedMinValue<-round((input$AreaSQM[1]*AreaSQMCoefficient+input$propertyAgeMLR[2]*PropertyAgeCoefficient+input$FloorLevel[1]*FloorCoefficient +input$ChildcareDistanceMLR[1]*ChilcareDistCoefficient+input$MrtDistMLR[2]*MrtDistCoefficient
                                 +PlanningRegionCoefficient+TenureCoefficient+SaleTypeCoefficient+StopsfromCityCentreCoefficient*input$NoofStopsAwayFromCityCentre[2]+Intercept),0)
        
        ExpectedMaxValue<-round((input$AreaSQM[2]*AreaSQMCoefficient+input$propertyAgeMLR[1]*PropertyAgeCoefficient+input$FloorLevel[2]*FloorCoefficient +input$ChildcareDistanceMLR[2]*ChilcareDistCoefficient+input$MrtDistMLR[1]*MrtDistCoefficient
                                 +PlanningRegionCoefficient+TenureCoefficient+SaleTypeCoefficient+StopsfromCityCentreCoefficient*input$NoofStopsAwayFromCityCentre[1]+Intercept),0)
        
        
        ExpectedMinValue<-prettyNum(ExpectedMinValue,big.mark=",",scientific=FALSE)
        ExpectedMaxValue<-prettyNum(ExpectedMaxValue,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0("$" ,ExpectedMinValue, " - ", "$", ExpectedMaxValue), h4("Expected Transaction Price"), 
          icon = NULL, color = "green")
      })
      
      
      output$InterceptCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        Intercept<-round(unname(summaryMLR)[1],0)
        
        Intercept<-prettyNum(Intercept,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( Intercept), h4("Intercept"),
          icon = NULL, color = "red")
      })
      
      output$AreaCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        AreaSQM<-round(unname(summaryMLR)[2],0)
        
        AreaSQM<-prettyNum(AreaSQM,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0("+", AreaSQM), h4("AreaSQM"),
          icon = NULL, color = "olive")
      })
      
      output$PropertyAgeCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        PropertyAge<-round(unname(summaryMLR)[3],0)
        
        PropertyAge<-prettyNum(PropertyAge,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( PropertyAge), h4("Property Age"),
          icon = NULL, color = "olive")
      })
      
      output$FloorCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        Floor<-round(unname(summaryMLR)[4],0)
        
        Floor<-prettyNum(Floor,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0(Floor), h4("Floor Level"),
          icon = NULL, color = "olive")
      })
      
      output$StopsfromCityCentreCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        StopsfromCityCentre<-round(unname(summaryMLR)[5],0)
        
        StopsfromCityCentre<-prettyNum(StopsfromCityCentre,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( StopsfromCityCentre), h4("Stops from City Centre"),
          icon = NULL, color = "olive")
      })
      
      output$ChilcareDistCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        ChilcareDist<-round(unname(summaryMLR)[6],0)
        
        ChilcareDist<-prettyNum(ChilcareDist,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0("+", ChilcareDist), h4("Distance (Childcare)"),
          icon = NULL, color = "olive")
      })
      
      output$MrtDistCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        MrtDist<-round(unname(summaryMLR)[7],0)
        
        MrtDist<-prettyNum(MrtDist,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( MrtDist), h4("Distance (MRT Station)"),
          icon = NULL, color = "olive")
      })
      
      output$PlanningRegionCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        PlanningRegion=paste('PlanningRegion',input$PlanningRegionMLR,sep='')
        ColforPlanningRegion<-match(PlanningRegion,coefficientnames)
        PlanningRegionCoefficient=unname(summaryMLR)[ColforPlanningRegion]
        
        if(input$PlanningRegionMLR=="Central Region"){
          PlanningRegionCoefficient<-0
        }
        
        PlanningRegionCoefficient<-round(PlanningRegionCoefficient,0)
        
        PlanningRegionCoefficient<-prettyNum(PlanningRegionCoefficient,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( PlanningRegionCoefficient), h4(input$PlanningRegionMLR),
          icon = NULL, color = "olive")
      })
      
      output$TenureCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        Tenure=paste('Tenure',input$tenureMLR,sep='')
        ColforTenure<-match(Tenure,coefficientnames)
        TenureCoefficient=prettyNum(round(unname(summaryMLR)[ColforTenure],0),big.mark=",",scientific=FALSE)
        
        
        if(input$tenureMLR=="99 Yrs Leasehold"){
          TenureCoefficient<-0
        }

        TenureCoefficient<-prettyNum(TenureCoefficient,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0("+", TenureCoefficient), h4(input$tenureMLR),
          icon = NULL, color = "olive")
      })
      
      output$SalesTypeCoefficient <- renderValueBox({
        
        realisMLR<-realis
        realisMLR$PlanningRegion=as.factor(realisMLR$PlanningRegion)
        realisMLR$Tenure=as.factor(realisMLR$Tenure)
        realisMLR$SaleType=as.factor(realisMLR$SaleType)
        realisMLR$Type=as.factor(realisMLR$Type)
        realisMLR$PropertyType=as.factor(realisMLR$PropertyType)
        
        
        #Final Model
        model=lm(PriceperUnit ~ AreaSQM + AgeOfProperty + Floor + NoofStopsAwayFromCityCentre  + ChildcareDistance_m + MrtDist + Tenure + PlanningRegion + SaleType
                 , data=realisMLR)
        
        summaryMLR<-summary(model)$coefficients[,1]
        
        coefficientnames<-names(summaryMLR)
        
        SaleType=paste('SaleType',input$SaleTypeMLR,sep='')
        ColforSaleType<-match(SaleType,coefficientnames)
        SaleTypeCoefficient=unname(summaryMLR)[ColforSaleType]
        
        if(input$SaleTypeMLR=="New Sale"){
          SaleTypeCoefficient<-0
        }
        
        SaleTypeCoefficient<-round(SaleTypeCoefficient,0)
        SaleTypeCoefficient<-prettyNum(SaleTypeCoefficient,big.mark=",",scientific=FALSE)
        
        valueBox(
          paste0( "+",SaleTypeCoefficient), h4(input$SaleTypeMLR),
          icon = NULL, color = "olive")
      })
      
      
      

})
