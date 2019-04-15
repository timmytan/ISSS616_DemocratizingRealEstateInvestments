# Load packages
library(datasets)
library(rCharts)
library(lubridate)
library(plyr)
library(dplyr)
library(geojsonio)
library(ggplot2)
library(htmltools)
library(leaflet)
library(maps)
library(plotly)
library(reshape2)
library(rgdal)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(sp)
library(svglite)
library(stringr)
library(zoo)
library(xts)
library(DT)

home <- "C:/Users/Timothy/Documents/MITB Coursework/ISSS616 Applied Statistical Analysis with R/Project/Application/ISSS616_DemocratizingRealEstateInvestments/"

setwd("data")

# Load data
realis = read.csv("realis2014to2019_Final.csv", header = TRUE, stringsAsFactors = FALSE)
school = read.csv("Longtitude and Latitude of Popular Primary Schools.csv", header = TRUE, stringsAsFactors = FALSE)

setwd(home)

# global variables
dflt <- list(postalCode = "", 
             planningArea = "", 
             tenure = "", 
             minSchoolDist = "", 
             maxSchoolDist = "", 
             minChildcareDist = "", 
             maxChildcareDist = "", 
             minMrtDist = "", 
             maxMrtDist = "", 
             stringsAsFactors = FALSE)

# data cleaning
realis$SaleDate <- as.Date(x = realis$SaleDate, "%d-%B-%y")
realis$Tenure <- gsub("\\From.*", "", realis$Tenure)
realis$AgeOfProperty <- as.numeric(year(today())) - as.numeric(realis$CompletionDate)
realis$AgeOfProperty[is.na(realis$AgeOfProperty)] <- 0
realis$PriceperUnit<-realis$TransactedPrice/realis$NoOfUnits

# splitting into years
year2014 <- realis[which(year(realis$SaleDate) == 2014),]
year2015 <- realis[which(year(realis$SaleDate) == 2015),]
year2016 <- realis[which(year(realis$SaleDate) == 2016),]
year2017 <- realis[which(year(realis$SaleDate) == 2017),]
year2018 <- realis[which(year(realis$SaleDate) == 2018),]

###data mgmt for leaflet###
setwd("data")

#read geojson files
planning_area_map <- geojson_read("2-planning-area.geojson", what = "sp")

setwd(home)

#dplyr query
year2018_n_transaction <- year2018 %>% group_by(PlanningArea) %>% tally(NoOfUnits)
year2018_price <- year2018 %>% group_by(PlanningArea) %>% dplyr::summarize(TransactedPrice = mean(TransactedPrice/NoOfUnits, na.rm=TRUE))
year2018_max <- year2018 %>% group_by(PlanningArea) %>% dplyr::summarize(TransactedPrice = max(TransactedPrice/NoOfUnits, na.rm=TRUE))
year2018_median <- year2018 %>% group_by(PlanningArea) %>% dplyr::summarize(TransactedPrice = median(TransactedPrice/NoOfUnits, na.rm=TRUE))

#caps data to match with polygon
year2018_n_transaction$PlanningArea <- toupper(year2018_n_transaction$PlanningArea)
year2018_price$PlanningArea <- toupper(year2018_price$PlanningArea)
year2018_max$PlanningArea <- toupper(year2018_max$PlanningArea)
year2018_median$PlanningArea <- toupper(year2018_median$PlanningArea)

#append data to planning_area_map
planning_area_map@data <- left_join(planning_area_map@data, year2018_price, by=c("name"="PlanningArea"))
planning_area_map@data <- left_join(planning_area_map@data, year2018_n_transaction, by=c("name"="PlanningArea"))
planning_area_map@data <- left_join(planning_area_map@data, year2018_max, by=c("name"="PlanningArea"))
planning_area_map@data <- left_join(planning_area_map@data, year2018_median, by=c("name"="PlanningArea"))
colnames(planning_area_map@data) <- c("name","mean_price","n_transaction","max_price","median_price")

#replace all NA with 0 in data frame
planning_area_map@data[is.na(planning_area_map@data)] <-0

#divide price by 1million
planning_area_map@data$mean_price <- planning_area_map@data$mean_price/1000000
planning_area_map@data$max_price <- planning_area_map@data$max_price/1000000
planning_area_map@data$median_price <- planning_area_map@data$median_price/1000000

# Run the application 
shinyApp(ui = "ui.R", server = "server.R")
