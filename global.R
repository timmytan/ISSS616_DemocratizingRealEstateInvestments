# Load packages
library(datasets)
library(rCharts)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(shiny)
library(shinydashboard)
library(svglite)
library(stringr)
library(zoo)
library(xts)

home <- getwd()

setwd("data")

# Load data
realis = read.csv("realis2014to2019.csv", header = TRUE, stringsAsFactors = FALSE)
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
realis$SaleDate <- as.Date(x = realis$SaleDate, "%d%B%Y")
realis$TenureStart <- as.Date(as.character(gsub(".*From", "", realis$Tenure)), "%d/%m/%y")
realis$Tenure <- gsub("\\From.*", "", realis$Tenure)
realis$TenureLength <- as.numeric(gsub("\\D+", "", realis$Tenure))
realis$Tenure

# splitting into years
year2014 <- realis[which(year(realis$SaleDate) == 2014),]
year2015 <- realis[which(year(realis$SaleDate) == 2015),]
year2016 <- realis[which(year(realis$SaleDate) == 2016),]
year2017 <- realis[which(year(realis$SaleDate) == 2017),]
year2018 <- realis[which(year(realis$SaleDate) == 2018),]


# Run the application 
shinyApp(ui = "ui.R", server = "server.R")
