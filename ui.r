ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Democratizing Real Estate Investments", titleWidth = 400),
                    dashboardSidebar(width = 400,
                                     sidebarMenu(id = "sbm",
                                                 menuItem("Dashboard", tabName = "dashboard", icon = NULL),
                                                 menuItem("Value Analyser", tabName = "valueAnalysis", icon = NULL),
                                                 conditionalPanel(
                                                       condition = "input.sbm == 'valueAnalysis'",
                                                       uiOutput("areaUI"),
                                                       uiOutput("tenureUI"),
                                                       sliderInput(
                                                             "propertAge",
                                                             label = "Age of Property:",
                                                             min = 0,
                                                             max = 99,
                                                             value = c(0, 30)
                                                       ),
                                                       
                                                       sliderInput(
                                                             "childcareDist", 
                                                             label = "Distance from nearest childcare centre (in metres):", 
                                                             min = 0, 
                                                             max = 5000, 
                                                             value = c(0, 1000)
                                                       ),
                                                       
                                                       sliderInput(
                                                             "mrtDist", 
                                                             label = "Distance from nearest MRT station (in metres):", 
                                                             min = 0, 
                                                             max = 5000, 
                                                             value = c(0, 1000)
                                                       )
                                                 ),
                                                 menuItem("Forecast Models", tabName = "forecastModels", icon = NULL)
                                                 )
                                     ),
                    dashboardBody(
                          tabItems(
                                tabItem(tabName = "dashboard",
                                        fluidPage(
                                              fluidRow(
                                                    column(width = 12,
                                                           valueBoxOutput("median2018Box", width = 3),
                                                           valueBoxOutput("annualizedChangeInMedianBox", width =4)
                                                    )
                                              ),
                                              
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 title = "Top 10 Areas by Annualized Growth Rate",
                                                                 solidHeader = TRUE,
                                                                 width = 7,
                                                                 height = 650,
                                                                 collapsible = TRUE,
                                                                 showOutput("top10PlanningArea", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              )#end of fluidRow
                                        )#end of fluidPage
                                ),#end of tabItem "dashboard"
                                
                                tabItem(tabName = "valueAnalysis",
                                        fluidPage(
                                              fluidRow(
                                                    column(width = 4,
                                                           box(
                                                                 h1("Value Analysis"),
                                                                 width = 12,
                                                                 height = 160,
                                                                 background = "navy",
                                                                 solidHeader = FALSE,
                                                                 collapsible = FALSE,
                                                                 collapsed = FALSE,
                                                                 h4("Please select criteria using the options on the sidebar."),
                                                                 actionButton("analyze", label = "Analyze") 
                                                           )
                                                    )
                                              ),
                                              
                                              conditionalPanel(
                                                    condition = "input.analyze",
                                                    column(width = 12,
                                                           box(
                                                                 title = "Median Transaction Value ($) Across Time",
                                                                 solidHeader = TRUE,
                                                                 width = 7,
                                                                 height = 650,
                                                                 collapsible = TRUE,
                                                                 showOutput("medPriceTS", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              )#end of fluidRow
                                        )#end of fluidPage
                                )#end of tabItem "valueAnalysis"
                          )#end of tabItems
                    )#end of dashboardBody
)#end of dashboardPage
                                              
                        
      
                          
  
