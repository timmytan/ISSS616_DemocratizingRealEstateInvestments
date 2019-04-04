ui <- dashboardPage(skin = "purple",
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
                                                           valueBoxOutput("median2018Box", width =3),
                                                           valueBoxOutput("annualizedChangeInMedianBox", width =3),
                                                           box(title = h2("Output 3"), width = 3, height = 100, background = "green"),
                                                           box(title = h2("Output 4"), width = 3, height = 100, background = "yellow")
                                                    )
                                              ),
                                              
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 h2("ISSS616/AY2018-19T2/G6"),
                                                                 h3("Democratizing Real Estate Investments"),
                                                                 width = 4,
                                                                 height = 500,
                                                                 background = "navy",
                                                                 solidHeader = FALSE,
                                                                 collapsible = FALSE,
                                                                 collapsed = FALSE,
                                                                 h4("Our app aims to make real estate analysis a more simple and efficient process by levelling the playing field between you - the retail investor - and institutional investors."),
                                                                 h4("Through the app, you have access to a set of sophisticated tools that are used by property developers to make informed decisions in your real estate negotiations."),
                                                                 h4("On this dashboard, we make use of descriptive statistical techniques to: "),
                                                                 h4(tags$ul(
                                                                       tags$li("Provide a dynamic heat-map of property prices across all regions in Singapore, featuring measures of central tendency and transactional volume."),
                                                                       tags$li("Provide a macro-overview of property prices in Singapore by featuring the top 10 regions with highest growth rate as well as median prices across a five year time-frame.")
                                                                       )
                                                                 )
                                                           ),
                                                           
                                                           box(
                                                                 title = "Heat Map based on Transaction Volume",
                                                                 width = 8,
                                                                 height = 500,
                                                                 solidHeader = TRUE,
                                                                 collapsible = FALSE,
                                                                 collapsed = FALSE
                                                           )#end of box
                                                    )#end of column
                                              ),#end of fluidRow
                                              
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 title = "Top 10 Areas by Annualized Growth Rate",
                                                                 solidHeader = TRUE,
                                                                 width = 12,
                                                                 height = 350,
                                                                 collapsible = TRUE,
                                                                 showOutput("top10PlanningArea", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              ),#end of fluidRow
                                              
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 title = "Top 10 Areas - Annual Price Change (PSF)",
                                                                 solidHeader = TRUE,
                                                                 width = 12,
                                                                 height = 600,
                                                                 collapsible = TRUE,
                                                                 showOutput("top10TS", "nvd3", package = "rCharts")
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
                                              
                        
      
                          
  
