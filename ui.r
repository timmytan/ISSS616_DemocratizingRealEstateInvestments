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
                                                           valueBoxOutput("volume2018Box", width =3),
                                                           valueBoxOutput("meanPSF2018Box", width =3)
                                                    )
                                              ),
                                              
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 h2("ISSS616/AY2018-19T2/G6"),
                                                                 h3("Democratizing Real Estate Investments"),
                                                                 width = 4,
                                                                 height = 510,
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
                                                                 title = "Heat Map based on Transaction Volume in 2018",
                                                                 width = 8,
                                                                 height = 510,
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
                                                                 collapsible = FALSE,
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
                                                                 collapsible = FALSE,
                                                                 showOutput("top10TS", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              )#end of fluidRow
                                        )#end of fluidPage
                                ),#end of tabItem "dashboard"
                                
                                tabItem(tabName = "valueAnalysis",
                                        fluidPage(
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 h1("Value Analysis"),
                                                                 h4("The Value Analysis page aims to assist you in making an informed choice of your property purchase. The page uses inferential statistical techniques to: "),
                                                                 h4(tags$ul(
                                                                       tags$li("Provide price trend of selected planning region based on parameter inputs like tenure, year of completion, property age, distance to nearest MRT and childcare centre."),
                                                                       tags$li("Provide comparative options based on similar price ranges. Back-end, we have tested that the price mean for these options are statistically similar through the ANOVA method.")
                                                                 )
                                                                 ),
                                                                 width = 12,
                                                                 height = 250,
                                                                 background = "navy",
                                                                 solidHeader = FALSE,
                                                                 collapsible = FALSE,
                                                                 collapsed = FALSE,
                                                                 h4("Please select criteria using the options on the sidebar."),
                                                                 actionButton("analyze", label = "Analyze") 
                                                           )
                                                    ),
                                              
                                              conditionalPanel(
                                                    condition = "input.analyze",
                                                    column(width = 12,
                                                           box(
                                                                 title = "Median Transaction Value ($) Across Time",
                                                                 solidHeader = TRUE,
                                                                 width = 6,
                                                                 height = 500,
                                                                 collapsible = TRUE,
                                                                 showOutput("medPriceTS", "nvd3", package = "rCharts")
                                                           ),#end of box
                                                           
                                                           box(
                                                                 title = "Mean Price (PSF) Across Time",
                                                                 solidHeader = TRUE,
                                                                 width = 6,
                                                                 height = 500,
                                                                 collapsible = TRUE,
                                                                 showOutput("meanPsfTS", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              )# end of conditional panel
                                              )#end of fluidRow
                                        )#end of fluidPage
                                )#end of tabItem "valueAnalysis"
                          )#end of tabItems
                    )#end of dashboardBody
)#end of dashboardPage
                                              
                        
      
                          
  
