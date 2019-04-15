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
                                                             "propertyAge",
                                                             label = "Age of Property:",
                                                             min = 0,
                                                             max = 99,
                                                             value = c(0, 30)
                                                       ),
                                                       
                                                       sliderInput(
                                                             "childcareDist", 
                                                             label = "Distance from nearest childcare centre (in metres):", 
                                                             min = 0, 
                                                             max = 2500, 
                                                             value = c(0, 1000)
                                                       ),
                                                       
                                                       sliderInput(
                                                             "mrtDist", 
                                                             label = "Distance from nearest MRT station (in metres):", 
                                                             min = 0, 
                                                             max = 3500, 
                                                             value = c(0, 1000)
                                                       )
                                                 ), #end of conditional panel for menuItem: value analyzer
                                                 
                                                 
                                                 
                                                 menuItem("Price Estimate", tabName = "PriceEstimate", icon = NULL),
                                                 conditionalPanel(
                                                   condition = "input.sbm == 'PriceEstimate'",
                                                   uiOutput("RegionUIMLR"),
                                                   uiOutput("tenureUIMLR"),
                                                   uiOutput("saletypeUIMLR"),
                                                   sliderInput(
                                                     "FloorLevel",
                                                     label = "Floor Level:",
                                                     min = 0,
                                                     max = 100,
                                                     value = c(33,66)
                                                   ),
                                                   sliderInput(
                                                     "AreaSQM",
                                                     label = "Area (Square Meter):",
                                                     min = 0,
                                                     max = 300,
                                                     value = c(100,200)
                                                   ),
                                                   sliderInput(
                                                     "propertyAgeMLR",
                                                     label = "Age of Property:",
                                                     min = 0,
                                                     max = 50,
                                                     value = c(15,25)
                                                   ),
                                                   sliderInput(
                                                     "NoofStopsAwayFromCityCentre",
                                                     label = "No of Stops Away From City Centre:",
                                                     min = 0,
                                                     max = 40,
                                                     value = c(10,15)
                                                   ),
                                                   sliderInput(
                                                     "ChildcareDistanceMLR",
                                                     label = "Distance from nearest Childcare Centre:",
                                                     min = 0,
                                                     max = 1000,
                                                     value = c(333,666)
                                                   ),
                                                   sliderInput(
                                                     "MrtDistMLR",
                                                     label = "Distance from nearest MRT:",
                                                     min = 0,
                                                     max = 1500,
                                                     value = c(500,1000)
                                                   )
                                                 ),#end of conditional panel for menuItem: Price Estimate
                                                 
                                                 
                                                 menuItem("Confidence Interval Test", tabName = "confidenceintervaltest", icon = NULL),
                                                 conditionalPanel(
                                                       condition = "input.sbm == 'confidenceintervaltest'",
                                                       uiOutput("areaUICI_1"),
                                                       uiOutput("areaUICI_2"),
                                                       fluidRow(
                                                             column(6, numericInput(inputId = "conf_level",
                                                                                    label=strong("Confidence Level"),
                                                                                    value=0.95, min=0.1, max=0.99, step=0.01))),
                                                       actionButton("analyzeCI", label = "Analyze")
                                                 ) #end of conditional panel for menuItem: confidence interval test
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
                                                                 collapsed = FALSE,
                                                                 leafletOutput("map", width="900px", height="450px")
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
                                                                       tags$li("Provide price trend of selected planning region based on parameter inputs like tenure, year of completion, property age, distance to nearest MRT and childcare centre.")
                                                                       
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
                                                                 height = 450,
                                                                 collapsible = TRUE,
                                                                 showOutput("medPriceTS", "nvd3", package = "rCharts")
                                                           ),#end of box
                                                           
                                                           box(
                                                                 title = "Mean Price (PSF) Across Time",
                                                                 solidHeader = TRUE,
                                                                 width = 6,
                                                                 height = 450,
                                                                 collapsible = TRUE,
                                                                 showOutput("meanPsfTS", "nvd3", package = "rCharts")
                                                           )#end of box
                                                    )#end of column
                                              )# end of conditional panel
                                              )#end of fluidRow
                                        )#end of fluidPage
                                ),#end of tabItem "valueAnalysis"
                                tabItem(tabName = "PriceEstimate",
                                        fluidPage(
                                              fluidRow(
                                                    column(width = 12,
                                                           box(
                                                                 h1("Price Estimate"),
                                                                 h4("The Price Estimator page aims to provide you with a forecasted price of the selected planning region based on a multiple linear regression model. 
                                                                    "),
                                                                 h4(tags$ul(
                                                                       tags$li("Provide price trend of selected planning region based on parameter inputs like tenure, year of completion, property age, distance to nearest MRT and childcare centre."),
                                                                       tags$li("A point estimate forecast and a 95% confidence interval will be derived. This means that there is a 95% chance that the forecasted price falls within this range.
                                                                               ")
                                                                       )
                                                                 ),
                                                                 width = 12,
                                                                 height = 250,
                                                                 background = "navy",
                                                                 solidHeader = FALSE,
                                                                 collapsible = FALSE,
                                                                 collapsed = FALSE,
                                                                 h4("Please select criteria using the options on the sidebar."),
                                                                 actionButton("analyzeMLR", label = "Analyze"), 
                                                                 actionButton("ModelEq", label = "Model Equation")
                                                                 
                                                                 )
                                                           )#end of column
                                                    ),#end of fluidRow
                                              
                                              conditionalPanel(
                                                    condition = "input.analyzeMLR && input.analyzeMLR%2==1",
                                                    fluidRow(
                                                          column(width = 12,
                                                                 valueBoxOutput("MLR4", width =12)
                                                          )#end of column
                                                    ),#end of fluid row
                                                    fluidRow(
                                                          column(width = 12,
                                                                 box(
                                                                       h3("You are measuring a property with the following traits:"),
                                                                       h4(htmlOutput("MLR")
                                                                       ),
                                                                       width=12,
                                                                       height=360,
                                                                       background = "light-blue"
                                                                 )#end of box
                                                          )#end of column
                                                    )#end of fluid row
                                              ), # end of conditional panel
                                              
                                                           
                                
                                            conditionalPanel(
                                                  condition = "input.ModelEq && input.ModelEq%2==1",
                                                  fluidRow(
                                                        column(width = 12,
                                                               box(
                                                                     h1("Model Equation:"),
                                                                     width=3,
                                                                     height=80,
                                                                     background = "orange"
                                                               )
                                                        )#end of column
                                                  ),#end of fluid row
                                                  fluidRow(
                                                        column(width = 12,
                                                               valueBoxOutput("InterceptCoefficient", width =3),
                                                               valueBoxOutput("AreaCoefficient", width =3),
                                                               valueBoxOutput("PropertyAgeCoefficient", width =3),
                                                               valueBoxOutput("FloorCoefficient", width =3)
                                                        )#end of column
                                                  ),#end of fluid row
                                                  fluidRow(
                                                        column(width = 12,
                                                               valueBoxOutput("StopsfromCityCentreCoefficient", width =3),
                                                               valueBoxOutput("ChilcareDistCoefficient", width =3),
                                                               valueBoxOutput("MrtDistCoefficient", width =3),
                                                               valueBoxOutput("PlanningRegionCoefficient", width =3)
                                                               
                                                        )
                                                  ),
                                                  fluidRow(
                                                        column(width = 12,
                                                               valueBoxOutput("TenureCoefficient", width =3),
                                                               valueBoxOutput("SalesTypeCoefficient", width =3)
                                                        )
                                                  )
                                            )# end of conditional panel
                                            )#end of fluidPage
                                      ),#end of tabItem "PriceEstimate"
                                
                                tabItem(tabName = "confidenceintervaltest",
                                        fluidPage(
                                          fluidRow(
                                            
                                            column(width = 12,
                                                   box(
                                                     h1("ANOVA Test"),
                                                     h4("The ANOVA Test aims to assist you in making an informed choice of your property purchase. The page uses inferential statistical techniques to: "),
                                                     h4(tags$ul(
                                                       tags$li("Inform if the Mean Price PSF in the 2 selected areas are significantly different based on a selected confidence level.")
                                                     )
                                                     ),
                                                     width = 12,
                                                     height = 250,
                                                     background = "navy",
                                                     solidHeader = FALSE,
                                                     collapsible = FALSE,
                                                     collapsed = FALSE,
                                                     h4("Please select criteria using the options on the sidebar."),
                                                     h4("H0: Mean price (PSF) in Planning Area 1 is EQUAL to mean price (PSF) in Planning Area 2"),
                                                     h4("H1: Mean price (PSF) in Planning Area 1 is NOT EQUAL to mean price (PSF) in Planning Area 2")
                                                   ),#end of box
                                                   conditionalPanel(
                                                     condition = "input.analyzeCI",
                                                     box(
                                                       h3("Conclusion of Mean Price PSF Comparison"),
                                                       h4(htmlOutput("CItest")),
                                                       width=8,
                                                       height=250,
                                                       background = "light-blue"
                                                     ),#end of box
                                                     box(
                                                       title=h3("BoxPlot of Mean Price PSF for Selected Planning Area"),
                                                       plotOutput("CIplot"),
                                                       width=6,
                                                       height=500
                                                     ),
                                                     box(
                                                       title=h3("Confidence Intervals of Mean Price PSF Difference"),
                                                       plotOutput("CIplot2"),
                                                       width=6,
                                                       height=500
                                                     )
                                                   )#end of panel
                                            )#end of column
                                          )#end of fluidrow
                                        )#end of fluidpage
                                )#end of tabitem "confidenceintervaltest"
                                
                                
                                )#end of tabItems
                    )#end of dashboardBody
)#end of dashboardPage
                                              
                        
      
                          
  
