ui <- tagList(

  #require(shiny)
  #require(visNetwork)

  # adjust menu based on screen size
  tags$head(tags$style(HTML("

      @media screen and (max-width: 500px) {
        #demox {
          width: 60%;
        }

      }
      "))),

  navbarPage('webDART', id = 'nav', inverse = TRUE, collapsible = FALSE,position = 'fixed-bottom',


             tabPanel("Interactive Map",
                      div(class="outer",  style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",value = 'mapPanel',
                          #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

                          # Map
                          leafletOutput("mymap", width = "100%", height = "100%"),

                          # Menu
                          absolutePanel(
                            style = "opacity: 0.92", draggable= TRUE, top = 10, left = 50,  width = 500,
                            HTML('<button data-toggle="collapse" data-target="#demox">Menu</button>'),
                            tags$div(id ='demox', class ="collapse", style = "overflow-y:scroll; max-height: 500px",

                                     wellPanel(

                                       actionButton("runDart", "Run Dart"),# actionButton('analyze', 'Analyze'),
                                       sliderInput("targetRadius", "Target Area Radius (m)", min = 0, max = 300, step = 30, value = 30),
                                       sliderInput("searchRadius", "Search Area Radius (m)", min = 500, max = 6000, step = 500, value = 3000),
                                       sliderInput("buffer", "Buffer Around Target Area (m)", min = 0, max = 300, step = 30, value = 60),
                                       sliderInput("nControl", "Number of Reference Pixels to Return", min = 1, max = 300, step = 1, value = 100),
                                       sliderInput("dDate", "Year of First Disturbance (For Synthetic Control)", sep = '', min = 1991, max = 2018, step = 1, value = 1991)

                                     )
                            )
                          ),


                          # Plot Panel
                          # conditionalPanel(style = "opacity: 0.92 bottom:0, right:'auto' width:'90%' height:300",
                          # condition = "input.analyze!==0",
                          # icon('hand-point-down', class = "fa-9x", lib = "font-awesome"),
                          # tags$h3("Plots"),
                          # plotOutput("myplot", click = 'plot_click', height = 400, width = 900)
                          # ),

                          # Debug window
                          if(.debugMessage){
                            # Messages
                            absolutePanel(
                              style = "opacity: 0.92", draggable = TRUE, bottom = 10, right = 50,  width = 150,
                              HTML('<button data-toggle="collapse" data-target="#demoz">Message</button>'),
                              tags$div(id='demoz', class="collapse" ,
                                       verbatimTextOutput('mymessage')
                              )
                            )
                          }

                      )
                      #   )
             ),


             # Plot Panel

             tabPanel("Analysis", value = 'analysisPanel',
                      #div(class="outer",  style = "position: relative; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",
                      tabsetPanel(
                        tabPanel("Quantiles",plotOutput("myplot", height = 'auto')),
                        tabPanel("Synthetic Control", plotOutput("scPlot")),
                        tabPanel("Variable Importance", visNetworkOutput('cartPlot', height = 800))
                        #downloadButton("exportPDF", "Export Figures")
                      )
             ),

             # Export Panel

             tabPanel("Export", value = 'exportPanel',
                      div(class="outer",  style = "position:inherit; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",
                          sidebarPanel(
                            #selectInput('whatToExport', "What to Export:", choices = c('Export Dart Pixels (xlsx)', 'Export Report (pdf)')),
                            downloadButton("exportPDF", "Export Report (pdf)"),
                            downloadButton("exportXLS", "Export Dart Pixels (xlsx)")#,
                          )
                      )
             )

  )

)
