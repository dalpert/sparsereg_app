ui = fluidPage(
  tabsetPanel( 
    
    # Data upload
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 #actionButton('preload.dat', label = 'Use preloaded data'),
                 
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    
    tabPanel("Data Exploration",
             titlePanel("Explore the data"),
             sidebarLayout(
               sidebarPanel(
                 
                 
                 
               ),
               mainPanel(
                 
               )
             )
    ),
    
    # Linear regression
    tabPanel(title = "Linear Regression",
             titlePanel("Linear Regression"),
             sidebarLayout(position = "right",
                           mainPanel(h2("Your Model"), 
                                     textOutput('txt'),
                                     textOutput('vars'),
                                     verbatimTextOutput("modelSummary"),
                                     uiOutput('diagnostic'),
                                     uiOutput('b.diag')
                           ),
                           sidebarPanel(h2("Your Data"),
                                        # Response
                                        selectInput('yvar', 'Select response', ""),
                                        # Covariates
                                        selectInput('xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("analysis","Run")
                           )
                           
             )
    ),
    
    # IV regression
    tabPanel(title = "2SLS",
             titlePanel("Two-Stage Least Squares"),
             sidebarLayout(position = "right",
                           mainPanel(h2("Your Model"), 
                                     textOutput('iv.txt'),
                                     textOutput('iv.vars'),
                                     verbatimTextOutput('iv.modelSummary')
                           ),
                           sidebarPanel(h2("Your Data"),
                                        # Response
                                        selectInput('iv.yvar', 'Select response', ""),
                                        # Endogenous variable
                                        selectInput('iv.endog', 'Select endogenous variable', ""),
                                        # Instrument
                                        selectInput('iv.inst', 'Select instrument', ""),
                                        # Covariates
                                        selectInput('iv.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("iv.analysis","Run")
                           )
                           
             )
    ),
    
    
    # sparsereg
    tabPanel(title = "sparsereg",
             titlePanel("Sparsereg"),
             sidebarLayout(position = "left",
                           sidebarPanel(h2("Your Data"),
                                        # Response
                                        selectInput('sp.yvar', 'Select response', ""),
                                        # Instrument
                                        selectInput('sp.treat', 'Select treatment', ""),
                                        # Covariates
                                        selectInput('sp.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        radioButtons(inputId = 'bins', "EM:",
                                                     choiceNames = list('TRUE', 'FALSE'),
                                                     choiceValues = list(1, 0),
                                                     selected = 1
                                        ),
                                        bsTooltip("bins", 'Whether to fit model via EM or MCMC. EM is much quicker, but only returns point estimates. MCMC is slower, but returns posterior intervals and approximate confidence intervals.',
                                                  "right", options = list(container = "body")),
                                        
                                        radioButtons(inputId = 'sc', "Scale type:",
                                                     choiceNames = list('none', 'TX', 'TT', 'TTX'),
                                                     choiceValues = list('none', 'TX', 'TT', 'TTX'),
                                                     selected = 'none'
                                        ),
                                        bsTooltip("sc", 'Indicates the types of interactions that will be created and used in estimation. scale.type="none" generates no interactions and corresponds to simply running LASSOplus with no interactions between variables. scale.type="TX" creates interactions between each X variable and each level of the treatment variables. scale.type="TT" creates interactions between each level of separate treatment variables. scale.type="TTX" interacts each X variable with all values generated by scale.type="TT". Note that users can create their own interactions of interest, select scale.type="none", to return the sparse version of the user specified model.',
                                                  "right", options = list(container = "body")),
                                        
                                        actionButton('sp.analysis',"Run"),
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 5px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: bold;
                                                             font-size: 100%;
                                                             color: #000000;
                                                             background-color: #CCFF66;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("Sparsereg is running...",id="loadmessage"))
                                        
                                        ),
                           mainPanel(h2("Your Model"), 
                                     shinyjs::useShinyjs(),
                                     
                                     textOutput('sp.txt'),
                                     
                                     textOutput('sp.progress'),
                                     
                                     #textOutput('s.vars'),
                                     tableOutput(outputId = 'sp.table'),
                                     #textOutput('wait'),
                                     #verbatimTextOutput('sp.progress'),
                                     verbatimTextOutput('sp.modelSummary'),
                                     
                                     uiOutput("sp.diag.button.1"),
                                     uiOutput("sp.plot.contain"),
                                     
                                     uiOutput("sp.diag.button.2"),
                                     uiOutput("sp.viol.contain")
                                     
                                     #actionButton('plotratio',"Plot ratio"),
                                     #plotOutput('corr')
                                     
                           )
                           
                           )
                           ),
    
    # sparseregIV
    tabPanel(title = "sparseregIV",
             titlePanel("Sparsereg IV"),
             
             sidebarLayout(position = "right",
                           mainPanel(h2("Your Model"), 
                                     textOutput('spiv.txt'),
                                     #textOutput('sp.vars'),
                                     tableOutput(outputId = 'table'),
                                     verbatimTextOutput('spiv.modelSummary'),
                                     
                                     uiOutput("spiv.plotly.txt"),
                                     uiOutput("spiv.plotly.color"),
                                     uiOutput("spiv.diag.button"),
                                     
                                     plotlyOutput("IVplot")
                                     
                                     #actionButton('plotratio',"Plot ratio"),
                                     #plotOutput('corr')
                                     
                           ),
                           sidebarPanel(h2("Your Data"),
                                        # Response
                                        selectInput('spiv.yvar', 'Select response', ""),
                                        # Endogenous variable
                                        selectInput('spiv.endog', 'Select endogenous variable', ""),
                                        # Instrument
                                        selectInput('spiv.inst', 'Select instrument', ""),
                                        # Covariates
                                        selectInput('spiv.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("spiv.analysis","Run"),
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 5px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: bold;
                                                             font-size: 100%;
                                                             color: #000000;
                                                             background-color: #CCFF66;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("SparseregIV is running...",id="loadmessage"))
                                        )
                           
                           )
                           ),
    
    # sparseregTE
    tabPanel(title = "sparseregTE",
             titlePanel("Sparsereg TE"),
             
             sidebarLayout(position = "right",
                           mainPanel(h2("Your Model"), 
                                     textOutput('te.txt'),
                                     #textOutput('te.vars'),
                                     tableOutput(outputId = 'te.table'),
                                     #textOutput('s.em.text'),
                                     verbatimTextOutput('te.modelSummary')
                                     
                                     #actionButton('plotratio',"Plot ratio"),
                                     #plotOutput('corr')
                                     
                           ),
                           
                           sidebarPanel(h2("Your Data"),
                                        # Response
                                        selectInput('te.yvar', 'Select response', ""),
                                        # Treatment
                                        selectInput('te.treat', 'Select treatment', ""),
                                        # Covariates
                                        selectInput('te.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("te.analysis","Run"),
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 5px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: bold;
                                                             font-size: 100%;
                                                             color: #000000;
                                                             background-color: #CCFF66;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("SparseregTE is running...",id="loadmessage"))
                                        
                                        )
                           
                           )
                           )
    
             )
  
  
  
             )