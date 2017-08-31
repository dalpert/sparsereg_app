ui = navbarPage('',theme = shinytheme("cerulean"),
                
    # Data upload
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 #checkboxInput("default.button", label = "Use default data"),
                 awesomeCheckbox("default.button", label = strong("Use default data")),
                 
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 
                 hidden(
                   fileInput('file1', 'Choose CSV File',
                             accept=c('text/csv',
                                      'text/comma-separated-values,text/plain',
                                      '.csv')),
                   
                   # added interface for uploading data from
                   # http://shiny.rstudio.com/gallery/file-upload.html
                   tags$br(),
                   awesomeCheckbox('header', 'Header', TRUE),
                   awesomeRadio('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ','),
                   awesomeRadio('quote', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"')
                 )
               ),
               mainPanel(
                 tableOutput('data.table')
               )
             )
             
    ),
    
    
    ### Linear regression
    tabPanel(title = 'Linear Regression',
             titlePanel('Linear Regression'),
             sidebarLayout(position = 'right',
                           mainPanel(h3('Model'), 
                                     textOutput('lin.vars'),
                                     verbatimTextOutput('lin.model.summary'),
                                     # For diagnostic plot tabs after the model is fitted--rendered in 'server.R'
                                     uiOutput('lin.tabs')
                           ),
                           sidebarPanel(h3('Data'),
                                        # Inputs and run button
                                        selectInput('lin.yvar', 'Select response', ''),
                                        selectInput('lin.xvars', 'Select covariates', '', selected = '', multiple = TRUE),
                                        actionButton('lin.analysis', label = 'Run', class = 'btn-primary')
                           )
             )
    ),
    
    ### IV regression (2SLS)
    tabPanel(title = '2SLS',
             titlePanel('Two-Stage Least Squares'),
             sidebarLayout(position = 'right',
                           mainPanel(h3('Model'), 
                                     textOutput('iv.vars'),
                                     verbatimTextOutput('iv.model.summary')
                           ),
                           sidebarPanel(h3('Data'),
                                        # Inputs (response, endogenous, inst, covariates) and run button
                                        selectInput('iv.yvar', 'Select response', ''),
                                        selectInput('iv.endog', 'Select endogenous variable', ''),
                                        selectInput('iv.inst', 'Select instrument', ''),
                                        selectInput('iv.xvars', 'Select covariates', '', selected = '', multiple = TRUE),
                                        actionButton('iv.analysis', label = 'Run', class = 'btn-primary')
                           )
             )
    ),
    
    ### sparsereg
    tabPanel(title = 'sparsereg',
             titlePanel('Sparsereg'),
             sidebarLayout(position = 'right',
                           mainPanel(h3("Model"), 
                                     shinyjs::useShinyjs(),
                                     textOutput('sp.txt'),
                                     tableOutput('sp.data.table'),
                                     verbatimTextOutput('sp.model.summary'),
                                     # For diagnostic plot tabs after the model is fitted--rendered in 'server.R' 
                                     uiOutput('sp.tabs')
                           ),
                           sidebarPanel(h3('Data'),
                                        # Inputs (response, treatment, covariates)
                                        selectInput('sp.yvar', 'Select response', ''),
                                        selectInput('sp.treat', 'Select treatment', ''),
                                        selectInput('sp.xvars', 'Select covariates', '', selected = '', multiple = TRUE),
                                        # EM choice
                                        ## A small note on bsTooltop: if inputId has a '.' in it, it will not work, 
                                        ## if the text is broken up into multiple lines it also won't work
                                        awesomeRadio('spem', label = 'EM:', choices = list(TRUE, FALSE), selected = TRUE),
                                        # Allows text to appear after hovering over the radio buttons
                                        # bsTooltip explanations pulled from sparsereg help page
                                        bsTooltip('spem', 'Whether to fit model via EM or MCMC. EM is much quicker, but only returns point estimates. MCMC is slower, but returns posterior intervals and approximate confidence intervals.',
                                                  'right', options = list(container = 'body')),
                                        # Choice of scale type
                                        awesomeRadio('spscale', label = 'Scale type:',
                                                     choices = list('none', 'TX', 'TT', 'TTX'),
                                                     selected = 'none'
                                        ),
                                        bsTooltip('spscale', 'Indicates the types of interactions that will be created and used in estimation. scale.type="none" generates no interactions and corresponds to simply running LASSOplus with no interactions between variables. scale.type="TX" creates interactions between each X variable and each level of the treatment variables. scale.type="TT" creates interactions between each level of separate treatment variables. scale.type="TTX" interacts each X variable with all values generated by scale.type="TT". Note that users can create their own interactions of interest, select scale.type="none", to return the sparse version of the user specified model.',
                                                  'right', options = list(container = 'body')),
                                        # Run button
                                        actionButton('sp.analysis', label = 'Run', class = 'btn-primary'),
                                        # CSS to create a progress bar while the program is running--not the best way to do this but it works for now
                                        tags$head(tags$style(type='text/css', "
                                                             #loadmessage-sp {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 55px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: normal;
                                                             font-size: 100%;
                                                             color: #FFFFFF;
                                                             background-color: #0A369D;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        # Displays the above banner while there is R code running behind the scenes
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("Sparsereg is running...",id="loadmessage-sp"))
                                        )
                                        )
             ),
    
    ### sparseregIV
    tabPanel(title = 'sparseregIV',
             titlePanel('Sparsereg IV'),
             sidebarLayout(position = 'right',
                           mainPanel(h3('Model'),
                                     # Two models are going on at once in this tab, one must always be hidden while the other shows
                                     hidden(tableOutput('spiv.data.table')),
                                     tableOutput('spiv2.data.table'),
                                     hidden(verbatimTextOutput('spiv.model.summary')),
                                     verbatimTextOutput('spiv2.model.summary'),
                                     # For diagnostic plot tabs after the model is fitted, two sets of tabs present
                                     hidden(uiOutput('spiv.tabs')),
                                     uiOutput('spiv2.tabs')
                           ),
                           sidebarPanel(h3('Data'),
                                        # Inputs (response, endogenous, inst, covariates)
                                        selectInput('spiv.yvar', 'Select response', ''),
                                        selectInput('spiv.endog', 'Select endogenous variable', ''),
                                        # Checkbox to choose a two instrument model
                                        awesomeCheckbox("num.insts", label = "Use two instruments", value = FALSE),
                                        selectInput('spiv.inst', 'Select instrument', ''),
                                        # Second instrument, only shows up if 'num.insts' is TRUE
                                        selectInput('spiv.inst.2', 'Select second instrument', ''),
                                        selectInput('spiv.xvars', 'Select covariates', '', selected = '', multiple = TRUE),
                                        # Number of bootstraps
                                        sliderInput('spivmultboot', 'Number of bootstraps', min = 1, max = 20, value = 3),
                                        bsTooltip('spivmultboot', 'Number of bootstrap samples to use during a two-instrument sparseregIV regression. Not applicable for one instrument case. The defauly is 20, but more than 3 takes very long to run.',
                                                  'right', options = list(container = 'body')),
                                        # Two run buttons--run totally different models
                                        actionButton('spiv2.analysis', label = 'Run', class = 'btn-primary'),
                                        hidden(actionButton('spiv.analysis', label = 'Run', class = 'btn-primary')),
                                        # CSS to create a progress bar while the program is running
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage-spiv {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 55px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: normal;
                                                             font-size: 100%;
                                                             color: #FFFFFF;
                                                             background-color: #0A369D;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        # Displays the above banner while there is R code running behind the scenes
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("SparseregIV is running...",id="loadmessage-spiv"))
                                        )
                                        )
             ),
    
    ### sparseregTE
    tabPanel(title = "sparseregTE",
             titlePanel("Sparsereg TE"),
             
             sidebarLayout(position = "right",
                           mainPanel(h3("Model"), 
                                     textOutput('te.txt'),
                                     #textOutput('te.vars'),
                                     tableOutput(outputId = 'te.table'),
                                     #textOutput('s.em.text'),
                                     verbatimTextOutput('te.model.summary'),
                                     
                                     uiOutput('te.tabs')
                                     
                           ),
                           
                           sidebarPanel(h3("Data"),
                                        # Response
                                        selectInput('te.yvar', 'Select response', ""),
                                        # Treatment
                                        selectInput('te.treat', 'Select treatment', ""),
                                        # Covariates
                                        selectInput('te.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("te.analysis","Run", class = "btn-primary"),
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage-te {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 55px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: normal;
                                                             font-size: 100%;
                                                             color: #FFFFFF;
                                                             background-color: #0A369D;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("SparseregTE is running...",id="loadmessage-te"))
                                        
                                        )
                           
                           )
                           ),
    
    # sparseregNP
    tabPanel(title = "sparseregNP",
             titlePanel("Sparsereg NP"),
             
             sidebarLayout(position = "right",
                           mainPanel(h3("Model"), 
                                     textOutput('np.txt'),
                                     #textOutput('te.vars'),
                                     tableOutput(outputId = 'np.table'),
                                     #textOutput('s.em.text'),
                                     verbatimTextOutput('np.model.summary')
                                     
                                     #actionButton('plotratio',"Plot ratio"),
                                     #plotOutput('corr')
                                     
                           ),
                           
                           sidebarPanel(h3("Data"),
                                        # Response
                                        selectInput('np.yvar', 'Select response', ""),
                                        # Covariates
                                        selectInput('np.xvars', 'Select covariates', "", 
                                                    selected = "", multiple = TRUE),
                                        # Run button
                                        actionButton("np.analysis","Run", class = "btn-primary"),
                                        tags$head(tags$style(type="text/css", "
                                                             #loadmessage-np {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 55px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: normal;
                                                             font-size: 100%;
                                                             color: #FFFFFF;
                                                             background-color: #0A369D;
                                                             z-index: 105;
                                                             }
                                                             ")),
                                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                         tags$div("SparseregNP is running...",id="loadmessage-np"))
                                        
                                        )
                           
                           )
                           )
    
             )