library(sparsereg)
library(AER)
library(shiny)
library(shinyBS)
library(shinyjs)
library(plotly)
data(mtcars)


runApp(
  list(
    
    ui = fluidPage(
      tabsetPanel( 
        
        # Data upload
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 
                 sidebarLayout(
                   sidebarPanel(

                     #checkboxInput("default.button", label = "Use default data"),
                     awesomeCheckbox("default.button", label = "Use default data"),
                     
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
        
        # tabPanel("Data Exploration",
        #          titlePanel("Explore the data"),
        #          sidebarLayout(
        #            sidebarPanel(
        #              selectInput('graph.type', 'Select plot', 
        #                          choices = c('Histogram', 'Scatter')),
        #              uiOutput("hist.x"),
        #              uiOutput("hist.plot"),
        #              uiOutput("scat.x"),
        #              uiOutput("scat.y"),
        #              uiOutput("scat.plot")
        #              
        #            ),
        #            mainPanel(
        #              
        #            )
        #          )
        # ),
        
        # Linear regression
        tabPanel(title = "Linear Regression",
                 titlePanel("Linear Regression"),
                 sidebarLayout(position = "right",
                               mainPanel(h2("Your Model"), 
                                        #textOutput('lin.txt'),
                                        textOutput('lin.vars'),
                                        verbatimTextOutput("lin.modelSummary"),
                                        tabsetPanel(
                                          # tabPanel('Model summary',
                                          #     textOutput('lin.txt'),
                                          #     textOutput('lin.vars'),
                                          #     verbatimTextOutput("lin.modelSummary")
                                          # ),
                                            tabPanel('Diagnostic plot',
                                                #uiOutput('diagnostic'),
                                                #uiOutput('b.diag')
                                                plotOutput('lin.plot')
                                            ),
                                            tabPanel('tab3')
                                        )
                               ),
                               sidebarPanel(h2("Your Data"),
                                            # Response
                                            selectInput('lin.yvar', 'Select response', ""),
                                            # Covariates
                                            selectInput('lin.xvars', 'Select covariates', "", 
                                                        selected = "", multiple = TRUE),
                                            # Run button
                                            actionButton("lin.analysis","Run", class = "btn-primary")
                               )
                               
                 )
        ),
        
        # IV regression
        tabPanel(title = "2SLS",
                 titlePanel("Two-Stage Least Squares"),
                 sidebarLayout(position = "right",
                               mainPanel(h2("Your Model"), 
                                  #textOutput('iv.txt'),
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
                                            actionButton("iv.analysis","Run", class = "btn-primary")
                               )
                               
                 )
        ),
        
        
        # sparsereg
        tabPanel(title = "sparsereg",
                 titlePanel("Sparsereg"),
                 sidebarLayout(position = 'right',
                               mainPanel(h2("Your Model"), 
                                         shinyjs::useShinyjs(),
                                         
                                         textOutput('sp.txt'),

                                         tableOutput(outputId = 'sp.data.table'),

                                         verbatimTextOutput('sp.modelSummary'),
                                         
                                         tabsetPanel(
                                           tabPanel('Plot',
                                               plotOutput('sp.plot')
                                           ),
                                           tabPanel('Violin plot',
                                               plotOutput('sp.viol.plot')
                                           )
                                         )
                               ),
                               sidebarPanel(h2("Your Data"),
                                            # Response
                                            selectInput('sp.yvar', 'Select response', ""),
                                            # Instrument
                                            selectInput('sp.treat', 'Select treatment', ""),
                                            # Covariates
                                            selectInput('sp.xvars', 'Select covariates', "", 
                                                        selected = "", multiple = TRUE),
                                            # radioButtons(inputId = 'bins', "EM:",
                                            #              choiceNames = list('TRUE', 'FALSE'),
                                            #              choiceValues = list(1, 0),
                                            #              selected = 1
                                            # ),
                                            awesomeRadio(inputId = 'bins', label = "EM:",
                                                         choices = list(TRUE, FALSE),
                                                         selected = TRUE
                                            ),
                                            bsTooltip("bins", 'Whether to fit model via EM or MCMC. EM is much quicker, but only returns point estimates. MCMC is slower, but returns posterior intervals and approximate confidence intervals.',
                                                      "right", options = list(container = "body")),
                                            
                                            # radioButtons(inputId = 'sc', "Scale type:",
                                            #              choiceNames = list('none', 'TX', 'TT', 'TTX'),
                                            #              choiceValues = list('none', 'TX', 'TT', 'TTX'),
                                            #              selected = 'none'
                                            # ),
                                            awesomeRadio(inputId = 'sc', label = "Scale type:",
                                                         choices = list('none', 'TX', 'TT', 'TTX'),
                                                         selected = 'none'
                                            ),
                                            
                                            bsTooltip("sc", 'Indicates the types of interactions that will be created and used in estimation. scale.type="none" generates no interactions and corresponds to simply running LASSOplus with no interactions between variables. scale.type="TX" creates interactions between each X variable and each level of the treatment variables. scale.type="TT" creates interactions between each level of separate treatment variables. scale.type="TTX" interacts each X variable with all values generated by scale.type="TT". Note that users can create their own interactions of interest, select scale.type="none", to return the sparse version of the user specified model.',
                                                      "right", options = list(container = "body")),
                                            
                                            actionButton('sp.analysis',"Run", class = "btn-primary"),
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
                                            
                               )
                               
                               
                 )
        ),
        
        # # sparseregIV
        # tabPanel(title = "sparseregIV",
        #          titlePanel("Sparsereg IV"),
        #          
        #          sidebarLayout(position = "right",
        #                        mainPanel(h2("Your Model"), 
        #                                  textOutput('spiv.txt'),
        #                                  #textOutput('sp.vars'),
        #                                  tableOutput(outputId = 'table'),
        #                                  verbatimTextOutput('spiv.modelSummary'),
        #                                  
        #                                  uiOutput("spiv.plotly.txt"),
        #                                  uiOutput("spiv.plotly.color"),
        #                                  uiOutput("spiv.diag.button"),
        #                                  
        #                                  plotlyOutput("IVplot")
        #                                  
        #                        ),
        #                        sidebarPanel(h2("Your Data"),
        #                                     # Response
        #                                     selectInput('spiv.yvar', 'Select response', ""),
        #                                     # Endogenous variable
        #                                     selectInput('spiv.endog', 'Select endogenous variable', ""),
        #                                     
        #                                     checkboxInput("num.insts", label = "Use two instruments", value = TRUE),
        #                                     
        #                                     # Instrument
        #                                     selectInput('spiv.inst', 'Select instrument', ""),
        #                                     
        #                                     hidden(
        #                                       selectInput('spiv.inst.2', 'Select second instrument', "")
        #                                     ),
        #                                     
        #                                     
        #                                     # Covariates
        #                                     selectInput('spiv.xvars', 'Select covariates', "", 
        #                                                 selected = "", multiple = TRUE),
        #                                     # Run button
        #                                     actionButton("spiv.analysis","Run"),
        #                                     tags$head(tags$style(type="text/css", "
        #                                                         #loadmessage {
        #                                                          position: fixed;
        #                                                          top: 0px;
        #                                                          left: 0px;
        #                                                          width: 100%;
        #                                                          padding: 5px 0px 5px 0px;
        #                                                          text-align: center;
        #                                                          font-weight: bold;
        #                                                          font-size: 100%;
        #                                                          color: #000000;
        #                                                          background-color: #CCFF66;
        #                                                          z-index: 105;
        #                                                          }
        #                                                          ")),
        #                                     conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        #                                                      tags$div("SparseregIV is running...",id="loadmessage"))
        #                        )
        #                        
        #          )
        # ),
        
        # sparseregIV
        tabPanel(title = "sparseregIV",
                 titlePanel("Sparsereg IV"),
                 
                 sidebarLayout(position = "right",
                               mainPanel(h2("Your Model"), 
                                         textOutput('spiv.txt'),
                                         hidden(tableOutput('spiv.data.table')),
                                         tableOutput('spiv2.data.table'),
                                         
                                         hidden(verbatimTextOutput('spiv.modelSummary')),
                                         verbatimTextOutput('spiv2.modelSummary'),
                                         
                                         # PRETTY IMPORTANTE
                                         uiOutput("dynamic.tabs")
                                         
                                         
                                         # hidden(
                                         # tabsetPanel(id = 'spiv2.diag.tabs',
                                         #   tabPanel('TWO INST tab1'),
                                         #   tabPanel('TWO INST tab2'),
                                         #   tabPanel('TWO INST tab3')
                                         # ))
                               ),
                               sidebarPanel(h2("Your Data"),
                                            # Response
                                            selectInput('spiv.yvar', 'Select response', ""),
                                            # Endogenous variable
                                            selectInput('spiv.endog', 'Select endogenous variable', ""),
                                            
                                            awesomeCheckbox("num.insts", label = "Use two instruments", value = FALSE),
                                            
                                            # Instrument
                                            selectInput('spiv.inst', 'Select instrument', ""),
                                            
                                            selectInput('spiv.inst.2', 'Select second instrument', ""),
                                            
                                            
                                            # Covariates
                                            selectInput('spiv.xvars', 'Select covariates', "", 
                                                        selected = "", multiple = TRUE),
                                            
                                            #uiOutput("spiv.run"),
                                            actionButton("spiv2.analysis", label = "IV2", class = "btn-primary"),
                                            
                                            # Run button
                                            hidden(actionButton("spiv.analysis","Run", class = "btn-primary")),
                                            
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
                                            actionButton("te.analysis","Run", class = "btn-primary"),
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
        ),
        
        # sparseregNP
        tabPanel(title = "sparseregNP",
                 titlePanel("Sparsereg NP"),
                 
                 sidebarLayout(position = "right",
                               mainPanel(h2("Your Model"), 
                                         textOutput('np.txt'),
                                         #textOutput('te.vars'),
                                         tableOutput(outputId = 'np.table'),
                                         #textOutput('s.em.text'),
                                         verbatimTextOutput('np.modelSummary')
                                         
                                         #actionButton('plotratio',"Plot ratio"),
                                         #plotOutput('corr')
                                         
                               ),
                               
                               sidebarPanel(h2("Your Data"),
                                            # Response
                                            selectInput('np.yvar', 'Select response', ""),
                                            # Covariates
                                            selectInput('np.xvars', 'Select covariates', "", 
                                                        selected = "", multiple = TRUE),
                                            # Run button
                                            actionButton("np.analysis","Run", class = "btn-primary"),
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
                                                             tags$div("SparseregNP is running...",id="loadmessage"))
                                            
                                            )
                               
                               )
                        )
        
      )
      
      
      
    ),
    
    server = function(input, output, session) {
      
      observeEvent(input$default.button, {
        toggle("file1")
        toggle('header')
        toggle('sep')
        toggle('quote')
      })
      
      # observeEvent( input$preload.dat, {
      #   print('sup')
      #   dat.orig <- reactive({
      #     df = mtcars
      #     # Linear regression dropdowns
      #     updateSelectInput(session, inputId = 'yvar', label = 'Select response',
      #                       choices = names(df), selected = names(df)[1])
      #     updateSelectInput(session, inputId = 'xvars', label = 'Select covariates',
      #                       choices = names(df))
      #     print('yoo')
      #     return(df)
      #   })
      # })
      
      # rv<-reactiveValues(data=mtcars)
      # observeEvent(input$preload.dat, {
      #   rv$data <- rnorm(100, input$recid_rate)
      # })
      # output$plot <- renderPlot({
      #   hist(rv$data)
      # })
      
      # observeEvent( input$preload.dat, {
      #   #print('sup')
      #   
      #   dat.orig <- reactive({
      #     df=mtcars
      #     return(df)
      #   })
      #   
      # })
      
      
      # Data upload--upload data and update input selections to have correct var. names

      dat.orig <- reactive({ 
        df <- reactive(# , 
          {
            mtcars
          })()
        
        if(!input$default.button) { ## ?req #  require that the input is available
          
          inFile <- input$file1 
          #print(inFile)
          
          # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
          # and                              write.csv(iris, "iris.csv")
          df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                         quote = input$quote)
        }
        # if(input$preload.dat != defaultClicked) {
        #   cat("preload button clicked ", input$preload.dat, " time\n")
        #   cat("previous value was", defaultClicked, "\n")
        #   df <- reactive(# , 
        #     {
        #       iris
        #     })()
        #   defaultClicked <<- input$preload.dat
        #   reset("file1")
        # }
        df <- df[,sapply(df, is.numeric)]
        
        # Linear regression dropdowns
        updateSelectInput(session, inputId = 'lin.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'lin.xvars', label = 'Select covariates',
                          choices = names(df))
        # IV regression dropdowns
        updateSelectInput(session, inputId = 'iv.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'iv.endog', label = 'Select endogenous variable',
                          choices = names(df), selected = names(df)[2])
        updateSelectInput(session, inputId = 'iv.inst', label = 'Select instrument',
                          choices = names(df), selected = names(df)[3])
        updateSelectInput(session, inputId = 'iv.xvars', label = 'Select covariates',
                          choices = names(df))
        # sparsereg dropdowns
        updateSelectInput(session, inputId = 'sp.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'sp.treat', label = 'Select treatment',
                          choices = names(df), selected = names(df)[2])
        updateSelectInput(session, inputId = 'sp.xvars', label = 'Select covariates',
                          choices = names(df))
        # sparseregIV dropdowns
        updateSelectInput(session, inputId = 'spiv.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'spiv.endog', label = 'Select endogenous variable',
                          choices = names(df), selected = names(df)[2])
        updateSelectInput(session, inputId = 'spiv.inst', label = 'Select instrument',
                          choices = names(df), selected = names(df)[3])
        updateSelectInput(session, inputId = 'spiv.inst.2', label = 'Select second instrument',
                          choices = names(df), selected = names(df)[4])
        updateSelectInput(session, inputId = 'spiv.xvars', label = 'Select covariates',
                          choices = names(df))
        # sparseregIV plot dropdowns
        #updateSelectInput(session, inputId = "spiv.plotly.txt", label = 'Select text', choices = names(df))
        #updateSelectInput(session, inputId = "spiv.plotly.col", label = 'Select color', choices = names(df))
        # sparseregTE dropdowns
        updateSelectInput(session, inputId = 'te.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'te.treat', label = 'Select treatment',
                          choices = names(df), selected = names(df)[2])
        updateSelectInput(session, inputId = 'te.xvars', label = 'Select covariates',
                          choices = names(df))
        # sparseregNP dropdowns
        updateSelectInput(session, inputId = 'np.yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'np.xvars', label = 'Select covariates',
                          choices = names(df))
        
        #
        output$lin.vars <- renderText({
          #eq()
          form.lin()
          #paste('lm(formula = ', eq(), 'data = ')
        })
        
        output$iv.vars <- renderText({
          iv.eq()
        })
        
        return(df)
      })
      
      
      dat.orig.all <- reactive({ 
        df.all <- reactive(# , 
          {
            mtcars
          })()
        
        if(!input$default.button) {
          req(input$file1) ## ?req #  require that the input is available
          
          inFile <- input$file1 
          
          # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
          # and                              write.csv(iris, "iris.csv")
          df.all <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                         quote = input$quote)
        }
        return(df.all)
      })
      
      output$data.table <- renderTable({
        dat.orig()
      })
      
      data.name <- reactive({
        req(input$file1) ## ?req #  require that the input is available
        f <- input$file1
        s = strsplit(f$name, '.', fixed = TRUE)
        return(s[[1]][1])
      })
      
      
      dname <- reactive({
        if(!input$default.button) {
          return(input$file1$name)
        }
        return('mtcars')
      })
      
      var.names = reactive({
        items=names(dat.orig.all())
        names(items)=items
        return(items)
      })
      
      
      
      # Data exploration
      # observeEvent( input$graph.type, {
      #   #form = as.formula(paste0('mpg ~ ', eq()))
      #   
      #   output$hist.x <- renderUI({
      #     actionButton('hist.x.b', label = "Plot diagnostics")
      #     
      #   })
      #   observeEvent( input$hist.x.b, {
      #     output$diagnostic <- renderUI({
      #       plotOutput('hist.plot')
      #     })
      #     output$hist.plot <- renderPlot({
      #       hist()
      #     })
      #   })
      #   
      # })
      
      
      
      
      
      
      ### Linear regression
      lin.eq <- reactive({
        paste(input$lin.yvar, ' ~ ', paste(input$lin.xvars, collapse = ' + '))
      })
      
      form.lin <- reactive({
        paste0('lm(formula = ', input$lin.yvar, ' ~ ', paste(input$lin.xvars, collapse = ' + '), ', data = ',
              strsplit(dname(), '.', fixed = TRUE)[[1]][1], ')')
      })
      
      
      # Event listener for linear regression 'run' button
      observeEvent( input$lin.analysis, {
        #form = as.formula(paste0('mpg ~ ', eq()))
        form = as.formula(lin.eq())
        fit.lin=lm(form, data = dat.orig())
        
        # Print model summary (tab 1)
        output$lin.modelSummary <- renderPrint({
          summary(fit.lin)
        })
        
        addTooltip(session, id = "lin.modelSummary", title = "This is an input.",
                   placement = "left", trigger = "hover")
        addTooltip(session, id = "lin.yvar", title = "This is an input.",
                   placement = "left", trigger = "hover")
        
        # Print diagnostic plot (tab 2)
        output$lin.plot <- renderPlot({
          plot(fit.lin)
        })
        
      })
    

      ### IV regression
      iv.eq <- reactive({
        #paste(input$iv.yvar, '~', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
        #     '|', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
        paste0('ivreg(formula = ', input$iv.yvar, ' ~ ', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
               ' | ', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + ') ,', data = ',
               strsplit(dname(), '.', fixed = TRUE)[[1]][1], ')' )
        
        #paste(input$xvars, collapse = ' + ')
      })
      
      iv.eq.form <- reactive({
        #paste(input$iv.yvar, '~', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
        #     '|', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
        paste0(input$iv.yvar, ' ~ ', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
              ' | ', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
        #paste(input$xvars, collapse = ' + ')
      })
      
      observeEvent( input$iv.analysis, {
        #form = as.formula(paste0('mpg ~ ', eq()))
        form = as.formula(iv.eq.form())
        fit.iv=ivreg(form, data = dat.orig())
        
        output$iv.modelSummary <- renderPrint({
          summary(fit.iv)
        })
        
      })
      
      
    
      ### sparsereg
      # reactive dataframe with data from selected variables
      sp.data <- reactive({
        dat.orig()[, c(input$sp.yvar, input$sp.treat, input$sp.xvars), drop = FALSE]
      })
      
      # table to display selected data
      output$sp.data.table <- renderTable({
        head(sp.data())
      }, rownames = TRUE)
      
      # event listener for sparsereg 'Run' button
      observeEvent( input$sp.analysis, {
        X<-as.matrix(sp.data()[-(1:2)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]

        fit.sparse <- sparsereg(y = sp.data()[,1], X, treat = sp.data()[,2], EM=eval(parse(text=input$bins)), 
                                scale.type = input$sc)
        
        output$sp.modelSummary <- renderPrint({
          summary(fit.sparse)
        })
        
        output$sp.plot <- renderPlot({
          plot(fit.sparse)
        })
        
        output$sp.viol.plot <- renderPlot({
          plot_posterior(fit.sparse, type = "mode", geom = "violin")
        })
        
      })
      
      
      # # sparseregIV
      # spiv.eq <- reactive({
      #   paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')")
      # })
      # 
      # spiv.dat <- reactive({
      #   parse(text = paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')"))
      # })
      # 
      # output$table <- renderTable({
      #   head(data())
      # }, rownames = TRUE)
      # 
      # data <- reactive({
      #   dat.orig()[, c(input$spiv.yvar, input$spiv.endog, input$spiv.inst, input$spiv.xvars), drop = FALSE]
      # })
      # 
      # observeEvent(input$num.insts, {
      #   toggle("spiv.inst.2")
      # })
      # 
      # observeEvent( input$spiv.analysis, {
      #   # dat.full = complete.cases(mtcars)
      #   X<-as.matrix(data()[-(1:3)])
      #   keep.cols<-apply(X,2,sd)>0
      #   X<-X[,keep.cols]
      #   
      #   #print(class(data()[,1]))
      #   #print(class(data()[,2]))
      #   #print(data()[,2])
      #   #print(class(data()[,3]))
      #   #print(data()[,3])
      #   #print(class(X))
      #   #print(sparseregIV)
      #   
      #   # Not working here, although runs in console :/
      #   fit.sparseIV <- sparseregIV(y = as.numeric(data()[,1]), endog = as.numeric(data()[,2]), inst = as.numeric(data()[,3]), X = X)
      # 
      #   # fit.sparseIV <- reactive({
      #   #   sparseregIV(y = as.numeric(data()[,1]), endog = as.numeric(data()[,2]), inst = as.numeric(data()[,3]), X = X)
      #   # })
      #   
      #   output$spiv.modelSummary <- renderPrint({
      #     summary(fit.sparseIV)
      #   })
      #   
      #   output$spiv.plotly.txt <- renderUI({
      #     items=names(dat.orig.all())
      #     names(items)=items
      #     selectInput("spiv.txt", label = 'Select text', choices = items)
      #   })
      #   output$spiv.plotly.color <- renderUI({
      #     items=names(dat.orig.all())
      #     names(items)=items
      #     selectInput("spiv.col", label = 'Select color', choices = items)
      #   })
      #   output$spiv.diag.button <- renderUI({
      #     actionButton('spiv.plot', label = "plotIVratio")
      #   })
      #   observeEvent( input$spiv.plot, {
      #     d <- cbind(dat.orig.all(), denom = fit.sparseIV$stage1, numer = fit.sparseIV$lice * fit.sparseIV$stage1)
      #     x <- list(
      #       title = 'Cov. of treatment and instrument from unit perturbation'
      #     )
      #     y <- list(
      #       title = 'Cov. of outcome and treatment from unit perturbation'
      #     )
      #     output$IVplot <- renderPlotly({
      #       plot_ly(d, x = ~denom, y = ~numer,
      #               color = ~eval(parse(text = input$spiv.col)),
      #               #size = ~eval(parse(text = size)),
      #               text = ~paste(input$spiv.txt, ": ", eval(parse(text = input$spiv.txt))),
      #               alpha=0.7) %>%
      #         layout(xaxis = x, yaxis = y)
      #     })
      #     
      #   })
      #   
      # })
      
      # sparseregIV
      spiv.eq <- reactive({
        paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')")
      })
      
      spiv.dat <- reactive({
        parse(text = paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')"))
      })
      
      output$spiv.data.table <- renderTable({
        head(spiv.data())
      }, rownames = TRUE)
      
      output$spiv2.data.table <- renderTable({
        head(spiv2.data())
      }, rownames = TRUE)
      
      spiv.data <- reactive({
        dat.orig()[, c(input$spiv.yvar, input$spiv.endog, input$spiv.inst, input$spiv.xvars), drop = FALSE]
      })
      spiv2.data <- reactive({
        dat.orig()[, c(input$spiv.yvar, input$spiv.endog, input$spiv.inst, input$spiv.inst.2, input$spiv.xvars), drop = FALSE]
      })
      
      observeEvent(input$num.insts, {
        toggle("spiv.inst.2")
        toggle("spiv2.analysis")
        toggle("spiv.analysis")
        
        toggle('spiv.data.table')
        toggle('spiv2.data.table')
        
        toggle('spiv.modelSummary')
        toggle('spiv2.modelSummary')
        
      })
      
      output$dynamic.tabs <- renderUI({
        
        #print(names(dat.orig()))
        items=names(dat.orig())
        print(items)
        #names(items)=items
        #print(items)
        
        if (input$num.insts == FALSE) { 
          #print(names(dat.orig()))
          tabsetPanel(
            id = "navbar",
            tabPanel(title = "tab1",
                     value = "tab1",
                     h1("Tab 1"),
                     plotOutput('spiv.plot.lte')
            ),
            tabPanel(title = "tab2",
                     value = "tab2",
                     h1("Tab 2"),
                     selectInput('test', label = 'TEST', choices=c(TRUE, FALSE), selected = TRUE)
            ),
            tabPanel(title = "tab3",
                     value = "tab3",
                     h1("Tab 3"),
                     selectInput('spiv.plotly.txt', label = 'text', choices = items),
                     selectInput('spiv.plotly.col', label = 'color', choices = items),
                     #uiOutput("spiv.plotly.txt"),
                     #uiOutput("spiv.plotly.col"),
                     actionButton('spiv.plotly.btn', label = "plotIVratio"),
                     #uiOutput("spiv.diag.button"),
                     plotlyOutput('spiv.plot')
            )
          )
        } else {
          tabsetPanel(
            id = "navbar",
            tabPanel(title = "tab4",
                     value = "tab4",
                     h1("Tab 4")
            ),
            tabPanel(title = "tab5",
                     value = "tab5",
                     h1("Tab 5")
            ),
            tabPanel(title = "tab6",
                     value = "tab6",
                     h1("Tab 6")
            )
          )
        }
      })
      
      observeEvent( input$spiv.analysis, {
        X<-as.matrix(spiv.data()[-(1:3)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        fit.sparseIV <- sparseregIV(y = as.numeric(spiv.data()[,1]), endog = as.numeric(spiv.data()[,2]), inst = as.numeric(spiv.data()[,3]), X = X)
        
        output$spiv.modelSummary <- renderPrint({
          summary(fit.sparseIV)
        })
        
        output$spiv.plot.lte <- renderPlot({
          plot_lte(fit.sparseIV)
        })
        
        # output$spiv.plotly.txt <- renderUI({
        #   items=names(dat.orig.all())
        #   names(items)=items
        #   selectInput("spiv.txt", label = 'Select text', choices = items)
        # })
        # output$spiv.plotly.color <- renderUI({
        #   items=names(dat.orig.all())
        #   names(items)=items
        #   selectInput("spiv.col", label = 'Select color', choices = items)
        # })
        # output$spiv.diag.button <- renderUI({
        #   actionButton('spiv.plot.btn', label = "plotIVratio")
        # })
        observeEvent( input$spiv.plotly.btn, {
          print('that worked')
          #print(fit.sparseIV$stage1)
          print(head(dat.orig.all()))
          d <- cbind(dat.orig.all(), denom = fit.sparseIV$stage1, numer = fit.sparseIV$lice * fit.sparseIV$stage1)
          #print(head(d))
          x <- list(
            title = 'Cov. of treatment and instrument from unit perturbation'
          )
          y <- list(
            title = 'Cov. of outcome and treatment from unit perturbation'
          )
          output$spiv.plot <- renderPlotly({
            plot_ly(d, x = ~denom, y = ~numer,
                    color = ~eval(parse(text = input$spiv.plotly.col)),
                    #size = ~eval(parse(text = size)),
                    text = ~paste(input$spiv.plotly.txt, ": ", eval(parse(text = input$spiv.plotly.txt))),
                    alpha=0.7) %>%
              layout(xaxis = x, yaxis = y)
          })

        })
        
      })
      
      
      observeEvent( input$spiv2.analysis, {
        # dat.full = complete.cases(mtcars)
        print(spiv2.data()[4])
        X<-as.matrix(spiv2.data()[-(1:4)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        #print(class(data()[,1]))
        #print(class(data()[,2]))
        #print(data()[,2])
        #print(class(data()[,3]))
        #print(data()[,3])
        #print(class(X))
        #print(sparseregIV)
        
        # Not working here, although runs in console :/
        fit.sparseIV.2 <- sparseregIV(y = as.numeric(spiv2.data()[,1]), endog = as.numeric(spiv2.data()[,2]), inst = as.numeric(spiv2.data()[,3]), 
                                      inst2 = as.numeric(spiv2.data()[,4]), X = X, mult.boot = 3)
        
        # fit.sparseIV <- reactive({
        #   sparseregIV(y = as.numeric(data()[,1]), endog = as.numeric(data()[,2]), inst = as.numeric(data()[,3]), X = X)
        # })
        
        output$spiv2.modelSummary <- renderPrint({
          summary(fit.sparseIV.2)
        })
        
      })
      
      
      
      
      # sparseregTE
      te.dat <- reactive({
        parse(text = paste0("c('", paste(input$te.xvars, collapse = "','"), "')"))
      })
      
      output$te.vars <- renderTable({
        dat.orig()[eval(te.dat())]
      })
      
      output$te.table <- renderTable({
        head(te.data())
      }, rownames = TRUE)
      
      te.data <- reactive({
        dat.orig()[, c(input$te.yvar, input$te.treat, input$te.xvars), drop = FALSE]
      })
      
      observeEvent( input$te.analysis, {
        # dat.full = complete.cases(mtcars)
        X<-as.matrix(te.data()[-(1:2)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        fit.sparseTE <- sparseregTE(y = as.numeric(te.data()[,1]), X = X, treat = as.numeric(te.data()[,2]))
        
        output$te.modelSummary <- renderPrint({
          summary(fit.sparseTE)
        })
        
      })
      
      
      # sparseregNP
      np.dat <- reactive({
        parse(text = paste0("c('", paste(input$np.xvars, collapse = "','"), "')"))
      })
      
      output$np.vars <- renderTable({
        dat.orig()[eval(np.dat())]
      })
      
      output$np.table <- renderTable({
        head(np.data())
      }, rownames = TRUE)
      
      np.data <- reactive({
        dat.orig()[, c(input$np.yvar, input$np.xvars), drop = FALSE]
      })
      
      observeEvent( input$np.analysis, {
        # dat.full = complete.cases(mtcars)
        X<-as.matrix(np.data()[-(1)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        fit.sparseNP <- sparseregNP(y = as.numeric(np.data()[,1]), X = X)
        
        output$np.modelSummary <- renderPrint({
          summary(fit.sparseNP$model)
        })
        
      })

      
    }
    
  ))
