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

                     checkboxInput("default.button", label = "Use default data"),

                  
                     hidden(
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
                     )
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
                     selectInput('graph.type', 'Select plot', 
                                 choices = c('Histogram', 'Scatter')),
                     uiOutput("hist.x"),
                     uiOutput("hist.plot"),
                     uiOutput("scat.x"),
                     uiOutput("scat.y"),
                     uiOutput("scat.plot")
                     
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
                                            actionButton("np.analysis","Run"),
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
            iris
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
        updateSelectInput(session, inputId = 'yvar', label = 'Select response',
                          choices = names(df), selected = names(df)[1])
        updateSelectInput(session, inputId = 'xvars', label = 'Select covariates',
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
        updateSelectInput(session, inputId = 'spiv.xvars', label = 'Select covariates',
                          choices = names(df))
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
        output$vars <- renderText({
          #eq()
          form.linreg()
          #paste('lm(formula = ', eq(), 'data = ')
        })
        
        output$iv.vars <- renderText({
          iv.eq()
        })
        
        return(df)
      })
      
      
      dat.orig.all <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df.all <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
      })
      
      output$contents <- renderTable({
        dat.orig()
      })
      
      data.name <- reactive({
        req(input$file1) ## ?req #  require that the input is available
        f <- input$file1
        s = strsplit(f$name, '.', fixed = TRUE)
        return(s[[1]][1])
      })
      
      
      
      # Data exploration
      observeEvent( input$graph.type, {
        #form = as.formula(paste0('mpg ~ ', eq()))
        
        output$hist.x <- renderUI({
          actionButton('hist.x.b', label = "Plot diagnostics")
          
        })
        observeEvent( input$hist.x.b, {
          output$diagnostic <- renderUI({
            plotOutput('hist.plot')
          })
          output$hist.plot <- renderPlot({
            hist()
          })
        })
        
      })
      
      
      
      
      
      
      # Linear regression
      eq <- reactive({
        paste(input$yvar, ' ~ ', paste(input$xvars, collapse = ' + '))
      })
      
      form.linreg <- reactive({
        paste0('lm(formula = ', input$yvar, ' ~ ', paste(input$xvars, collapse = ' + '), ', data = ',
              strsplit(dname(), '.', fixed = TRUE)[[1]][1], ')')
      })
      
      
      
      observeEvent( input$analysis, {
        #form = as.formula(paste0('mpg ~ ', eq()))
        form = as.formula(eq())
        model=lm(form, data = dat.orig())
        
        output$modelSummary <- renderPrint({
          summary(model)
        })
        
        output$b.diag <- renderUI({
          actionButton('l.diag', label = "Plot diagnostics")
        })
        observeEvent( input$l.diag, {
          output$diagnostic <- renderUI({
            plotOutput('d')
          })
          output$d <- renderPlot({
            plot(model)
          })
        })
        
      })
      
      
      dname <- reactive({
        if(!input$default.button) {
          return(input$file1$name)
        }
        return('iris')
      })

      # IV regression
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
        model=ivreg(form, data = dat.orig())
        
        output$iv.modelSummary <- renderPrint({
          summary(model)
        })
        
      })
      
      
    
      # sparsereg
      sp.data <- reactive({
        dat.orig()[, c(input$sp.yvar, input$sp.treat, input$sp.xvars), drop = FALSE]
      })
      
      output$sp.table <- renderTable({
        head(sp.data())
      }, rownames = TRUE)
      
      # output$sp.progress <- renderPrint({
      #   
      # })
      
      
      observeEvent( input$sp.analysis, {
        X<-as.matrix(sp.data()[-(1:2)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]

        fit.sparse <- sparsereg(y = sp.data()[,1], X, treat = sp.data()[,2], EM=eval(parse(text=input$bins)), 
                                scale.type = input$sc)
        
        output$sp.modelSummary <- renderPrint({
          summary(fit.sparse)
        })
        
        output$sp.diag.button.1 <- renderUI({
          actionButton('sp.plot.a', label = "Plot")
        })
        observeEvent( input$sp.plot.a, {
          output$sp.plot.contain <- renderUI({
            plotOutput('sp.plot')
          })
          output$sp.plot <- renderPlot({
            plot(fit.sparse)
          })
        })
        
        output$sp.diag.button.2 <- renderUI({
          actionButton('sp.viol.a', label = "Violin plot")
        })
        observeEvent( input$sp.viol.a, {
          output$sp.viol.contain <- renderUI({
            plotOutput('sp.viol')
          })
          output$sp.viol <- renderPlot({
            violinplot(fit.sparse)
          })
        })
        
      })
      
      
      # sparseregIV
      spiv.eq <- reactive({
        paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')")
      })
      
      spiv.dat <- reactive({
        parse(text = paste0("c('", paste(input$spiv.xvars, collapse = "','"), "')"))
      })
      
      output$table <- renderTable({
        head(data())
      }, rownames = TRUE)
      
      data <- reactive({
        dat.orig()[, c(input$spiv.yvar, input$spiv.endog, input$spiv.inst, input$spiv.xvars), drop = FALSE]
      })
    
      observeEvent( input$spiv.analysis, {
        # dat.full = complete.cases(mtcars)
        X<-as.matrix(data()[-(1:3)])
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
        fit.sparseIV <- sparseregIV(y = as.numeric(data()[,1]), endog = as.numeric(data()[,2]), inst = as.numeric(data()[,3]), X = X)

        # fit.sparseIV <- reactive({
        #   sparseregIV(y = as.numeric(data()[,1]), endog = as.numeric(data()[,2]), inst = as.numeric(data()[,3]), X = X)
        # })
        
        output$spiv.modelSummary <- renderPrint({
          summary(fit.sparseIV)
        })
        
        output$spiv.plotly.txt <- renderUI({
          items=names(dat.orig.all())
          names(items)=items
          selectInput("spiv.txt", label = 'Select text', choices = items)
        })
        output$spiv.plotly.color <- renderUI({
          items=names(dat.orig.all())
          names(items)=items
          selectInput("spiv.col", label = 'Select color', choices = items)
        })
        output$spiv.diag.button <- renderUI({
          actionButton('spiv.plot', label = "plotIVratio")
        })
        observeEvent( input$spiv.plot, {
          d <- cbind(dat.orig.all(), denom = fit.sparseIV$stage1, numer = fit.sparseIV$lice * fit.sparseIV$stage1)
          x <- list(
            title = 'Cov. of treatment and instrument from unit perturbation'
          )
          y <- list(
            title = 'Cov. of outcome and treatment from unit perturbation'
          )
          output$IVplot <- renderPlotly({
            plot_ly(d, x = ~denom, y = ~numer,
                    color = ~eval(parse(text = input$spiv.col)),
                    #size = ~eval(parse(text = size)),
                    text = ~paste(input$spiv.txt, ": ", eval(parse(text = input$spiv.txt))),
                    alpha=0.7) %>%
              layout(xaxis = x, yaxis = y)
          })
          
        })
        
      })
      
      
      
      # addPopover(session, "s.rb", "Data", content = paste0("
      #                                                          Waiting time between ",
      #                                                          "eruptions and the duration of the eruption for the Old Faithful geyser ",
      #                                                          "in Yellowstone National Park, Wyoming, USA.
      #                                                          
      #                                                          Azzalini, A. and ",
      #                                                          "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
      #                                                          "Applied Statistics 39, 357-365.
      #                                                          
      #                                                          "), trigger = 'click')
      
      
      
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
          summary(fit.sparseNP)
        })
        
      })

      
    }
    
  ))
