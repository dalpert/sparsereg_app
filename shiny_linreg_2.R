# Daniel Alpert
# Shiny app for 'sparsereg' package - Professor Dustin Tingley

# Load libraries
library(sparsereg)
library(AER)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(ggExtra)
library(dplyr)
data(mtcars)


runApp(
  list(
    
    ui = navbarPage('',
  
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
                sidebarPanel(h3('Your Data'),
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
                mainPanel(h2("Your Model"), 
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
                                         font-weight: bold;
                                         font-size: 100%;
                                         color: #000000;
                                         background-color: #428CF4;
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
                    bsTooltip('spivmultboot', 'Number of bootstrap samples to use during a two-instrument sparseregIV regression. Not applicable for one instrument case. More than 3 can take very long to run.',
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
                                         font-weight: bold;
                                         font-size: 100%;
                                         color: #000000;
                                         background-color: #428CF4;
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
                               mainPanel(h2("Your Model"), 
                                         textOutput('te.txt'),
                                         #textOutput('te.vars'),
                                         tableOutput(outputId = 'te.table'),
                                         #textOutput('s.em.text'),
                                         verbatimTextOutput('te.model.summary'),
                                         
                                         uiOutput('te.tabs')
                                         
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
                                                                #loadmessagete {
                                                                 position: fixed;
                                                                 top: 0px;
                                                                 left: 0px;
                                                                 width: 100%;
                                                                 padding: 55px 0px 5px 0px;
                                                                 text-align: center;
                                                                 font-weight: bold;
                                                                 font-size: 100%;
                                                                 color: #000000;
                                                                 background-color: #428CF4;
                                                                 z-index: 105;
                                                                 }
                                                                 ")),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             tags$div("SparseregTE is running...",id="loadmessagete"))
                                            
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
                                         verbatimTextOutput('np.model.summary')
                                         
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
                                                                 top: 60px;
                                                                 left: 0px;
                                                                 width: 100%;
                                                                 padding: 5px 0px 5px 0px;
                                                                 text-align: center;
                                                                 font-weight: bold;
                                                                 font-size: 100%;
                                                                 color: #000000;
                                                                 background-color: #428CF4;
                                                                 z-index: 105;
                                                                 }
                                                                 ")),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             tags$div("SparseregNP is running...",id="loadmessage"))
                                            
                                            )
                               
                               )
                        )
      
    ),
    
    server = function(input, output, session) {

      observeEvent( input$default.button, {
        toggle("file1")
        toggle('header')
        toggle('sep')
        toggle('quote')
      })
      
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
        output$lin.model.summary <- renderPrint({
          summary(fit.lin)
        })
        
        addTooltip(session, id = "lin.model.summary", title = "This is an input.",
                   placement = "left", trigger = "hover")
        addTooltip(session, id = "lin.yvar", title = "This is an input.",
                   placement = "left", trigger = "hover")
        
        output$lin.tabs <- renderUI({
          tabsetPanel(
            tabPanel('Residual vs. Fitted',
              plotOutput('lin.plot.1')
            ),
            tabPanel('Normal Q-Q',
              plotOutput('lin.plot.2')
            ),
            tabPanel('Scale-Location',
              plotOutput('lin.plot.3')
            ),
            tabPanel("Cook's Distance",
              plotOutput('lin.plot.4')
            )
          )
        })
        
        output$lin.plot.1 <- renderPlot({
          plot(fit.lin, which = 1)
        })
        output$lin.plot.2 <- renderPlot({
          plot(fit.lin, which = 2)
        })
        output$lin.plot.3 <- renderPlot({
          plot(fit.lin, which = 3)
        })
        output$lin.plot.4 <- renderPlot({
          plot(fit.lin, which = 4)
        })
        
        
        
      })
    

      ### IV regression
      iv.eq <- reactive({
        paste0('ivreg(formula = ', input$iv.yvar, ' ~ ', input$iv.endog, ' + ', paste(input$iv.xvars, collapse = ' + '), 
               ' | ', input$iv.inst, ' + ', paste(input$iv.xvars, collapse = ' + ') ,', data = ',
               strsplit(dname(), '.', fixed = TRUE)[[1]][1], ')' )
      })
      
      iv.eq.form <- reactive({
        #paste(input$iv.yvar, '~', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
        #     '|', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
        paste0(input$iv.yvar, ' ~ ', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
              ' | ', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
        #paste(input$xvars, collapse = ' + ')
      })
      
      observeEvent( input$iv.analysis, {
        form = as.formula(iv.eq.form())
        fit.iv=ivreg(form, data = dat.orig())
        
        output$iv.model.summary <- renderPrint({
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

        fit.sparse <- sparsereg(y = sp.data()[,1], X, treat = sp.data()[,2], EM=eval(parse(text=input$spem)), 
                                scale.type = input$spscale)
        
        output$sp.model.summary <- renderPrint({
          summary(fit.sparse)
        })
        
        output$sp.tabs <- renderUI({
          tabsetPanel(
            tabPanel('Plot',
                plotOutput('sp.plot')
            ),
            tabPanel('Violin Plot',
                plotOutput('sp.viol.plot')
            )
          )
        })
        
        output$sp.plot <- renderPlot({
          plot(fit.sparse)
        })
        output$sp.viol.plot <- renderPlot({
          plot_posterior(fit.sparse, type = "mode", geom = "violin")
        })
        
      })
      
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
      
      observeEvent( input$num.insts, {
        toggle('spiv.inst.2')
        toggle('spivmultboot')
        toggle('spiv2.analysis')
        toggle('spiv.analysis')
        
        toggle('spiv.data.table')
        toggle('spiv2.data.table')
        
        toggle('spiv.model.summary')
        toggle('spiv2.model.summary')
        
        toggle('spiv.tabs')
        toggle('spiv2.tabs')
      })
      
      # output$spiv.tabs <- renderUI({
      #   
      #   #print(names(dat.orig()))
      #   items=names(dat.orig.all())
      #   print(items)
      #   #names(items)=items
      #   #print(items)
      #   
      #   if (input$num.insts == FALSE) { 
      #     #print(names(dat.orig()))
      #     tabsetPanel(
      #       id = "navbar",
      #       tabPanel(title = "tab1",
      #                value = "tab1",
      #                h1("Tab 1"),
      #                plotOutput('spiv.plot.lte')
      #       ),
      #       tabPanel(title = "tab2",
      #                value = "tab2",
      #                h1("Tab 2"),
      #                selectInput('test', label = 'TEST', choices=c(TRUE, FALSE), selected = TRUE)
      #       ),
      #       tabPanel(title = "tab3",
      #                value = "tab3",
      #                h1("Tab 3"),
      #                selectInput('spiv.plotly.txt', label = 'text', choices = items),
      #                selectInput('spiv.plotly.col', label = 'color', choices = items),
      #                #uiOutput("spiv.plotly.txt"),
      #                #uiOutput("spiv.plotly.col"),
      #                actionButton('spiv.plotly.btn', label = "plotIVratio"),
      #                #uiOutput("spiv.diag.button"),
      #                plotlyOutput('spiv.plot')
      #       )
      #     )
      #   } else {
      #     tabsetPanel(
      #       id = "navbar",
      #       tabPanel(title = "tab4",
      #                value = "tab4",
      #                 h1("Tab 4"),
      #                 awesomeRadio('spiv2.col', label = 'Color', choices = c(TRUE, FALSE), selected = TRUE),
      #                 plotOutput('spiv2.plot'),
      #                 plotlyOutput('spiv2.plotly')
      # 
      #                
      #       ),
      #       tabPanel(title = "tab5",
      #                value = "tab5",
      #                h1("Tab 5")
      #       ),
      #       tabPanel(title = "tab6",
      #                value = "tab6",
      #                h1("Tab 6")
      #       )
      #     )
      #   }
      # })
      
      observeEvent( input$spiv.analysis, {
        X<-as.matrix(spiv.data()[-(1:3)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        fit.sparseIV <- sparseregIV(y = as.numeric(spiv.data()[,1]), endog = as.numeric(spiv.data()[,2]), inst = as.numeric(spiv.data()[,3]), X = X)
        
        items=names(dat.orig.all())
        print(items)
        
        output$spiv.tabs <- renderUI({
          tabsetPanel(
            tabPanel('LTE Plot',
                    plotOutput('spiv.plot.lte')
            ),
            tabPanel('Plotly',
                    selectInput('spiv.plotly.txt', label = 'text', choices = items),
                    selectInput('spiv.plotly.col', label = 'color', choices = items),
                    actionButton('spiv.plotly.btn', label = "plotIVratio"),
                    plotlyOutput('spiv.plot')
            )
          )
        })
        
        output$spiv.model.summary <- renderPrint({
          summary(fit.sparseIV)
        })
        
        output$spiv.plot.lte <- renderPlot({
          plot_lte(fit.sparseIV)
        })
        
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
        print(spiv2.data()[4])
        X<-as.matrix(spiv2.data()[-(1:4)])
        keep.cols<-apply(X,2,sd)>0
        X<-X[,keep.cols]
        
        # Not working here, although runs in console :/
        fit.sparseIV.2 <- sparseregIV(y = as.numeric(spiv2.data()[,1]), endog = as.numeric(spiv2.data()[,2]), inst = as.numeric(spiv2.data()[,3]), 
                                      inst2 = as.numeric(spiv2.data()[,4]), X = X, mult.boot = input$spivmultboot)
        
        output$spiv2.model.summary <- renderPrint({
          summary(fit.sparseIV.2)
        })
        
        output$spiv2.plot <- renderPlot({
          plot_lte(fit.sparseIV.2, color = TRUE)
        })
        
        # if (input$spiv2.color) {
        #   output$spiv2.plot <- renderPlot({
        #     plot_lte(fit.sparseIV.2, color = TRUE)
        #   })
        # } else {
        #   output$spiv2.plotly <- renderPlotly({
        #     plot_lte(fit.sparseIV.2)
        #   })
        # }
        
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
        
        print(exists('fit.sparseTE'))
        
        fit.sparseTE <- sparseregTE(y = as.numeric(te.data()[,1]), X = X, treat = as.numeric(te.data()[,2]))
        
        output$te.model.summary <- renderPrint({
          summary(fit.sparseTE)
        })
        
        print(exists('fit.sparseTE'))
        
        output$te.tabs <- renderUI({
          
          items=names(te.data()[(-1)])  
          
          print('this works!!!')
            tabsetPanel(
              tabPanel('Diagnostic plot - 2 vars',
                       selectInput('te2.plot.x', label = 'X', choices = items),
                       selectInput('te2.plot.y', label = 'Y', choices = items),
                       awesomeRadio('te2.plot.col', label = 'Color', choices = c(TRUE, FALSE), selected = TRUE),
                       awesomeRadio('te2.plot.marg', label = 'Marginal distribution', choices = c(TRUE, FALSE), selected = TRUE),
                       actionBttn('te2.plot.btn', 'Plot'),
                       plotOutput('te2.plot')
              ),
              tabPanel('Diagnostic plot - 1 var',
                       selectInput('te1.plot.x', label = 'X', choices = items),
                       awesomeRadio('te1.plot.col', label = 'Color', choices = c(TRUE, FALSE), selected = TRUE),
                       awesomeRadio('te1.plot.marg', label = 'Marginal distribution', choices = c(TRUE, FALSE), selected = TRUE),
                       sliderInput("te1.plot.size", "Point size", min = 0.1, max = 2, value = 0.1, step= 0.1),
                       actionBttn('te1.plot.btn', 'Plot'),
                       plotOutput('te1.plot')
              )
            )
        })
        
        observeEvent( input$te2.plot.btn, {
          output$te2.plot <- renderPlot({
            plot_lte(fit.sparseTE, dat.orig()[,input$te2.plot.x], dat.orig()[,input$te2.plot.y], 
                     add.marginal = input$te2.plot.marg, color = input$te2.plot.col)
          })
        })
        
        observeEvent( input$te1.plot.btn, {
          output$te1.plot <- renderPlot({
            plot_lte(fit.sparseTE, dat.orig()[,input$te1.plot.x], add.marginal = input$te1.plot.marg, 
                     color = input$te1.plot.col, size = input$te1.plot.size)
          })
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
        
        output$np.model.summary <- renderPrint({
          summary(fit.sparseNP$model)
        })
        
      })

      
    }
    
  ))
