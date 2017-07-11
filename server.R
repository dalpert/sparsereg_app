server = function(input, output, session) {
  
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
  
  
  # Data upload--upload data and update input selections to have correct var. names
  dat.orig <- reactive({ 
    
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    print(inFile)
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    df <- df[,sapply(df, is.numeric)]
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
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
  
  
  # Linear regression
  eq <- reactive({
    paste(input$yvar, '~', paste(input$xvars, collapse = ' + '))
  })
  
  form.linreg <- reactive({
    paste('lm(formula = ', input$yvar, '~', paste(input$xvars, collapse = ' + '), ', data = ',
          strsplit(input$file1$name, '.', fixed = TRUE)[[1]][1], ')')
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
  
  
  # IV regression
  iv.eq <- reactive({
    #paste(input$iv.yvar, '~', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
    #     '|', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '))
    paste('ivreg(formula = ', input$iv.yvar, '~', input$iv.endog, '+', paste(input$iv.xvars, collapse = ' + '), 
          '|', input$iv.inst, '+', paste(input$iv.xvars, collapse = ' + '), ', data = ',
          strsplit(input$file1$name, '.', fixed = TRUE)[[1]][1], ')')
    
    #paste(input$xvars, collapse = ' + ')
  })
  
  observeEvent( input$iv.analysis, {
    #form = as.formula(paste0('mpg ~ ', eq()))
    form = as.formula(iv.eq())
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
    
    print(class(data()[,1]))
    print(class(data()[,2]))
    print(data()[,2])
    print(class(data()[,3]))
    print(data()[,3])
    print(class(X))
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
  
  
}