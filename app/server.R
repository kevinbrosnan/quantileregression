library(shiny)
library(shinydashboard)
library(DT)
library(quantreg)
library(lqmm)
library(bayesQR)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  output$thyroid.tab <- DT::renderDataTable(thyroid, 
                        rownames = FALSE, server = FALSE,
                        options = list(pageLength = 5, dom = 'tp'),
                        selection = list(mode = "single", 
                                         target = "column",
                                         selected = 1)
  )
  
  output$thyroid.tab.val <- DT::renderDataTable(
                            validation.tab(input$thyroid.tab_columns_selected), 
                            rownames = FALSE, 
                            options = list(dom = "tp")
  )
  
  output$thyroid.plot.val <- renderPlot({
    if (!is.null(input$thyroid.tab_columns_selected)) {
      data.col <- input$thyroid.tab_columns_selected + 1
      var.name <- names(thyroid)[data.col]
      var.class <- class(thyroid[[data.col]])
      
      switch(var.class, 
             character = {
               # Graphical Barchart of the character data
               barplot(table(thyroid[,var.name]),
                       main = paste0("Barchart of ", var.name),
                       xlab = var.name, las = 1)
             },
             numeric = {
               # Graphical Histogram of the numerical data 
               hist(thyroid[,var.name], 
                    main = paste0("Histogram of ", var.name),
                    xlab = var.name, las = 1)
             },
             integer = {
               # Graphical Barchart of the integer data
               barplot(table(thyroid[,var.name]), 
                       main = paste0("Barchart of ", var.name),
                       xlab = var.name, las = 1)
             }
      )
    }
  })

  
  output$analdesc <- renderPlot({
    plot(x = thyroid$Gestation, y = thyroid[[input$dep_var]],
         main = paste0("Gestation v ", input$dep_var), 
         ylab = input$dep_var, xlab = "Gestation",
         las = 1)
  }) 
  
  
  model.quantile <- reactive({
    if (input$method == "Frequentist") {
      mdl.quant <- rq(thyroid[[input$dep_var]] ~ thyroid$Gestation,
                      tau = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
      quant.results <- as.data.frame(t(coef(mdl.quant)))

    } else if (input$method == "Bayesian") {
      
      mdl.bays <- bayesQR(thyroid[[input$dep_var]] ~ thyroid$Gestation, ndraw = 100,
                          quantile = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
      quant.results <- data.frame(matrix(NA, nrow = 7, ncol = 2))
      for (i in 1:7) {
        quant.results[i,] <- mdl.bays[[i]]$betadraw[100,1:2]  
      }
      
    } else if (input$method == "Linear Mixed Models") {
      
      mdl.mix <- lqm(thyroid[[input$dep_var]] ~ thyroid$Gestation,
                     tau = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9,0.95))
      quant.results <- data.frame(t(coef(mdl.mix)), row.names = NULL)
    }
    
    linear.quant <- lm(thyroid[[input$dep_var]] ~ thyroid$Gestation)
    quant.results[8,1] <- linear.quant$coefficients[1]
    quant.results[8,2] <- linear.quant$coefficients[2]
    colnames(quant.results) <- c("Intercept", "Slope")
    quant.results[,"Quantile"] <- c("Tau = 0.05", "Tau = 0.10", "Tau = 0.25",
                                 "Median","Tau = 0.75","Tau = 0.90",
                                 "Tau = 0.95","Mean")
    
    quant.results <- data.frame(quant.results[ ,"Quantile"], 
                                round(quant.results[ ,"Intercept"], 3), 
                                round(quant.results[ ,"Slope"], 3))
    colnames(quant.results) <- c("Quantile", "Intercept", "Slope")
    return(quant.results)
  })
  
  
  output$modeltab <- DT::renderDataTable(model.quantile(),
                                         rownames = FALSE, 
                                         options = list(dom = "t")
    
  )
    
  output$modelplot <- renderPlot({
    plot(x = thyroid$Gestation, y = thyroid[[input$dep_var]],
         main = paste0("Gestation v ", input$dep_var, " by ", input$method, " method"),
         xlab = "Gestation", ylab = input$dep_var)
    mdl.output <- model.quantile()
    for (i in 1:7) {
      abline(a = mdl.output$Intercept[i], b = mdl.output$Slope[i], 
             col = mdl.output$Quantile[i])
    }
  })  
  
})