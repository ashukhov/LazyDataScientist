# Lazy Data Scientist is a R-developed statistics tool to facilitate data analysis for students who have zero to none experience regarding this topic
# Copyright (C) 2020 Arseniy Shukhov

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sortable)
library(data.table)
library(dplyr)
library(pROC)
library(ggplot2)
library(ggmosaic)
library(ggpubr)
library(DT)
library(corrgram)
library(gridExtra)
library(RColorBrewer)


source("functions.R")

dfGlobal <- NULL

shinyServer(function(input, output) {
  
  ###########################################################################################################################
  ###########################################################################################################################
  ## Upload
  ## Der erste Tab. Hier wird ein .csv-Datensatz hochgeladen. Dieser Datensatz bildet die Basis für die gesamte restliche Funktionalität
  
  
  getFile <- reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$df_omit) {
      omit_replace <- input$omit_replace
      if(omit_replace == "omit") {
        df <- na.omit(df)  
      }
      else if(omit_replace == "exclude") {
        df <- na.omit(df, na.action="exclude")
      } 
      else if(omit_replace == "replace") {
        df <- na.omit(df, na.action="replace")
      }
    }
    dfGlobal <<- df
    return(df)
  })
  
  output$readCSVCode <- renderPrint({
    print(paste("dfGlobal <- read.csv(", input$file1$datapath, ", header=", input$header, ", sep='", input$sep, "', quote='", input$quote, "')",sep=""))
  })
  ##
  # Liest den Datensatz aus der .csv-Datei
  # Globale Variable wird überschrieben
  # Funktion muss mind. 1-mal aufgerufen werden,
  # um den Datensatz zu setzen
  
  ##
  # Ausgabe des Datensatz
  
  output$contents <- renderDataTable({
    df <- getFile()
    head(df, nrow(df))
  }) 
  
  ##
  # Ausgabe des Codes für den Datensatz
  
  output$contentsCode <- renderPrint({
    print("head(dfGlobal, nrow(dfGlobal))")
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Structure 
  ## Hier sollen Informationen über den Datensatzes angezeigt werden
  
  ##
  # Erhalten der Datensatz-Structure
  # muss reactive sein, im Falle einer dplyr Ãnderung
  
  getStructure <- reactive({
    return(str(dfGlobal))
  })
  
  ##
  # Ausgabe der Datensatz-Structure
  
  output$structureDF <- renderPrint({
    getStructure()
  })
  
  ##
  # Erhalten des Codes der Datensatz-Structure

  output$structureDFCode <- renderPrint({
    print("str(dfGlobal)")
  })
  
  ##
  # Erhalten der Datensatz-Summary
  # muss reactive sein, im Falle einer dplyr Ãnderung
  
  getSummary <- reactive({
    return(summary(dfGlobal))
    
  })
  
  ##
  # Ausgabe der Datensatz-Summary
  
  output$summaryDF <- renderPrint({
    getSummary()
  })
  
  ##
  # Erhalten des Codes der Datensatz-Summary
  
  output$summaryDFCode <- renderPrint({
    print("summary(dfGlobal)")
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Exploration
  ## Bei der Exploration werden aus den ausgewählten Variablen die Werte berechnet und angezeigt
  
  ## 
  # Drag $ Drop Menu für Exploration
  
  output$dragExploration <- renderUI({
    fluidRow(
      column(
        width = 12,
        bucket_list(
          header = "Spaltenauswahl",
          group_name = "exploration_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Alle Parameter",
            labels = getHeader(dfGlobal),    
            input_id = "exploration_param_untaken"
          ),
          add_rank_list(
            text = "Parameter für Analyse",
            labels = NULL,
            input_id = "exploration_param_taken"
          )
        )
      )
    )
  })
  
  ##
  # reactive Funktion, um eine Tabelle zu erstellen
  # mit den Variablen und dessen berechneten Werten
  
  useDesc <- reactive({
    req(input$exploration_param_taken)
    textDesc <- ""
    data <- setNames(data.table(matrix(nrow = 0, ncol = (1+length(input$exploration_param_taken)))), 
                     c("Data",input$exploration_param_taken))
    
    for(val in input$checkGroup) {
      values<-list()
      values <- append(values, val)
      for(header in input$exploration_param_taken) {
        values <-append(values, eval(parse(text=paste(textDesc,val," (dfGlobal[,\"", header,"\"", "]);", sep=""))))
      }
      
      frame <- data.frame(values)
      data <- rbind(data, values, fill=TRUE,make.row.names = "T")
    }
    
    return(data)
  })
  
  ##
  # Ausgabe der Exploration-Tabelle
  
  output$value <- renderTable({
    useDesc()
  })
  
  ##
  # show code for exploration in table format
  
  output$explorationCode <- renderTable({
    table <- c()
    for(val in input$checkGroup) {
      counter <- 1
      text <- paste(val, "(dfGlobal[,c(", sep="")
      for(header in input$exploration_param_taken) {
        if(counter == length(input$exploration_param_taken)) {
          text <- paste(text, "'", header, "'", sep="")
        } else {
          text <- paste(text, "'", header, "',", sep="")
        }
        counter <- counter + 1
      }
      table <- append(table, paste(text, ")])", sep=""))
    }
    head(table)
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Correlation
  ## Aus den ausgewählten Variablen für das Regressionsmodell werden die unabhängigen Variablen auf Korrelation geprüft
  ## Korrelogramme, Pairwise Correlation Plots und eine Warnung im bestimmten Fall sollen dem User helfen
  
  
  # Regression Bucketlist
  output$dragRegression <- renderUI({
    fluidRow(
      column(
        width = 12,
        bucket_list(
          header = "Spaltenauswahl",
          group_name = "regression_group",
          orientation = "vertical",
          add_rank_list(
            text = "Alle Parameter",
            labels = getHeader(dfGlobal),
            input_id = "regression_param_untaken"
          ),
          add_rank_list(
            text = "Abhängige Variable (Output) (nur eine)",
            labels = NULL,
            input_id = "regression_param_dependent"
          ),
          add_rank_list(
            text = "Unabhängige Variablen (Regressoren)",
            labels = NULL,
            input_id = "regression_param_independent"
          )
        )
      )
    )
  })
  
  
  
  output$plotCorrelogram <- renderPlot({
    corrgram(dfGlobal, order=TRUE, lower.panel=panel.shade,
             upper.panel=panel.pie, text.panel=panel.txt,
             main="Correlogram")
  })
  
  output$correlogramCode <- renderPrint({
    print("corrgram(dfGlobal, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main='Correlogram')")
  })
  
  ## UI-Methode, welche den pairs-Plot fÃ¯Â¿Â½r die Korrelation ausgibt
  output$plotCorrelation <- renderPlot({
    if(input$checkboxRegression) {
      command <- makeCorrelationPlot(input$regression_param_dependent, input$regression_param_independent)
      eval(parse(text=command))
    }
  })
  

  
  
  # Download Correlationplot
  output$downloadCorrelation <- downloadHandler(
    filename = function() {
      # Filename mit aktuellem Datum wird festgelegt
      # format(Sys.time(), "%Y-%m-%d %H-%M-%OS")
      # Beispiel "2020-03-08 23-03-28"
      paste("correlation-", format(Sys.time(), "%Y-%m-%d %H-%M-%OS"), input$radioCorrelation, sep="")
    },
    content = function(file) {
      if(input$radioCorrelation == ".png")
        png(file)
      else
        pdf(file)
      
      # Insert plot
      command <- makeCorrelationPlot(input$regression_param_dependent, input$regression_param_independent)
      eval(parse(text=command))
      dev.off()
    }
  )
  
  ##
  # Methode, um für jede unabhängige Variable einen Meldungstext zu erhalten.
  # Korrelierende Variablen werden beschrieben.
  
  getCorrelationHelp <- reactive({
    
    text <- ""
    allCor <- list()
    
    if(length(input$regression_param_independent) > 1) {
      for(i in 1:length(input$regression_param_independent)) {
        for(j in i:length(input$regression_param_independent)) {
          
          val1 <- input$regression_param_independent[i]
          val2 <- input$regression_param_independent[j]
          
          if(val1 == val2) {
            next
          }
          
          command <- "abs(cor("
          command <- paste(command, "dfGlobal[,\"", val1, "\"],",sep="")
          command <- paste(command, "dfGlobal[,\"", val2, "\"]))", sep="")
          
          correlation <- eval(parse(text=command))

            if(correlation > 0.5) {
            text <- paste(text, " Es besteht eine Korrelation zwischen ", val1, " und " , val2,".", sep="")
            allCor <- append(allCor, paste(val1 , ",", val2, sep=""))
          }
        }
      }
    }
    return(text)
    
  })
  
  output$correlationHelp <- renderUI({
    getCorrelationHelp()
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ## Regression
  # Hier sollen aus den ausgewählten Variablen Regressionsmodelle erstellt werden.
  # Linear oder Logistisch wird vom System erfasst
  
  
  ##
  # Zeigt eine Summary des Regressionsmodells an
  
  output$regressionSummary <- renderPrint({
    if(input$checkboxSummary) {
      type <- eval(parse(text=paste("class(dfGlobal$", input$regression_param_dependent,")",sep="")))
      if(type == 'numeric' | type == 'integer') {
        linearmodell <- createLinearRegression(dfGlobal, input$regression_param_dependent, input$regression_param_independent)
        summary(linearmodell)
      }
      else if(type  == 'factor' | type == 'character') {
        logisticmodell <- useLogisticRegression()
        summary(logisticmodell)
      }
    }
  })

  ##
  # Ãberprüft die Variablen
  # Anhand dessen wird entweder ein lineares Modell 
  # oder ein logistisches erstellt und angezeigt
  
  output$plotRegression <- renderPlot({
    if(input$checkboxRegression) {
      type <- eval(parse(text=paste("class(dfGlobal$", input$regression_param_dependent,")",sep="")))
      if(type == 'numeric' | type == 'integer') {
        par(mfrow=c(2,2))
        plot(createLinearRegression(dfGlobal, input$regression_param_dependent, input$regression_param_independent))
        par(mfrow=c(1,1))
      }
      else if(type  == 'factor' | type == 'character') {
        plotROC()
      }
    }
  })
  
  ##
  # Gibt den Regressionscode an
  
  output$regressionCode <- renderPrint({
    if(input$checkboxRegression) {
      type <- eval(parse(text=paste("class(dfGlobal$", input$regression_param_dependent,")",sep="")))
      if(type == 'numeric' | type == 'integer') {
        print(createLinearRegression(dfGlobal, input$regression_param_dependent, input$regression_param_independent)$call)
      }
      else if(type  == 'factor' | type == 'character') {
        print(useLogisticRegression()$call)
      }
    }  })
  
  ##
  # Download Regressionplot
  
  output$downloadRegression <- downloadHandler(
    filename = function() {
      # Filename mit aktuellem Datum wird festgelegt
      # format(Sys.time(), "%Y-%m-%d %H-%M-%OS")
      # Beispiel "2020-03-08 23-03-28"
      paste("regression-", format(Sys.time(), "%Y-%m-%d %H-%M-%OS"), input$radioRegression, sep="")
    },
    content = function(file) {
      if(input$radioRegression == ".png")
        png(file)
      else
        pdf(file)
      
      par(mfrow=c(2,2))
      plot(createLinearRegression(dfGlobal, input$regression_param_dependent, input$regression_param_independent))
      dev.off()
    }
  )
  
  ##
  # Diese Methode erstellt ein logistisches Regressionsmodell
  # anhand der abhÃ¯Â¿Â½ngigen Variable und der unabhÃ¯Â¿Â½ngigen Variablen
  
  useLogisticRegression <- reactive({
    
    text <- "glm(dfGlobal[,\""
    text <- paste(text, input$regression_param_dependent,"\"] ~ ",sep="")
    
    counter <- 1
    for(val in input$regression_param_independent) {
      if(counter == length(input$regression_param_independent)) {
        text <- paste(text, "dfGlobal[,\"",val, "\"],", sep="")
      }
      else {
        text <- paste(text, "dfGlobal[,\"", val, "\"] + ", sep="")
      }
      counter <- counter + 1
    }
    
    text <- paste(text, "data=dfGlobal, family=binomial(link=\"logit\"))", sep="")
    logistmodell <- eval(parse(text=text))
    return(logistmodell)
  })
  
  
  ## 
  # Funktion zur Erstellung einer ROC-Kurve fÃ¯Â¿Â½r das logistische Regressionsmodell
  
  plotROC <- reactive({
    
    logistmodell <- useLogisticRegression()
    text <- "predict(logistmodell, newdata=dfGlobal[,c("
    counter <- 1
    for(val in input$regression_param_independent) {
      if(counter == length(input$regression_param_independent)) {
        text <- paste(text, "\"", val, "\"", sep="")
      }
      else {
        text <- paste(text, "\"", val, "\",", sep="")
      }
      counter <- counter + 1
    }
    text <- paste(text, ")])",sep="")
    
    pr <- eval(parse(text=text))
    
    text <- ""
    
    dFrame<-data.frame(cbind(labels = as.numeric(
      eval(parse(text=paste("dfGlobal[,\"", input$regression_param_dependent, "\"]", sep="")))
    )-1),
    predictions=ifelse(pr<=0,0,1))
    
    pROC_obj <- roc(response = dFrame$labels,
                    predictor=dFrame$predictions,
                    smoothed = TRUE,
                    # arguments for confidence interval
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE,
                    max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)
    # calculates confidence region
    sens.ci <- ci.se(pROC_obj)
    # colours confidence region lightblue
    plot(sens.ci, type="shape", col="lightblue")
    # adds error bars to plot
    plot(sens.ci, type="bars")
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Prediction 
  ## Werte für y werden anhand der eingegebenden x-Werte berechnet
  
  ##
  # Hier werden mehrere Boxes erstellt
  # Jede Box entspricht einer unabhängigen Variable
  # muss reactive sein, bei Ãnderung des Modells
  
  get_prediction_boxes <- reactive({
    prediction_boxes <- list()
    
    for (i in 1:length(input$regression_param_independent)){
      prediction_boxes[[i]] <- box(width = 3, background = "blue",
                                   title = h3(input$regression_param_independent[i], style = "display:inline; font-weight:bold"),
                                   textInput(paste("prediction_value_", i,sep=""), label="Value"))
      
    }
    return(prediction_boxes)
  })
  
  ##
  # Ausgabe der Boxes
  
  output$select_prediction <- renderUI({
    req(input$regression_param_dependent)
    req(input$regression_param_independent)
    get_prediction_boxes()
  })
  
  ##
  # Der y-Wert wird anhand der eingegebenden x-Werte berechnet
  
  get_prediction_y_value <- reactive({
    req(input$regression_param_dependent)
    req(input$regression_param_independent)
    newDF <- data.frame(matrix(ncol = 3, nrow = 0))
    col <- c(input$regression_param_independent)
    
    values <- c()
    for(i in 1:length(input$regression_param_independent)) {
      values <- c(values, eval(parse(text=paste("input$prediction_value_", i,sep=""))))
    }
    newDF <- rbind(newDF, as.numeric(values))
    colnames(newDF) <- col
    predicted_value <- predict(createLinearRegression(dfGlobal, input$regression_param_dependent, input$regression_param_independent),newdata=newDF, interval="prediction", level=0.95)
    return(predicted_value)
  })
  
  ##
  # Ausgabe des y-Werts mit CI
  
  output$prediction_y_value <- renderPrint({get_prediction_y_value()})

  
  ###########################################################################################################################
  ###########################################################################################################################
  ## Grafik 
  # Dieser Teil besteht aus mehreren Feldern, für die Eingabe von Variablen, Auswahl von Plots und Plot-Einstellungen
  # Daraufhin werden Plots erstellt mit den entsprechenden Einstellungen
  
  
  ## 
  # Drag $ Drop Menu for the Graphic Plots
  
  output$dragGrafik <- renderUI({
    fluidRow(
      column(
        width = 12,
        bucket_list(
          header = "Spaltenauswahl",
          group_name = "grafik_group",
          orientation = "vertical",
          add_rank_list(
            text = "Alle Parameter",
            labels = getHeader(dfGlobal),
            input_id = "grafik_param_untaken"
          ),
          add_rank_list(
            text = "y-Achse (nur eine)",
            labels = NULL,
            input_id = "grafik_param_y"
          ),
          add_rank_list(
            text = "x-Achse (nur eine)",
            labels = NULL,
            input_id = "grafik_param_x"
          ),
          add_rank_list(
            text = "Colored categories",
            labels=NULL,
            input_id="grafik_param_color"
          ),
          add_rank_list(
            text = "Categorise multiple graphics",
            labels=NULL,
            input_id = "grafik_param_facet"
          )
        )
      )
    )
  })
  
  ##
  # Outputs a histogram of chosen variables from given dataset
  
  output$plotHistogram <- renderPlot({
    if('histogram' %in% input$checkboxGraphic) {
      par(mfrow=c(2,1))
      command <- "ggplot(dfGlobal, aes(x=dfGlobal$"
      command <- paste(command, input$grafik_param_y, ")) + geom_histogram(color='#add8e6', fill='#add8e6')", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$histogramCode <- renderPrint({
    if('histogram' %in% input$checkboxGraphic) {
      par(mfrow=c(2,1))
      command <- "ggplot(dfGlobal, aes(x=dfGlobal$"
      command <- paste(command, input$grafik_param_y, ")) + geom_histogram(color='#add8e6', fill='#add8e6')", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs a scatterplot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotScatterplot <- renderPlot({
    if('scatter' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command, "+ geom_point()", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$scatterplotCode <- renderPrint({
    if('scatter' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command, "+ geom_point()", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs a barplot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotBarplot <- renderPlot({
    if('barplot' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command, " + geom_bar(stat=\"identity\")", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$barplotCode <- renderPrint({
    if('barplot' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command, " + geom_bar(stat=\"identity\")", sep="") 
      print(command)
    }
  })
  
  # Outputs a lineplot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotLineplot <- renderPlot({
    if('line' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command," + geom_line()", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$lineplotCode <- renderPrint({
    if('line' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y, 
                            scale_x_log=('scale_x_log' %in% input$graphic_settings), 
                            scale_y_log=('scale_y_log' %in% input$graphic_settings),
                            color_variable = input$grafik_param_color, color=(length(input$grafik_param_color) > 0),
                            facet_variable = input$grafik_param_facet, facet_wrap = (length(input$grafik_param_facet) > 0))
      command <- paste(command," + geom_line()", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs a boxplot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotBoxplot <- renderPlot({
    if('boxplot' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, "+ geom_boxplot()", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$boxplotCode <- renderPrint({
    if('boxplot' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, "+ geom_boxplot()", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs a qqplot of chosen variables from given dataset
  
  output$plotQQ <- renderPlot({
    if('qq' %in% input$checkboxGraphic) {
      req(input$grafik_param_y)
      plot <- NULL
      if(length(input$grafik_param_x) > 0) {
        plot <- ggqqplot(dfGlobal, x = input$violinCI_param_y, 
                 color=input$grafik_param_x,
                 ggtheme = theme_pubclean())
      }
      else {
        plot <- ggqqplot(dfGlobal, x = input$violinCI_param_y, 
                 color="darkblue",
                 ggtheme = theme_pubclean())
      }
      plot
    }
  })
  
  output$QQCode <- renderPrint({
    if('qq' %in% input$checkboxGraphic) {
      req(input$grafik_param_y)
      if(length(input$grafik_param_x) > 0) {
        print(paste("ggqqplot(dfGlobal, x = ", input$violinCI_param_y, 
                         "color=", input$grafik_param_x,
                         "ggtheme = theme_pubclean())", sep=""))
      }
      else {
        print(paste("ggqqplot(dfGlobal, x = ", input$violinCI_param_y, 
                         "color='darkblue', ggtheme = theme_pubclean())", sep=""))
      }
    }
  })
  
  ##
  # Outputs a violin plot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotViolin <- renderPlot({
    if('violin' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, " + geom_violin()", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$violinCode <- renderPrint({
    if('violin' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, " + geom_violin()", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs an ecdf plot of chosen variables from given dataset
  # calls functions.R script
  
  output$plotECDF <- renderPlot({
    if('ecdf' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, " + stat_ecdf(geom = \"point\")", sep="") 
      eval(parse(text=command))
    }
  })
  
  output$ECDFCode <- renderPrint({
    if('ecdf' %in% input$checkboxGraphic) {
      command <- makeGGPlot(independent=input$grafik_param_x, dependent=input$grafik_param_y)
      command <- paste(command, " + stat_ecdf(geom = \"point\")", sep="") 
      print(command)
    }
  })
  
  ##
  # Outputs a Mosaicplot of chosen variables from given dataset
  
  output$plotMosaicplot <- renderPlot({
    if('mosaic' %in% input$checkboxGraphic) {
      command <- paste("mosaicplot(", input$grafik_param_y, " ~ ",sep="") 
      counter <- 1
      for(x_val in input$grafik_param_x) {
        if(counter==length(input$grafik_param_x)) {
          command <- paste(command, x_val, sep="")
        }
        else {
          command <- paste(command, x_val, "+", sep="")
        }
        counter <- counter + 1
      }
      command <- paste(command, ",data=dfGlobal, shade=TRUE)",sep= "")
      eval(parse(text=command))
    }
  })
  
  output$mosaicCode <- renderPrint({
    if('mosaic' %in% input$checkboxGraphic) {
      command <- paste("mosaicplot(", input$grafik_param_y, " ~ ",sep="") 
      counter <- 1
      for(x_val in input$grafik_param_x) {
        if(counter==length(input$grafik_param_x)) {
          command <- paste(command, x_val, sep="")
        }
        else {
          command <- paste(command, x_val, "+", sep="")
        }
        counter <- counter + 1
      }
      command <- paste(command, ",data=dfGlobal, shade=TRUE)",sep= "")
      print(command)
    }
  })
  
  ##
  # Outputs ftable of given dataset
  
  output$ftable <- renderTable({
    if('ftable' %in% input$checkboxGraphic) {
      command <- paste("ftable(dfGlobal[,c(\"", input$grafik_param_y, "\", \"", input$grafik_param_x,"\")])", sep="")
      eval(parse(text=command))
    }
  })
  
  output$ftableCode <- renderPrint({
    if('ftable' %in% input$checkboxGraphic) {
      command <- paste("ftable(dfGlobal[,c(\"", input$grafik_param_y, "\", \"", input$grafik_param_x,"\")])", sep="")
      print(command)
    }
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### DPLYR
  ## Dieser Tab ist dafür da, den Datensatz mittels dplyr zu verändern.
  ## Diese Änderungen sollen auf den Datensatz übertragen werden.
  
  ##
  # Waits for the user to press the button
  # Creates the dplyr dataset from the functions.R script
  
  dplyr_df <- eventReactive(input$dplyr_button, {
    command <-useDplyr(input$select_input, input$filter_input, input$mutate_input, input$group_by_input, input$summarise_input, input$arrange_input)
    return(command)
  })
  
  ##
  # Calls the dplyr_df function to receive the dplyr_table 
  # after pressing the button
  
  output$dplyr_table <- renderTable({
    if(input$checkboxDplyr) {
      dfGlobal <<- eval(parse(text=dplyr_df()))
    }
    eval(parse(text=dplyr_df()))
  })
  
  ##
  # Shows the dplyr code
  
  output$dplyr_tableCode <- renderPrint({
    print(paste("", dplyr_df(), sep=""))
  })
  
  ##
  # Shows the dplyr code to overwrite
  
  output$dplyr_OverwriteCode <- renderPrint({
    print(paste("dfGlobal <- ", dplyr_df(), sep=""))
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Normalverteilung Test
  ## In diesem Tab werden Konfidenzintervalle berechnet.
  ## Diese werden auf einem Violinplot dargestellt.
  
  ## 
  # Drag & Drop Menu for the violin plot with CI
  
  output$dragViolinCI <- renderUI({
    fluidRow(
      column(
        width = 12,
        bucket_list(
          header = "Spaltenauswahl",
          group_name = "violinCI_group",
          orientation = "vertical",
          add_rank_list(
            text = "Alle Parameter",
            labels = getHeader(dfGlobal),
            input_id = "violinCI_param_untaken"
          ),
          add_rank_list(
            text = "Abhängige Variable (Output) (nur eine)",
            labels = NULL,
            input_id = "violinCI_param_y"
          ),
          add_rank_list(
            text = "Unabhängige Variablen (Regressoren)",
            labels = NULL,
            input_id = "violinCI_param_x"
          )
        )
      )
    )
  })
  
  
  ## 
  # shows a ggplot - violinplot with confidence intervals

  output$violinCI <- renderPlot({
    req(input$violinCI_param_y)
    if(length(input$violinCI_param_x) == 0) {
      Stats <- getViolinCI(dfGlobal,1, input$violinCI_param_y)
      ggplot() + geom_violin(data = dfGlobal, mapping = aes(x=1,y=dfGlobal[,input$violinCI_param_y], fill="lightblue", color="lightblue")) +
        geom_point(mapping = aes(x=1, y=Mean), data = Stats) +
        geom_errorbar(mapping = aes(x = 1, ymin = CI_L, ymax = CI_U), 
                      data = Stats, width = 0.2)
    }
    else {
      x_variable <- input$violinCI_param_x[1]
      Stats <- getViolinCI(dfGlobal, x_variable, input$violinCI_param_y)
      ggplot() + geom_violin(data = dfGlobal, mapping = aes(x=dfGlobal[,x_variable], y=dfGlobal[,input$violinCI_param_y], fill="lightblue", color="lightblue")) +
        geom_point(mapping = aes(x=levels(dfGlobal[,x_variable]), y=Mean), data = Stats) +
        geom_errorbar(mapping = aes(x = levels(dfGlobal[,x_variable]), ymin = CI_L, ymax = CI_U), 
                      data = Stats, width = 0.2)

    }
  })
  

  
  ##
  # Waits for interaction to drop menu x variable
  # Returns number of factor levels
  
  checkFactor <- eventReactive(input$violinCI_param_x,{
    length <- length(input$violinCI_param_x)
    if(length == 1) {
      return(nlevels(dfGlobal[,input$violinCI_param_x]))
    }else if(length > 1) {
      return(-1)
    }else {return(0)}
  })
    
  ##
  # Plots one qq-plot if none factor variable given
  # Plots two qq-plots if a factor with two values / levels given
  # Plots boxplots if a factor with more than two values / levels given

  output$qqDistribution <- renderPlot({
    req(input$violinCI_param_y)
    if(checkFactor() == 0) {
      plot <- ggqqplot(dfGlobal, x = input$violinCI_param_y,
               color = "darkblue",
               ggtheme = theme_pubclean())
      plot
    }
    else if(checkFactor() == 2) {
      x_variable <- input$violinCI_param_x[1]
      newDF1 <- dfGlobal %>%
                filter(dfGlobal[,x_variable] == levels(dfGlobal[,x_variable])[1])
      plot1 <- ggqqplot(newDF1, x = input$violinCI_param_y, 
                       color="darkblue",
                       ggtheme = theme_pubclean())
      newDF2 <- dfGlobal %>%
        filter(dfGlobal[,x_variable] == levels(dfGlobal[,x_variable])[2])
      plot2 <- ggqqplot(newDF2, x = input$violinCI_param_y, 
                        color="darkblue",
                        ggtheme = theme_pubclean())
      grid.arrange(grobs=list(plot1,plot2), ncol=2)
    }
    else if(checkFactor() > 2) {
      x_variable <- input$violinCI_param_x[1]
      colors_vec <- brewer.pal(checkFactor(), name = 'Dark2')
      ggplot(data=dfGlobal, aes(x= dfGlobal[,x_variable], y= dfGlobal[,input$violinCI_param_y])) +
        geom_boxplot() + scale_fill_manual(values=c(colors_vec))
    }
    else if(checkFactor() == -1) {
      x_variable <- input$violinCI_param_x[1]
      colors_vec <- brewer.pal(checkFactor(), name = 'Dark2')
      ggplot(data=dfGlobal, aes(x= dfGlobal[,x_variable], y= dfGlobal[,input$violinCI_param_y])) +
        geom_boxplot() + scale_fill_manual(values=c(colors_vec))
    }
  })
  
  
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Hypothesentests - Mittelwerte
  ##

  output$hypoMeanSidebar <- renderUI({
    if(input$normDistributionRadio == "norm") {
      if(checkFactor() == 0) {
        box(title = h4("t-Test", style = "display:inline; font-weight:bold"),
            textInput("hypoMeanNoFactor", "Gib einen Mittelwert für die Hypothese ein."),
            radioButtons("alternativeRadio", "Was sind die Alternatives?",
                         choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"))
        
      }
      else if(checkFactor() == 2) {
        box(title = h4("t-Test", style = "display:inline; font-weight:bold"),
            radioButtons("pairedRadio", "Sollen die Daten gepaart verglichen werden?",
                         choices = c(Ja = "TRUE", Nein = "FALSE"), selected="TRUE"),
            radioButtons("alternativeRadio", "Was sind die Alternatives?",
                         choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"))
          
      }
      else if(checkFactor() == -1){
        h4("Bitte ein lineares Regressionsmodell durchführen.")
      }
    }
    else {
      if(checkFactor() == 0) {
        box(title = h4("Wilcoxon-Test", style = "display:inline; font-weight:bold"),
            textInput("hypoMeanNoFactor", "Gib einen Mittelwert für die Hypothese ein."),
            radioButtons("alternativeRadio", "Was sind die Alternatives?",
                         choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"))
        
      }
      else if(checkFactor() == 2) {
        box(title = h4("Wilcoxon-Test", style = "display:inline; font-weight:bold"),
            radioButtons("pairedRadio", "Sollen die Daten gepaart verglichen werden?",
                         choices = c(Ja = "TRUE", Nein = "FALSE"), selected="TRUE"),
            radioButtons("alternativeRadio", "Was sind die Alternatives?",
                         choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"))
        
        
      }
      else if(checkFactor() == -1){
        h4("Bitte ein lineares Regressionsmodell durchführen.")
      }
    }
  })
  
  ttest <- reactive({
    if(checkFactor() == 0) {
      req(input$hypoMeanNoFactor)
      result <- t.test(x=dfGlobal[,input$violinCI_param_y],y=NULL, mu=as.numeric(input$hypoMeanNoFactor), alternative = input$alternativeRadio)
    }
    else if(checkFactor() == 2) {
      result <- t.test(x=as.numeric(dfGlobal[,input$violinCI_param_x]), y= dfGlobal[,input$violinCI_param_y], paired=as.logical(input$pairedRadio), alternative=input$alternativeRadio)
    }
    else if(checkFactor() > 3) {
      result <- aov(dfGlobal[,input$violinCI_param_y] ~ dfGlobal[,input$violinCI_param_x], data=dfGlobal)
    }
    return(result)
  })
  
  wilcoxtest <- reactive({
    if(checkFactor() == 0) {
      req(input$hypoMeanNoFactor)
      result <- wilcox.test(x=dfGlobal[,input$violinCI_param_y],y=NULL, mu=as.numeric(input$hypoMeanNoFactor), alternative = input$alternativeRadio)
    }
    else if(checkFactor() == 2) {
      result <- wilcox.test(x=as.numeric(dfGlobal[,input$violinCI_param_x]), y= dfGlobal[,input$violinCI_param_y], paired=as.logical(input$pairedRadio), alternative=input$alternativeRadio)
    }
    else if(checkFactor() > 3) {
      result <- kruskal.test(dfGlobal[,input$violinCI_param_y] ~ dfGlobal[,input$violinCI_param_x], data=dfGlobal)
    }
    return(result)
  })
  
  output$hypothesisMeanResult <- renderPrint({
    result <- NULL
    if(checkFactor() != -1) {
      if(input$normDistributionRadio == "norm")
        result <- ttest()
      else {
        result <- wilcoxtest()
      }
      print(result)
    }
  })
  
  output$hypothesisMean <- renderUI({
    verbatimTextOutput("hypothesisMeanResult")
  })
  
  output$hypothesisMeanSummary <- renderUI({
    if(checkFactor() > 2) {
      if(input$normDistributionRadio == "norm") {
        verbatimTextOutput("hypothesisSummaryPrint")
      }
    }
  })
  
  output$hypothesisSummaryPrint <- renderPrint({
    result <- ttest()
    print(summary(result))
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Hypothesentests - Proportionen
  # 
  
  makePropSidebar <- reactive({
    boxes <- list()
    if(checkFactor() == 0) {
      boxes[[1]] <- box(title = h4("Prop-Test", style = "display:inline; font-weight:bold"),
                        radioButtons("alternativePropRadio", "Was sind die Alternatives?",
                                     choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"),
                        textInput("propTestInput_1", "Gib einen Anteil für die Hypothese ein.")
      )
    }
    else if(checkFactor() == 2) {
      boxes[[1]] <- box(title = h4("Prop-Test", style = "display:inline; font-weight:bold"),
                        radioButtons("alternativePropRadio", "Was sind die Alternatives?",
                                     choices = c(two.sided = "two.sided", greater = "greater", less="less"), selected="two.sided"))
      
      for(i in 1:2) {
        boxes[[i+1]] <- box(title = h4(factor(dfGlobal[,input$violinCI_param_x])[i], style = "display:inline; font-weight:bold"), 
                            textInput(paste("propTestInput_", i, sep=""),"Bitte einen Wert eingeben."))
      }
      
    }
    else if(checkFactor() > 2) {
      
    }
    return(boxes)
  })
  
  output$hypoPropSidebar <- renderUI(makePropSidebar())
  
  output$hypothesisProp <- renderPrint({
    req(input$violinCI_param_y)
    values <- c()
    rows <- c()
    if(length(input$violinCI_param_x) > 0) {
      for(i in 1:nlevels(dfGlobal[,input$violinCI_param_x])) {
        values <- append(values, as.numeric(eval(parse(text=paste("input$propTestInput_",i, sep="")))))
        newDF <- eval(parse(text="dfGlobal %>% filter(dfGlobal[,input$violinCI_param_x] == as.character(factor(dfGlobal[,input$violinCI_param_x])[i]))"))
        rows <- append(rows, nrow(newDF))
      }
    }
    else {
      values <- append(values, as.numeric(input$propTestInput_1))
      rows <- append(rows, nrow(dfGlobal))
    }
    req(values)
    print(prop.test(values, rows, alternative = input$alternativePropRadio))
    print(str(prop.test(values, rows, alternative = input$alternativePropRadio)))
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  ### Hypothesentests - Verteilung
  ##
  
  output$hypothesisDist <- renderPrint({
    req(input$violinCI_param_x)
    req(input$violinCI_param_y)
    if(checkFactor() > 2) {
      table <- table(dfGlobal[,input$violinCI_param_x], dfGlobal[,input$violinCI_param_y])
      chisq.test(table)
    }
  })
  
  output$hypoDistMosaic <- renderPlot({
    req(input$violinCI_param_x)
    req(input$violinCI_param_y)
    if(checkFactor() > 2) 
      mosaicplot(~ dfGlobal[,input$violinCI_param_y] + dfGlobal[,input$violinCI_param_x], data=dfGlobal, shade=TRUE) 
  })
  
})
