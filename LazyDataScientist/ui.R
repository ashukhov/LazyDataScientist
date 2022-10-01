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
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)
source("functions.R")

ui <- dashboardPage(skin="purple",
    dashboardHeader(title = "Lazy Data Scientist"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Upload", tabName = "upload", icon = icon("file-upload")),           
            menuItem("Data frame", tabName = "dataframe", icon = icon("list-alt", lib="glyphicon")),
            menuItem("Filterung", tabName = "transformation", icon = icon("exchange-alt")),
            menuItem("Exploration", tabName = "exploration", icon = icon("stats", lib="glyphicon")),
            menuItem("Graphik", tabName = "graphic", icon = icon("chart-bar")),
            menuItem("Regression", tabName = "regression", icon = icon("chart-line"), 
                     menuItem("Correlation", tabName="correlation"),
                     menuItem("Model", tabName="model"),
                     menuItem("Prediction", tabName="prediction")),
            menuItem("Normalverteilung Test", tabName="normDistribution", icon= icon("bell")),
            menuItem("Hypothesen Testing", tabName="hypotest", icon=icon("cogs"),
                     menuItem("Mittelwerte", tabName="hypoMean"),
                     menuItem("Proportionen", tabName="hypoProp"),
                     menuItem("Distribution", tabName="hypoDist"))
        )
    ),
    dashboardBody(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
        tabItems(
            # First tab content
            tabItem(tabName = "upload",
                    h2("Upload tab content"),
                    sidebarLayout(
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                            
                            # Input: Select a file ----
                            fileInput("file1", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Checkbox if file has header ----
                            checkboxInput("header", "Header", TRUE),
                            
                            checkboxInput("df_omit", "Omit NAs", FALSE),
                            
                            radioButtons("omit_replace", "Omitting or Replacing",
                                         choices = c(omit = "omit",
                                                     exclude = "exclude",
                                                     replace = "replace"),
                                         selected="omit"),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            
                            # Input: Select quotes ----
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"'),
                            tags$b("Code"),
                            verbatimTextOutput("readCSVCode")
                            
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                            # Output: Data file ----
                            DTOutput("contents"),
                            h4("Code"),
                            verbatimTextOutput("contentsCode")
                        ))
            ),
            
            tabItem(tabName = "dataframe",
                    h2("Data frame's structure"),
                    
                        # Main panel for displaying outputs ----
                        fluidRow(
                            column(12, 
                                h3("Datensatz Struktur"),
                                verbatimTextOutput("structureDF"),
                                h4("Code"),
                                verbatimTextOutput("structureDFCode")
                            ),
                            column(12, 
                                h3("Datensatz Summary"),
                                verbatimTextOutput("summaryDF"),
                                h4("Code"),
                                verbatimTextOutput("summaryDFCode")
                            )
                        )
            ),
            
            # Second tab content
            tabItem(tabName = "exploration",
                    h2("Exploration tab content"),
                    sidebarLayout(
                        
                        sidebarPanel(
                            uiOutput("dragExploration"),
                            checkboxGroupInput("checkGroup", label = h3("Lagedaten"),
                                               choices = list("Max" = "max", 
                                                              "Min" = "min",
                                                              "Median" = "median",
                                                              "Mean" = "mean",
                                                              "SD" = "sd",
                                                              "IQR" = "IQR",
                                                              "MAD" = "mad",
                                                              "VAR" = "var"),
                                               selected = 1),
                            bsPopover("checkGroup", title = "Legend", content = "SD ... <i>Standard deviation</i><br>IQR ... <i>Interquartile range</i><br>MAD ... <i>Median absolute deviation</i><br>VAR ... <i>Variance</i>",
                                      placement = "bottom", trigger = "hover")
                            
                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(
                            tableOutput("value"),
                            tableOutput("explorationCode")
                        ))
            ),
            
            # Third tab content
            tabItem(tabName = "graphic",
                    h2("Graphic tab content"),
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("dragGrafik"),
                            h3("Plots"),
                            checkboxGroupInput("checkboxGraphic", "Choose graph",
                                               choiceNames =
                                                   list("Histogramm", "Scatterplot", "Barplot", "Line",
                                                        "Boxplot", "QQ-Plot", "Violinplot",
                                                        "ECDF", "Mosaicplot", "Ftable"),
                                               choiceValues =
                                                   list("histogram", "scatter", "barplot", "line",
                                                        "boxplot", "qq", "violin",
                                                        "ecdf", "mosaic", "ftable")
                            ),
                            h3("Graphic Settings"),
                            checkboxGroupInput("graphic_settings", "Choose graphic settings",
                                               choiceNames = 
                                                   list("Logarthmic X", "Logarithmic Y"),
                                               choiceValues =
                                                   list("scale_x_log","scale_y_log"))
                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(
                            h2("Histogram"),
                            tags$b("Description"),
                            p("Is a graphical plot of a frequency distributuon in pillar shapes, these frequencies correspond to the measurements."),
                            tags$b("Requirements"),
                            p("Only numerical values are allowed for this plot!"),
                            plotOutput("plotHistogram"),
                            verbatimTextOutput("histogramCode"),
                            
                            h2("Scatterplot"),
                            tags$b("Description"),
                            p("Is a graphical plot of the value pairs of two statistical characteristics, it will be shown in a cartesian coordinate system which results in a point cloud diagramm."),
                            tags$b("Requirements"),
                            p("Only numerical values are allowed for this plot!"),
                            plotOutput("plotScatterplot"),
                            verbatimTextOutput("scatterplotCode"),
                            
                            h2("Barplot"),
                            tags$b("Description"),
                            p("It shows the relationship between a numeric and a categoric variable. Each entity of the categoric variable is represented as a bar. The size of the bar represents its numeric value."),
                            tags$b("Requirements"),
                            p("For this plot a numerical as well as a categorical values is needed."),
                            plotOutput("plotBarplot"),
                            verbatimTextOutput("barplotCode"),
                            
                            h2("Lineplot"),
                            tags$b("Description"),
                            p("Is a graphical plot of the functional context of two or three characteristics in a linear shape."),
                            tags$b("Requirements"),
                            p("It is recommended to use a time variable for the x-axis, to see the progress over the time."),
                            plotOutput("plotLineplot"),
                            verbatimTextOutput("lineplotCode"),
                            
                            h2("Boxplot"),
                            tags$b("Description"),
                            p("Is a graphical plot which sums up a dataset in five different points which are the following ones:"),
                            tags$ul(
                                tags$li("Minima"),
                                tags$li("Bottom quartil"),
                                tags$li("Median"),
                                tags$li("Top quartil"),
                                tags$li("Maxima"),
                            ),
                            tags$b("Requirements"),
                            p("This kind of plot is mostly useful for unimodale Data. The reason therefor is that otherwise the points, mentioned above, will get weird values."),
                            plotOutput("plotBoxplot"),
                            verbatimTextOutput("boxplotCode"),
                            
                            h2("QQ"),
                            tags$b("Description"),
                            p("A Q-Q plot is a scatterplot created by plotting two sets of quantiles against one another."),
                            tags$b("Requirements"),
                            p("Numerical values are recommended for this kind of plot!"),
                            plotOutput("plotQQ"),
                            verbatimTextOutput("QQCode"),
                            
                            h2("Violin"),
                            tags$b("Description"),
                            p("A violin plot is a method of plotting numeric data, they are similar to box plots, except that they also show the probability density of the data at different values, usually smoothed by a kernel density estimator."),
                            tags$b("Requirements"),
                            p("Also here are numerical values recommended!"),
                            plotOutput("plotViolin"),
                            verbatimTextOutput("violinCode"),
                            
                            h2("ECDF"),
                            tags$b("Description"),
                            p("Function. The ECDF essentially allows you to plot a feature of your data in order from least to greatest and see the whole feature as if is distributed across the data set."),
                            tags$b("Requirements"),
                            p("Again use numerical values for this kind of plot!"),
                            plotOutput("plotECDF"),
                            verbatimTextOutput("ECDFCode"),
                            
                            h2("Mosaicplot"),
                            tags$b("Description"),
                            p("Is a graphical method for visualizing data from two or more qualitative variables. It is the multidimensional extension of spineplots, which graphically display the same information for only one variable."),
                            tags$b("Requirements"),
                            p("In this case categorial values are recommended but numerical should work as well."),
                            plotOutput("plotMosaicplot"),
                            verbatimTextOutput("mosaicCode"),
                            
                            h2("Table"),
                            tags$b("Description"),
                            p("This one is not really a plot, it is rather a table where the values are easier to read."),
                            tags$b("Requirements"),
                            p("There souldnt be any requirements for this option."),
                            tableOutput("ftable"),
                            verbatimTextOutput("ftableCode")
                            
                        )
                    )
            ),
            
            # Fifth tab content
            tabItem(tabName = "correlation",
                    h2("Plot tab content"),
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("dragRegression"),
                            div(checkboxInput("checkboxRegression", "Plotten", value = FALSE, width = NULL), style ="font-size: 20px"),
                            div(checkboxInput("checkboxSummary", "Summary", value=FALSE, width=NULL), style ="font-size: 20px")
                            #p(),
                            #radioButtons(inputId = "radio", label = "Download as", choices = list(".png",".pdf")),
                            #downloadButton(outputId = "download", label = "Download the plot")
                            
                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(
                            h3("Correlogram"),
                            plotOutput("plotCorrelogram"),
                            h4("Code"),
                            verbatimTextOutput("correlogramCode"),
                            h3("Correlation Pairwise Plot"),
                            plotOutput("plotCorrelation"),
                            radioButtons(inputId = "radioCorrelation", label = "Download as", choices = list(".png",".pdf")),
                            downloadButton(outputId = "downloadCorrelation", label = "Download correlation plot"),
                            p(htmlOutput("correlationHelp")),
                            tags$style("#correlationHelp{color: red; font-size: 22px; font-style: bold }")
                        )
                    )
            ),
            tabItem(tabName="model",
                    h2("Regression"),
                    h3("Summary"),
                    p("Is a generic function used to produce result summaries of the results of various model fitting functions."),
                    verbatimTextOutput("regressionSummary"),
                    h3("Model"),
                    plotOutput("plotRegression"),
                    h4("Code"),
                    verbatimTextOutput("regressionCode"),
                    radioButtons(inputId = "radioRegression", label = "Download as", choices = list(".png",".pdf")),
                    downloadButton(outputId = "downloadRegression", label = "Download regression plot")
            ),
            tabItem(tabName="prediction",
                uiOutput("select_prediction"),
                verbatimTextOutput("prediction_y_value")
            ),
            # Seventh tab content
            tabItem(tabName = "transformation",
                    h2("Filterung"),
                    sidebarLayout(
                        sidebarPanel(
                            textInput("select_input",label="Select"),
                            bsPopover(id = "select_input", title ="<b>Help: Select</b>", 
                                      content = "Reduces the dataframe to the given columns<br><br>Example: <i>Column1, Column2, Column4</i>",
                                      placement = "top", trigger = "hover"),
                            
                            textInput("filter_input", label="Filter"),
                            bsPopover(id = "filter_input", title ="<b>Help: Filter</b>",
                                      content = "Returns rows where the given condition is met<br><br>Example: <i>Column1 == 20</i>",
                                      placement = "top", trigger = "hover"),
                            
                            textInput("mutate_input", label="Add Columns"),
                            bsPopover(id = "mutate_input", title ="<b>Help: Add Columns</b>",
                                      content = "Modifies existing columns or adds new ones<br><br>Examples:<br><i>1. newColumn = Column2 / Value</i><br><i>2. Column2 = Column2 / Value</i>",
                                      placement = "top", trigger = "hover"),
                            
                            textInput("group_by_input", label="Group by"),
                            bsPopover(id = "group_by_input", title ="<b>Help: Group by</b>",
                                      content = "Before summarise: turns groups with multiple entries into one row each.<br>group_by on its own does <b>not</b> change the look of the dataframe!<br><br>Example: <i>Column1, ...</i>",
                                      placement = "top", trigger = "hover"),
                            
                            textInput("summarise_input", label="Summarise Rows"),
                            bsPopover(id = "summarise_input", title ="<b>Help: Summarise Rows</b>",
                                      content = "Reduces multiple values down to a single value via mathematical functions<br><br>Example: <i>mean(Column1)</i>",
                                      placement = "top", trigger = "hover"),
                            
                            textInput("arrange_input", label="Arrange"),
                            bsPopover(id = "arrange_input", title ="<b>Help: Arrange</b>",
                                      content = "Arranges the dataframe in ascending or descending order (Default: ASC)<br><br>Example: <i>desc(Column1)</i>",
                                      placement = "top", trigger = "hover"),
                            
                            actionButton("dplyr_button", "Änderungen anzeigen"),
                            h4("Code"),
                            verbatimTextOutput("dplyr_tableCode"),
                            div(checkboxInput("checkboxDplyr", "Datensatz überschreiben", value = FALSE, width = NULL), style ="font-size: 20px"),
                            h4("Code"),
                            verbatimTextOutput("dplyr_OverwriteCode")
                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(
                            tableOutput("dplyr_table")
                        )
                    )
            ),
            tabItem(tabName = "normDistribution", 
                    h2("Distribution Test"),
                    sidebarLayout(
                        sidebarPanel(uiOutput("dragViolinCI")),
                        mainPanel(
                            plotOutput("violinCI"),
                            plotOutput("qqDistribution"),
                            radioButtons("normDistributionRadio", "Sind die Daten normalverteit?",
                                         choices = c(Ja = "norm", Nein = "notNorm")),
                            bsPopover(id="normDistributionRadio", title="<h4><b>Sind die Daten normalverteilt?</b></h4>",
                                      content="<b>Ja</b>: <br /> * Student t-Test <br /> * ANOVA <br /> * Lineares Modell <br /> <b>Nein</b>: <br /> * Wilcoxon-Test <br /> * Kruskal-Wallis-Test",
                                      placement = "top", trigger="hover")
                        )
                    )
            ),
            tabItem(tabName = "hypoMean",
                    h2("Hypothesen Testing - Mittelwerte"),
                    fluidRow(
                        column(12, 
                               uiOutput("hypoMeanSidebar")
                        ),
                        column(12,
                               uiOutput("hypothesisMean")
                        ),
                        column(12,
                               uiOutput("hypothesisMeanSummary")
                        )
                    )
            ),
            tabItem(tabName = "hypoProp",
                    h2("Hypothesen Testing - Proportionen"),
                    fluidRow(
                        column(12,
                               uiOutput("hypoPropSidebar"),
                        column(12,
                               verbatimTextOutput("hypothesisProp")))
                    )
            ),
            tabItem(tabName = "hypoDist",
                    h2("Hypothesen Testing - Verteilung"),
                    fluidRow(
                           column(12,
                                  verbatimTextOutput("hypothesisDist")
                            ),
                           column(12,
                                  plotOutput("hypoDistMosaic"))
                    )
            )
)),footer = dashboardFooter(
    left = "Lazy Data Scientist 1.0 https://github.com/ashukhov-tgm/LazyDataScientist",
    right = "Copyright (c) 2020 Arseniy Shukhov under GNU GPLv3 licence"
)
)
