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
source("functions.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  
  navbarPage("Lazy Data Scientist",
             # Sidebar layout with input and output definitions ----
             tabPanel("Upload",
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
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Data file ----
                          tabsetPanel(
                            tabPanel("Tabelle", tableOutput("contents")),
                            tabPanel("Pic", imageOutput("pic")),
                            tabPanel("More Table", textOutput("dataTest"))
                          )
                          
                          
                          
                        ))),
             tabPanel("Exploration", sidebarLayout(
               
               sidebarPanel(
                 uiOutput("dragExploration"),
                 checkboxGroupInput("checkGroup", label = h3("Lagedaten"),
                                    choices = list("Median" = "median",
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
                 tableOutput("value")
                 # p("Legende"),
                 # tags$ul(
                 #     tags$li("median = Mittelwert "),
                 #     tags$li("mean = Durchschnitt"),
                 #     tags$li("sd = Standard Deviation = Standardabweichung"),
                 #     tags$li("IQR = interquartile range = Interquartilsabstand"),
                 #     tags$li("mad = median absolute deviation = Mittlere absolute Abweichung vom Median"),
                 #     tags$li("var = variance = Varianz ")
                 #)
               ))),
             tabPanel("Grafik",  sidebarLayout(
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
                 h2("Scatterplot"),
                 tags$b("Description"),
                 p("Is a graphical plot of the value pairs of two statistical characteristics, it will be shown in a cartesian coordinate system which results in a point cloud diagramm."),
                 tags$b("Requirements"),
                 p("Only numerical values are allowed for this plot!"),
                 plotOutput("plotScatterplot"),
                 h2("Barplot"),
                 tags$b("Description"),
                 p("It shows the relationship between a numeric and a categoric variable. Each entity of the categoric variable is represented as a bar. The size of the bar represents its numeric value."),
                 tags$b("Requirements"),
                 p("For this plot a numerical as well as a categorical values is needed."),
                 plotOutput("plotBarplot"),
                 h2("Lineplot"),
                 tags$b("Description"),
                 p("Is a graphical plot of the functional context of two or three characteristics in a linear shape."),
                 tags$b("Requirements"),
                 p("It is recommended to use a time variable for the x-axis, to see the progress over the time."),
                 plotOutput("plotLineplot"),
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
                 h2("QQ"),
                 tags$b("Description"),
                 p("A Q-Q plot is a scatterplot created by plotting two sets of quantiles against one another."),
                 tags$b("Requirements"),
                 p("Numerical values are recommended for this kind of plot!"),
                 plotOutput("plotQQ"),
                 h2("Violin"),
                 tags$b("Description"),
                 p("A violin plot is a method of plotting numeric data, they are similar to box plots, except that they also show the probability density of the data at different values, usually smoothed by a kernel density estimator."),
                 tags$b("Requirements"),
                 p("Also here are numerical values recommended!"),
                 plotOutput("plotViolin"),
                 h2("ECDF"),
                 tags$b("Description"),
                 p("Function. The ECDF essentially allows you to plot a feature of your data in order from least to greatest and see the whole feature as if is distributed across the data set."),
                 tags$b("Requirements"),
                 p("Again use numerical values for this kind of plot!"),
                 plotOutput("plotECDF"),
                 h2("Mosaicplot"),
                 tags$b("Description"),
                 p("Is a graphical method for visualizing data from two or more qualitative variables. It is the multidimensional extension of spineplots, which graphically display the same information for only one variable."),
                 tags$b("Requirements"),
                 p("In this case categorial values are recommended but numerical should work as well."),
                 plotOutput("plotMosaicplot"),
                 h2("Table"),
                 tags$b("Description"),
                 p("This one is not really a plot, it is more likely a table where the values are easier to read."),
                 tags$b("Requirements"),
                 p("There souldnt be any requirements for this option."),
                 textOutput("ftable")
                 
               )
             )),
             tabPanel("Regression", sidebarLayout(
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
                 tabsetPanel(
                   tabPanel("Korrelation", plotOutput("plotCorrelation"),
                            radioButtons(inputId = "radioCorrelation", label = "Download as", choices = list(".png",".pdf")),
                            downloadButton(outputId = "downloadCorrelation", label = "Download correlation plot"),
                            p(htmlOutput("correlationHelp")),
                            tags$style("#correlationHelp{color: red; font-size: 22px; font-style: bold }")
                   ),
                   tabPanel("Regression",
                            h2("Summary"),
                            p("Is a generic function used to produce result summaries of the results of various model fitting functions."),
                            verbatimTextOutput("regressionSummary"),
                            plotOutput("plotRegression"),
                            radioButtons(inputId = "radioRegression", label = "Download as", choices = list(".png",".pdf")),
                            downloadButton(outputId = "downloadRegression", label = "Download regression plot")
                   )
                 ),
               )
             )),
             tabPanel("Umwandlung", sidebarLayout(
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
                 
                 div(checkboxInput("checkboxDplyr", "Transformation", value = FALSE, width = NULL), style ="font-size: 20px")
               ),
               # Main panel for displaying outputs ----
               mainPanel(
                 tableOutput("dplyr_table")
               )
             ))
  )
)
)
