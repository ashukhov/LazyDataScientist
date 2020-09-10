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

getHeader <- function(data) {
  return(colnames(data))
}



createLinearRegression <- function(df, dependent, independent) {
  textReg <- ""
  counter <- 1
  variables <- ""
  for(val in independent) {
    if(counter == length(independent)) {
      variables <- paste(variables, val, sep="")
    }
    else {
      variables <- paste(variables, val, " + ",sep="")
      
    }
    counter <- counter + 1
  }
  textReg <- paste("lm(", dependent, "~", variables, ", data=df)",sep="")
  modell <- eval(parse(text=textReg))
  return(modell)
}


panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}



panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r, col = if(r <= 0.25) {col = "gray40"}else if(r >= 0.25 && r <= 0.5){col = "gray0"}else if(r >= 0.5 && r <= 0.75){col = "firebrick"}else {col = "firebrick1"})
}



makeCorrelationPlot <- function(dependent, independent) {
  
  params <- "dfGlobal[,c(\""
  params <- paste(params,dependent, "\",\"",sep="")
  counter <- 1
  for(val in independent) {
    if(counter == length(independent)) {
      params <- paste(params, val, "\"",sep="")
    }
    else {
      params <- paste(params, val, "\",\"", sep="")
    }
    counter <- counter + 1
  }
  params <- paste(params, ")]",sep="")
  params <- paste("pairs(", params, ",diag.panel = panel.hist,lower.panel = panel.smooth, upper.panel = panel.cor,
            gap=0, row1attop=FALSE)",sep="")
  
  return(params)
}


makeGGPlot <- function(independent, dependent, color_variable=NULL, facet_variable=NULL, color=FALSE, facet_wrap = FALSE, scale_x_log= FALSE, scale_y_log=FALSE) {
  command <- "ggplot(dfGlobal, aes(x="
  command <- paste(command, "dfGlobal[,\"", independent,"\"], y=dfGlobal[,\"", dependent, "\"]",sep="")
  if(color==TRUE) {
    command <- paste(command, ",color=", color_variable, ", fill=\"#add8e6\"))", sep="")
  }
  else {
    command <- paste(command, ", fill=\"#add8e6\"))", sep="")
  }
  if(facet_wrap == TRUE) {
    command <- paste(command, " + facet_wrap(~", facet_variable, ")", sep="")
  }
  if(scale_x_log == TRUE) {
    command <- paste(command, "+ scale_x_log10()")
  }
  if(scale_y_log == TRUE) {
    command <- paste(command, "+ scale_y_log10()")
  }
  return(command)
}


useDplyr <- function(select_input, filter_input, mutate_input, group_by_input, summarise_input, arrange_input) {
  command <- "dfGlobal"
  if(select_input != "") 
    command <- paste(command, " %>% dplyr::select(",select_input, ")", sep="")
  if(filter_input != "") 
    command <- paste(command, " %>% filter(", filter_input, ")", sep="")
  if(mutate_input != "") 
    command <- paste(command, " %>% mutate(", mutate_input, ")", sep="")
  if(group_by_input != "")
    command <- paste(command, " %>% group_by(", group_by_input, ")", sep="")
  if(summarise_input != "") 
    command <- paste(command, " %>% summarise(", summarise_input, ")", sep="")
  if(arrange_input != "") 
    command <- paste(command, " %>% arrange(", arrange_input, ")", sep="")
  return(command)
}

## 
# calculates confidence intervals for violin plot 
# and returns new data set

getViolinCI <- function(dfGlobal, x, y) {
  if(x!= 1) {
    Stats <- dfGlobal %>% group_by(dfGlobal[,x]) %>% summarize(Mean = mean(dfGlobal[,y]), SD = sd(dfGlobal[,y]),
                                                                                  CI_L = Mean - (SD * 1.96)/sqrt(nrow(dfGlobal)),
                                                                                  CI_U = Mean + (SD * 1.96)/sqrt(nrow(dfGlobal)))
  }
  else {
    Stats <- dfGlobal %>% group_by(1) %>% summarize(Mean = mean(dfGlobal[,y]), SD = sd(dfGlobal[,y]),
                                                               CI_L = Mean - (SD * 1.96)/sqrt(nrow(dfGlobal)),
                                                               CI_U = Mean + (SD * 1.96)/sqrt(nrow(dfGlobal)))
  }
  return(Stats)
}

