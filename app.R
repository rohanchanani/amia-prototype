####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################


# Import libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tools)
library(shiny)
library(shinythemes)
library(shinyjs)


questions_df <- read_csv("https://raw.githubusercontent.com/rohanchanani/ApprenticeshipKMS/main/questions.csv")

oneClassify <- function(length) {
  if(length < 150) {
    return("short")
  }
  if(length<350) {
    return("medium")
  }
  return("long")
}

classify <- function(column) {
  sapply(column, oneClassify)
}

calcMean <- function(array) {
  if (length(array) != 0) {
    return(mean(array))
  } else {
    return(0)
  }
}

createCategorical <- function(strata1, strata2) {
  
  if(strata1 == 'type' | strata2 == 'type') {
    questions_df <- questions_df %>% mutate(type=classify(nchar(question)+nchar(answer)))
  }
  values1 <- questions_df %>% distinct(!!as.symbol(strata1)) %>% pull(!!as.symbol(strata1))
  outputTable <- data.frame(values1)
  names(outputTable)[1] <- strata1
  values2 <- questions_df %>% distinct(!!as.symbol(strata2)) %>% pull(!!as.symbol(strata2))
  for (col in 1:length(values2)) {
    newCol <- c()
    for (row in 1:length(values1)) {
      currentVal <- questions_df %>% filter(!!as.symbol(strata1)==values1[row], !!as.symbol(strata2)==values2[col]) %>% nrow()
      newCol[row] <- currentVal
    }
    outputTable[values2[col]] <- newCol
  }
  return(outputTable)
}

createNumeric <- function(strata1, strata2, character='') {
  character <- substr(character, 1, 1)
  
  if(strata1 == 'type' | strata2=='type') {
    questions_df <- questions_df %>% mutate(type=classify(nchar(question)+nchar(answer)))
  }
  if (character == '') {
    questions_df <- questions_df %>% mutate(length=nchar(question)+nchar(answer))
    metric <- 'length'
  } else {
    questions_df <- questions_df %>% mutate(occurences=str_count(paste(question, answer, sep=""), character))
    metric <- 'occurences'
  }
  values1 <- questions_df %>% distinct(!!as.symbol(strata1)) %>% pull(!!as.symbol(strata1))
  outputTable <- data.frame(values1)
  names(outputTable)[1] <- strata1
  values2 <- questions_df %>% distinct(!!as.symbol(strata2)) %>% pull(!!as.symbol(strata2))
  for (col in 1:length(values2)) {
    newCol <- c()
    for (row in 1:length(values1)) {
      currentVal <- questions_df %>% filter(!!as.symbol(strata1)==values1[row], !!as.symbol(strata2)==values2[col]) %>% pull(!!as.symbol(metric)) %>% calcMean()
      newCol[row] <- currentVal
    }
    outputTable[values2[col]] <- newCol
  }
  return(outputTable)
}

createExpected <- function(actualOutput) {
  for (row in 1:nrow(actualOutput)) {
    avgValue <- calcMean(as.numeric(as.vector(actualOutput[row,2:ncol(actualOutput)])))
    for (col in 2:ncol(actualOutput)) {
      actualOutput[row,col] <- avgValue
    }
  }
  return(actualOutput)
}

findDifference <- function(outputTable) {
  expected <- createExpected(outputTable)
  for (row in 1:nrow(outputTable)) {
    for (col in 2:ncol(outputTable)) {
      outputTable[row,col] <- outputTable[row,col]-expected[row,col]
    }
  }
  return(outputTable)
}

makeBarGraph <- function(outputTable, strata2, numeric=FALSE, character="") {
  if (numeric) {
    if (character == "") {
      unit <- "(Average Character Length)"
    } else {
      character <- paste('"',substr(character, 1, 1), '"', sep="")
      unit <- paste('(Average Occurences of',character,')')
    }
  } else {
    unit <- "(Number of Questions)"
  }
  title <- paste(toTitleCase(strata2), "Discrepancies by", toTitleCase(colnames(outputTable)[1]))
  differenceTable <- findDifference(outputTable)
  testStrata <- c()
  totalDiscrepancy <- c()
  for (row in 1:nrow(differenceTable)) {
    testStrata[row] <- differenceTable[row,1]
    totalDiscrepancy[row] <- sum(abs(differenceTable[row,2:ncol(differenceTable)]))
  }
  finalData <- data.frame(testStrata, totalDiscrepancy)
  return(ggplot(data=finalData, aes(x=testStrata, y=totalDiscrepancy)) + geom_bar(aes(x=testStrata), stat='identity') + coord_flip() + ggtitle(title) + labs(x=toTitleCase(toTitleCase(colnames(outputTable)[1])), y=paste(toTitleCase(strata2), "Discrepancy", unit)) + theme(aspect.ratio=1))
}

ids <- c("categoricalValueTable", "categoricalExpectedTable", "categoricalDifferenceTable", "categoricalBarGraph", "numericValueTable", "numericExpectedTable", "numericDifferenceTable", "numericBarGraph")

####################################
# User interface                   #
####################################

ui <- fillPage(theme = shinytheme("united"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Sample App",
                  tabPanel("Categorical",
                           sidebarPanel(
                             useShinyjs(),
                             HTML("<h3>Input parameters</h3>"),
                             
                             selectInput("categorical1", label = "First Strata:", 
                                         choices = list("Section" = "section", "Company" = "company_name", "Type" = "type"), 
                                         selected = "Section"),
                             
                             selectInput("categorical2", label = "Second Strata:", 
                                         choices = list("Type" = "type", "Company" = "company_name", "Section" = "section"), 
                                         selected = "Type"),
                             
                             selectInput("catDisplay", label="Display:", choices=list("Actual"="categoricalValueTable", "Expected"="categoricalExpectedTable","Difference"="categoricalDifferenceTable","Graph"="categoricalBarGraph"), selected="Graph")
                           ), # sidebarPanel
                           mainPanel(
                             dataTableOutput("categoricalValueTable"),
                             dataTableOutput("categoricalExpectedTable"),
                             dataTableOutput("categoricalDifferenceTable"),
                             plotOutput("categoricalBarGraph")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Numeric",
                           sidebarPanel(
                             HTML("<h3>Input parameters</h3>"),
                             
                             selectInput("numeric1", label = "First Strata:", 
                                         choices = list("Section" = "section", "Company" = "company_name", "Type" = "type"), 
                                         selected = "Section"),
                             
                             selectInput("numeric2", label = "Second Strata:", 
                                         choices = list("Type" = "type", "Company" = "company_name", "Section" = "section"), 
                                         selected = "Type"),
                             
                             textInput("character", "Character to Count (optional):",""),
                             
                             selectInput("numDisplay", label="Display:", choices=list("Actual"="numericValueTable", "Expected"="numericExpectedTable","Difference"="numericDifferenceTable","Graph"="numericBarGraph"), selected="Graph")
                           ), # sidebarPanel
                           mainPanel(
                             dataTableOutput("numericValueTable"),
                             dataTableOutput("numericExpectedTable"),
                             dataTableOutput("numericDifferenceTable"),
                             plotOutput("numericBarGraph")
                           ) # mainPanel
                           
                  ) # Navbar 1, tabPanel
                  
              ) # navbarPage
          ) # fluidPage

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  output$categoricalValueTable <- renderDataTable({createCategorical(input$categorical1, input$categorical2)})
  output$categoricalExpectedTable <- renderDataTable({createExpected(createCategorical(input$categorical1, input$categorical2))})
  output$categoricalDifferenceTable <- renderDataTable({findDifference(createCategorical(input$categorical1, input$categorical2))})
  output$categoricalBarGraph <- renderPlot({makeBarGraph(createCategorical(input$categorical1, input$categorical2), input$categorical2)})
  
  output$numericValueTable <- renderDataTable({createNumeric(input$numeric1, input$numeric2, input$character)})
  output$numericExpectedTable <- renderDataTable({createExpected(createNumeric(input$numeric1, input$numeric2, input$character))})
  output$numericDifferenceTable <- renderDataTable({findDifference(createNumeric(input$numeric1, input$numeric2, input$character))})
  output$numericBarGraph <- renderPlot({makeBarGraph(createNumeric(input$numeric1, input$numeric2, input$character), input$numeric2, TRUE, input$character)})
  
  
  observe({
    for (test in 1:length(ids)) {
      if (input$catDisplay == ids[test] | input$numDisplay == ids[test]) {
        show(ids[test])
      } else {
        hide(ids[test])
      }
    }
  })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)