library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tools)
library(Dict)
library(labelled)
library(gsubfn)
library(shiny)
library(shinythemes)
library(shinyjs)


questions_df <- read_csv("https://raw.githubusercontent.com/rohanchanani/ApprenticeshipKMS/main/questions.csv")
specifications <- c("Setting", "Campus")

companies <- questions_df %>% distinct(company_name) %>% pull(company_name)
company_replacements <- c("30305", "30306", "30307", "30308", "30309")
sections <- questions_df %>% distinct(section) %>% pull(section)
section_replacements <- c("Asthma", "Sickle Cell", "Cystic Fibrosis", "Hemophilia", "Myocarditis", "Kawasaki disease", "Leukemia")
replace_company <- function(company) {
  return(company_replacements[match(company, companies)])
}
replace_section <- function(section) {
  return(section_replacements[match(section, sections)])
}
full_section <- function(section) {
  return(sapply(section, replace_section))
}
full_company <- function(company) {
  return(sapply(company, replace_company))
}

rand_column <- function(possibilities) {
  random <- as.numeric(sample(1:length(possibilities), 1))
  return(possibilities[random])
}

rand_admit <- function(test) {
  return(rand_column(c(0, 1)))
}
rand_setting <- function(test) {
  return(rand_column(c("Inpatient", "ED", "Outpatient")))
}
rand_campus <- function(test) {
  return(rand_column(c("Scottish-Rite", "Egleston")))
}

rand_insurance <- function(test) {
  return(rand_column(c("Medicaid", "Self Pay", "Private")))
}

calcMean <- function(array) {
  if (length(array) != 0) {
    return(mean(array))
  } else {
    return(0)
  }
}

questions_df <- questions_df %>% mutate(Disease=full_section(section), "Zip Code"=full_company(company_name), Insurance=sapply(question, rand_insurance), "Length of Stay"=nchar(question)+nchar(answer), Readmissions=sapply(question, rand_admit), Setting=sapply(question, rand_setting), Campus=sapply(question, rand_campus)) %>% subset(select=c("Disease", "Zip Code", "Insurance", "Length of Stay", "Readmissions", "Setting", "Campus")) %>% na.omit()

diagnosticGraph <- function(determinant, metric, setting, campus) {
  usableData <- questions_df
  speciValues <- c(setting, campus)
  for (speci in 1:length(specifications)) {
    if (speciValues[speci] != "All") {
      usableData <- usableData %>% filter(!!as.symbol(specifications[speci])==speciValues[speci])
    }
  }
  determinantPossibilities <- usableData %>% distinct(!!as.symbol(determinant)) %>% pull(!!as.symbol(determinant)) %>% remove_attributes("names")
  graphTable <- data.frame(determinantPossibilities)
  names(graphTable)[1] <- 'determinant'
  metricValues <- c()
  for (row in 1:length(determinantPossibilities)) {
    metricValues[row] <- usableData %>% filter(!!as.symbol(determinant)==determinantPossibilities[row]) %>% pull(!!as.symbol(metric)) %>% calcMean()
  }
  graphTable['metric'] <- metricValues
  return(ggplot(data=graphTable, aes(x=determinant, y=metric)) + geom_bar(aes(x=determinant), stat='identity') + coord_flip() + ggtitle(toTitleCase(paste(metric, "by", determinant))) + labs(x=toTitleCase(determinant), y=toTitleCase(metric)))
}

createTables <- function(dimension, determinant, metric, setting, campus) {
  targetData <- questions_df
  speciValues <- c(setting, campus)
  for (speci in 1:length(specifications)) {
    if (speciValues[speci] != "All") {
      targetData <- targetData %>% filter(!!as.symbol(specifications[speci])==speciValues[speci])
    }
  }
  values1 <- targetData %>% distinct(!!as.symbol(dimension)) %>% pull(!!as.symbol(dimension)) %>% remove_attributes("names")
  outputTable <- data.frame(values1)
  names(outputTable)[1] <- dimension
  outputExpected <- outputTable
  outputDifference <- outputTable
  outputTotal <- outputTable
  values2 <- targetData %>% distinct(!!as.symbol(determinant)) %>% pull(!!as.symbol(determinant)) %>% remove_attributes("names")
  for (col in 1:length(values2)) {
    newCol <- c()
    totals <- c()
    newExpected <- c()
    newDifference <- c()
    for (row in 1:length(values1)) {
      metricVector <- targetData %>% filter(!!as.symbol(dimension)==values1[row], !!as.symbol(determinant)==values2[col]) %>% pull(!!as.symbol(metric))
      newCol[row] <- metricVector %>% calcMean()
      totals[row] <- metricVector %>% sum()
      expectedRate <- targetData %>% filter(!!as.symbol(dimension)==values1[row]) %>% pull(!!as.symbol(metric)) %>% calcMean()
      newExpected[row] <- expectedRate * length(metricVector)
      newDifference[row] <- totals[row] - newExpected[row]
    }
    outputTable[values2[col]] <- newCol
    outputExpected[values2[col]] <- newExpected
    outputTotal[values2[col]] <- totals
    outputDifference[values2[col]] <- newDifference
  }
  return(list(outputTable, outputTotal, outputExpected, outputDifference))
}

groupedBar <- function(dimension, determinant, metric, setting, campus) {
  rawData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[4], check.names = FALSE)
  graphData <- data.frame(graphDim=character(), graphDet=character(), graphMet=numeric())
  names(graphData)[1] <- dimension
  names(graphData)[2] <- determinant
  names(graphData)[3] <- metric
  dims <- rawData %>% pull(!!as.symbol(dimension))
  dets <- rawData %>% colnames()
  counter = 1
  for (row in 1:nrow(rawData)) {
    for (col in 2:ncol(rawData)) {
      graphData[counter,] <- list(dims[row], dets[col], rawData[row,col])
      counter = counter+1
    }
  }
  title <- toTitleCase(paste("Actual - Expected",metric,"by",determinant))
  return(ggplot(graphData, aes(fill=!!as.symbol(determinant), y=!!as.symbol(metric), x=!!as.symbol(dimension))) + 
           geom_bar(position="dodge", stat="identity") + coord_flip() + ggtitle(title))
}

isolatedBar <- function(dimension, determinant, metric, setting, campus, target) {
  rawData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[4], check.names = FALSE)
  title <- toTitleCase(paste("Actual - Predicted",metric,"for patients with",determinant,"of",target))
  return(ggplot(data=rawData, aes(x=!!as.symbol(dimension), y=!!as.symbol(target))) + geom_bar(stat="identity") + coord_flip() + ggtitle(title) + labs(x=dimension, y=metric))
}

ids <- c("ActualAvg", "ActualTot", "Expected", "Difference", "Graph", "GroupedGraph")

shinyServer(function(input, output, session) {
    for (speci in 1:length(specifications)) {
      specVals <- questions_df %>% distinct(!!as.symbol(specifications[speci])) %>% pull(!!as.symbol(specifications[speci])) %>% remove_attributes("names")
      updateSelectInput(session, specifications[speci], choices = c("All", specVals), selected="All")
    }
    initialPossibilities <- questions_df %>% distinct(Insurance) %>% pull(Insurance) %>% remove_attributes("names")
    updateSelectInput(session, "Target", choices =c("Show All", initialPossibilities), selected="Show All")
    observeEvent(input$Determinant, {
      targetPossibilities <- eventReactive(input$Determinant, {questions_df %>% distinct(!!as.symbol(input$Determinant)) %>% pull(!!as.symbol(input$Determinant)) %>% remove_attributes("names")})
      updateSelectInput(session, "Target", choices =c("Show All", targetPossibilities()), selected=input$Target)
    })
    output$DiagnosticGraph <- renderPlot({diagnosticGraph(input$Determinant, input$Outcome, input$Setting, input$Campus)})
    
    observe({
      if (input$Target=="Show All") {
        hide("Dimension")
        hide("Question")
        hide("DimLabel")
        hide("heading")
        updateSelectInput(session, "Dimension", choices = c("", "Zip Code", "Disease"), selected="")
      } else {
        show("Dimension")
        show("heading")
        show("DimLabel")
        output$Question <- renderText({paste("contributes most to the disparity in ",input$Outcome," for patients with ",input$Determinant," of ",input$Target,"?",sep="")})
        show("Question")
      }
      if (input$Dimension=="") {
        show("DiagnosticGraph")
        hide("Display")
        for (id in 1:length(ids)) {
          hide(ids[id])
        }
      } else {
        show("Display")
        hide("DiagnosticGraph")
        outputs <- createTables(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus)
        output$ActualAvg <- renderDataTable({data.frame(outputs[1], check.names=FALSE)})
        output$ActualTot <- renderDataTable({data.frame(outputs[2], check.names=FALSE)})
        output$Expected <- renderDataTable({data.frame(outputs[3], check.names=FALSE)})
        output$Difference <- renderDataTable({data.frame(outputs[4], check.names=FALSE)})
        output$GroupedGraph <- renderPlot({groupedBar(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus)})
        output$Graph <- renderPlot({isolatedBar(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus, input$Target)})
        for (id in 1:length(ids)) {
          if (input$Display==ids[id]) {
            show(ids[id])
          } else {
            hide(ids[id])
          }
        }
      }
    })
})
