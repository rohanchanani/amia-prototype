library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tools)
library(Dict)
library(labelled)
library(shiny)
library(shinythemes)
library(shinyjs)


questions_df <- read_csv("https://raw.githubusercontent.com/rohanchanani/ApprenticeshipKMS/main/questions.csv")

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
rand_admit <- function(test) {
  random <- as.numeric(sample(0:1, 1))
  if (random == 0) {
    return(0)
  } else {
    return(1)
  }
}

oneClassify <- function(length) {
  if(length < 250) {
    return("Medicaid")
  }
  if(length<400) {
    return("Private")
  }
  return("Self-Pay")
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

questions_df <- questions_df %>% mutate(Disease=full_section(section), "Zip Code"=full_company(company_name), Insurance=classify(nchar(question)+nchar(answer)), "Length of Stay"=nchar(question)+nchar(answer), Readmissions=lapply(question, rand_admit)) %>% subset(select=-c(section, company_id, company_name, version, question_id, number))

createTable <- function(dimension, determinant, metric, isNumeric) {
  values1 <- questions_df %>% distinct(!!as.symbol(dimension)) %>% pull(!!as.symbol(dimension)) %>% remove_attributes("names")
  outputTable <- data.frame(values1)
  names(outputTable)[1] <- dimension
  values2 <- questions_df %>% distinct(!!as.symbol(determinant)) %>% pull(!!as.symbol(determinant)) %>% remove_attributes("names")
  for (col in 1:length(values2)) {
    newCol <- c()
    for (row in 1:length(values1)) {
      if (isNumeric) {
        currentVal <- questions_df %>% filter(!!as.symbol(dimension)==values1[row], !!as.symbol(determinant)==values2[col]) %>% pull(!!as.symbol(metric)) %>% calcMean()
      } else {
        currentVal <- questions_df %>% filter(!!as.symbol(dimension)==values1[row], !!as.symbol(determinant)==values2[col], !!as.symbol(metric)==1) %>% nrow()
      }
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

makeBarGraph <- function(outputTable, determinant, metric) {
  unit <- paste("(", metric, ")", sep="")
  title <- paste(toTitleCase(determinant), "Discrepancies by", toTitleCase(colnames(outputTable)[1]))
  differenceTable <- findDifference(outputTable)
  testStrata <- c()
  totalDiscrepancy <- c()
  for (row in 1:nrow(differenceTable)) {
    testStrata[row] <- differenceTable[row,1]
    totalDiscrepancy[row] <- sum(abs(differenceTable[row,2:ncol(differenceTable)]))
  }
  finalData <- data.frame(testStrata, totalDiscrepancy)
  return(ggplot(data=finalData, aes(x=testStrata, y=totalDiscrepancy)) + geom_bar(aes(x=testStrata), stat='identity') + coord_flip() + ggtitle(title) + labs(x=toTitleCase(toTitleCase(colnames(outputTable)[1])), y=paste(toTitleCase(determinant), "Discrepancy", unit)))
}

ids <- c("value", "expected", "difference", "graph")

shinyServer(function(input, output, session) {
    output$value <- renderDataTable({createTable(input$dimension, input$determinant, input$outcome, input$outcome=="Length of Stay")})
    output$expected <- renderDataTable({createExpected(createTable(input$dimension, input$determinant, input$outcome, input$outcome=="Length of Stay"))})
    output$difference <- renderDataTable({findDifference(createTable(input$dimension, input$determinant, input$outcome, input$outcome=="Length of Stay"))})
    output$graph <- renderPlot({makeBarGraph(createTable(input$dimension, input$determinant, input$outcome, input$outcome=="Length of Stay"), input$determinant, input$outcome)})
    
    
    observe({
      for (test in 1:length(ids)) {
        if (input$display == ids[test]) {
          show(ids[test])
        } else {
          hide(ids[test])
        }
      }
    })
})