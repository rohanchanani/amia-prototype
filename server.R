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
average_units = c("Length of Stay"="Length of Stay (Hours)", "Readmissions"="Readmission Rate")
absolute_units = c("Length of Stay"="Hours", "Readmissions"="Readmissions")
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
  nudge = metricValues %>% sapply(abs) %>% mean() / 15
  return(ggplot(data=graphTable, aes(x=determinant, y=metric)) + geom_bar(aes(x=determinant), stat='identity') + geom_text(aes(y=metric+nudge*sign(metric), label = signif(metric, digits=2))) + coord_flip() + ggtitle(toTitleCase(paste(metric, "by", determinant))) + labs(x=toTitleCase(determinant), y=toTitleCase(paste("Average",average_units[metric]))))
}

createTables <- function(dimension, determinant, metric, setting, campus) {
  targetData <- questions_df %>% filter(Setting==setting, Campus==campus)
  values1 <- questions_df %>% distinct(!!as.symbol(dimension)) %>% pull(!!as.symbol(dimension)) %>% remove_attributes("names")
  outputTable <- data.frame(values1)
  names(outputTable)[1] <- dimension
  outputExpected <- outputTable
  outputDifference <- outputTable
  outputTotal <- outputTable
  outputRelative <- outputTable
  values2 <- questions_df %>% distinct(!!as.symbol(determinant)) %>% pull(!!as.symbol(determinant)) %>% remove_attributes("names")
  for (col in 1:length(values2)) {
    newCol <- c()
    totals <- c()
    newExpected <- c()
    newDifference <- c()
    newRelative <- c()
    for (row in 1:length(values1)) {
      metricVector <- questions_df %>% filter(!!as.symbol(dimension)==values1[row], !!as.symbol(determinant)==values2[col]) %>% pull(!!as.symbol(metric))
      newCol[row] <- metricVector %>% calcMean()
      totals[row] <- metricVector %>% sum()
      expectedRate <- questions_df %>% filter(!!as.symbol(dimension)==values1[row]) %>% pull(!!as.symbol(metric)) %>% calcMean()
      newExpected[row] <- expectedRate * length(metricVector)
      newDifference[row] <- totals[row] - newExpected[row]
      if (expectedRate == 0) {
        newRelative[row] <- 0
      } else {
        newRelative[row] <- newCol[row] / expectedRate
      }
    }
    outputTable[values2[col]] <- newCol
    outputExpected[values2[col]] <- newExpected
    outputTotal[values2[col]] <- totals
    outputDifference[values2[col]] <- newDifference
    outputRelative[values2[col]] <- newRelative
  }
  return(list(outputTable, outputTotal, outputExpected, outputDifference, outputRelative))
}

createSpecific <- function(actual, expected, difference, target) {
  initialValues <- actual %>% pull(!!as.symbol(names(actual)[1])) %>% remove_attributes("names")
  output <- data.frame(initialValues)
  names(output)[1] = names(actual)[1]
  output["Actual"] = pull(actual, !!as.symbol(target))
  output["Equitable"] = pull(expected, !!as.symbol(target))
  output["Difference"] = pull(difference, !!as.symbol(target))
  return(output[order(output$Difference),])
}

groupedBar <- function(dimension, determinant, metric, setting, campus) {
  rawData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[4], check.names = FALSE)
  graphData <- data.frame(graphDim=character(), graphDet=character(), graphMet=numeric())
  names(graphData)[1] <- dimension
  names(graphData)[2] <- determinant
  names(graphData)[3] <- metric
  dims <- rawData %>% pull(!!as.symbol(dimension))
  dets <- rawData %>% colnames()
  dimsList <- c()
  detsList <- c()
  for (dim in 1:length(dims)) {
    dimsList[dims[dim]] <- dim
  }
  for (det in 1:length(dets)) {
    detsList[dets[det]] <- det
  }
  counter = 1
  allVals <- c()
  for (row in 1:nrow(rawData)) {
    for (col in 2:ncol(rawData)) {
      graphData[counter,] <- list(dims[row], dets[col], rawData[row,col])
      allVals[counter] <- rawData[row,col]
      counter = counter+1
    }
  }
  title <- toTitleCase(paste("Discrepancy in Total",absolute_units[metric],"by",determinant,"and",dimension))
  yLabel1 <- toTitleCase(paste("Actual - Equitable*","Total",absolute_units[metric],"(Total cumulative",absolute_units[metric],"across all visits in each",paste(dimension,")",sep="")))
  yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the difference between the actual cumulative",metric, "and the equitable cumulative",metric,"for all visits in each",paste(dimension,".",sep=""),"In a perfectly equitable",paste(dimension,",",sep=""),"each of the bars would be at 0.")
  textX = 1.5 + rawData %>% pull(!!as.symbol(dimension)) %>% length()
  textY1 = min(allVals) + (max(allVals) - min(allVals)) * 0.2
  textY2 = min(allVals) + (max(allVals) - min(allVals)) * 0.8
  arrowOffset = (max(allVals) - min(allVals)) / 16
  arrowLength = (max(allVals) - min(allVals)) * 3 / 8
  minText = paste("Fewer total", absolute_units[metric], "relative to whole population within the",dimension)
  maxText = paste("More total", absolute_units[metric], "relative to whole population within the",dimension)
  meanY=(max(allVals) + min(allVals)) / 2
  arrowX = textX - 0.5
  nudge = allVals %>% sapply(abs) %>% mean() / 8
  mini_nudge = nudge / 2
  return(ggplot(graphData, aes(fill=!!as.symbol(determinant), y=!!as.symbol(metric), x=!!as.symbol(dimension)))
         + geom_bar(position=position_dodge(0.75), width=0.75, stat="identity")
         + annotate("text", x=textX+0.5, y=textY1, label="")
         + annotate("text", x=textX, y=textY1, label=str_wrap(minText, width=60))
         + annotate("text", x=textX, y=textY2, label=str_wrap(maxText, width=60))
         + geom_segment(aes(x=arrowX,y=meanY+arrowOffset,xend=arrowX,yend=meanY+arrowOffset+arrowLength),arrow=arrow())
         + geom_segment(aes(x=arrowX,y=meanY-arrowOffset,xend=arrowX,yend=meanY-arrowOffset-arrowLength),arrow=arrow())
         + coord_flip(clip="off")
         + geom_text(position=position_dodge(width=0.75),aes(y=!!as.symbol(metric)+sign(!!as.symbol(metric))*nudge - mini_nudge,fill=!!as.symbol(determinant),label=signif(!!as.symbol(metric), digits=2),hjust=0))
         + ggtitle(title)
         + labs(x=dimension, y=paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

groupedActual <- function(dimension, determinant, metric, setting, campus) {
  rawData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[1], check.names = FALSE)
  graphData <- data.frame(graphDim=character(), graphDet=character(), graphMet=numeric())
  names(graphData)[1] <- dimension
  names(graphData)[2] <- determinant
  names(graphData)[3] <- metric
  dims <- rawData %>% pull(!!as.symbol(dimension))
  dets <- rawData %>% colnames()
  dimsList <- c()
  detsList <- c()
  for (dim in 1:length(dims)) {
    dimsList[dims[dim]] <- dim
  }
  for (det in 1:length(dets)) {
    detsList[dets[det]] <- det
  }
  counter = 1
  allVals <- c()
  for (row in 1:nrow(rawData)) {
    for (col in 2:ncol(rawData)) {
      graphData[counter,] <- list(dims[row], dets[col], rawData[row,col])
      allVals[counter] <- rawData[row,col]
      counter = counter+1
    }
  }
  title <- toTitleCase(paste("Average",average_units[metric],"by",dimension,"and",determinant))
  nudge = allVals %>% sapply(abs) %>% mean() / 8
  mini_nudge = nudge / 2
  return(ggplot(graphData, aes(fill=!!as.symbol(determinant), y=!!as.symbol(metric), x=!!as.symbol(dimension)))
         + geom_bar(position=position_dodge(0.75), width=0.75, stat="identity")
         + coord_flip(clip="off")
         + geom_text(position=position_dodge(width=0.75),aes(y=!!as.symbol(metric)+sign(!!as.symbol(metric))*nudge - mini_nudge,fill=!!as.symbol(determinant),label=signif(!!as.symbol(metric), digits=2),hjust=0))
         + ggtitle(title)
         + labs(x=dimension, y=paste("Average",absolute_units[metric])))
}

isolatedBar <- function(dimension, determinant, metric, setting, campus, target, index=4) {
  initialData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[index], check.names = FALSE)
  initialValues <- initialData %>% pull(!!as.symbol(names(initialData)[1])) %>% remove_attributes("names")
  rawData <- data.frame(initialValues)
  names(rawData)[1] <-  names(initialData)[1]
  rawData["Difference"] <- initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  title <- toTitleCase(paste("Discrepancy in total",absolute_units[metric],"for patients with",determinant,"of",target,"by",dimension))
  yLabel1 <- toTitleCase(paste("Actual - Equitable* total",absolute_units[metric],"Total cumulative",absolute_units[metric],"across all",target,"visits in each",paste(dimension,")",sep="")))
  yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the difference between the actual cumulative",metric, "for",target,"visits and the equitable cumulative",metric, "for each",paste(dimension,".",sep=""),"In a perfectly equitable",paste(dimension,",",sep=""),"the bar would be at 0.")
  textX = 1.5 + rawData %>% pull(!!as.symbol(dimension)) %>% length()
  targetValues = rawData %>% pull(Difference)
  textY1 = min(targetValues) + (max(targetValues) - min(targetValues)) * 0.2
  textY2 = min(targetValues) + (max(targetValues) - min(targetValues)) * 0.8
  arrowOffset = (max(targetValues) - min(targetValues)) / 16
  arrowLength = (max(targetValues) - min(targetValues)) * 3 / 8
  minText = paste("Fewer total", absolute_units[metric], "in", target, "visits relative to whole population within the",dimension)
  maxText = paste("More total", absolute_units[metric], "in", target, "visits relative to whole population within the",dimension)
  meanY=(max(targetValues) + min(targetValues)) / 2
  arrowX = textX - 0.5
  nudge = targetValues %>% sapply(abs) %>% mean() / 15
  return(ggplot(data=rawData, aes(x=reorder(!!as.symbol(dimension), Difference), y=Difference))
         + geom_bar(stat="identity") + annotate("text", x=textX+0.25, y=0, label="") 
         + annotate("text", x=textX+0.5, y=textY1, label="")
         + annotate("text", x=textX, y=textY1, label=str_wrap(minText, width=60))
         + annotate("text", x=textX, y=textY2, label=str_wrap(maxText, width=60))
         + geom_segment(aes(x=arrowX,y=meanY+arrowOffset,xend=arrowX,yend=meanY+arrowOffset+arrowLength),arrow=arrow())
         + geom_segment(aes(x=arrowX,y=meanY-arrowOffset,xend=arrowX,yend=meanY-arrowOffset-arrowLength),arrow=arrow())
         + coord_flip(clip="off")
         + geom_text(aes(y=Difference + nudge*sign(Difference),label = signif(Difference, digits=2)))
         + ggtitle(title)
         + labs(x=dimension, y=paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

isolatedRelative <- function(dimension, determinant, metric, setting, campus, target, index=5) {
  initialData <- data.frame(createTables(dimension, determinant, metric, setting, campus)[index], check.names = FALSE)
  initialValues <- initialData %>% pull(!!as.symbol(names(initialData)[1])) %>% remove_attributes("names")
  rawData <- data.frame(initialValues)
  names(rawData)[1] <-  names(initialData)[1]
  rawData["Ratio"] <- initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  title <- toTitleCase(paste("Discrepancy in",metric,"for patients with",determinant,"of",target,"by",dimension))
  yLabel1 <- toTitleCase(paste("Actual / Equitable*",metric,"across all",target,"visits in each",dimension))
  yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the ratio between the actual average",average_units[metric], "for",target,"visits and the equitable average",average_units[metric], "for each",paste(dimension,".",sep=""),"In a perfectly equitable",paste(dimension,",",sep=""),"the bar would be at 1.")
  targetValues = rawData %>% pull(Ratio)
  nudge = targetValues %>% sapply(abs) %>% mean() / 15
  return(ggplot(data=rawData, aes(x=reorder(!!as.symbol(dimension), Ratio), y=Ratio))
         + geom_bar(stat="identity")
         + coord_flip(clip="off")
         + geom_text(aes(y=Ratio + nudge*sign(Ratio),label = signif(Ratio, digits=2)))
         + ggtitle(title)
         + labs(x=dimension, y=paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

createHighlight <- function(outputs, index, xVal, dimension, determinant, target,metric) {
  actualTable <- data.frame(outputs[index], check.names=FALSE)
  actualDifference <- data.frame(outputs[6-index], check.names=FALSE)
  firstVal <- c(paste(actualTable[xVal,1], target,sep="/"))
  results <- data.frame(firstVal)
  names(results)[1] <- paste(dimension,determinant,sep="/")
  if (index==1) {
    metric <- paste("Average",average_units[metric])
  } else {
    metric <- paste("Total",metric,paste("(",absolute_units[metric],")",sep=""))
  }
  results["Actual"] <- c(actualTable[xVal,target])
  names(results)[2] <- paste("Actual",metric)
  if (index==1) {
    results["Expected"] <- c(actualTable[xVal,target] / actualDifference[xVal,target])
    results["Difference"] <- c(actualDifference[xVal,target])
    separator <- " / "
  } else {
    results["Expected"] <- c(actualTable[xVal,target] - actualDifference[xVal,target])
    results["Difference"] <- c(actualDifference[xVal,target])
    separator <- " - "
  }
  names(results)[3] <- paste("Expected",metric)
  names(results)[4] <- paste(paste("Actual","Equitable",sep=separator),metric)
  return(results)
}

ids <- c("ActualAvg", "ActualTot", "Expected", "Difference", "Graph", "GroupedGraph","SpecificTable","GroupedActualGraph","RelativeGraph", "RelativeTable")
idValues = list(Graph=list("Graph", "SpecificTable"), GroupedGraph=list("GroupedGraph","ActualTot","Expected","Difference"), ActualAvg=list("GroupedActualGraph", "ActualAvg"),PartialDiff=list("RelativeGraph", "RelativeTable"))
displayDf = data.frame(ids)
possibleIds = c("Graph", "GroupedGraph", "ActualAvg", "PartialDiff")
graph_col <- c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0)
displayDf["Graph"] <- graph_col
grouped_col <- c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0)
displayDf["GroupedGraph"] <- grouped_col
avg_col <- c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0)
displayDf["ActualAvg"] <- avg_col
partial_col <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
displayDf["PartialDiff"] <- partial_col 
#print(displayDf)
macro=TRUE
shinyServer(function(input, output, session) {
    hide("Target")
    hide("Dimension")
    hide("Question")
    hide("DimLabel")
    hide("heading")
    hide("macroView")
    for (speci in 1:length(specifications)) {
      specVals <- questions_df %>% distinct(!!as.symbol(specifications[speci])) %>% pull(!!as.symbol(specifications[speci])) %>% remove_attributes("names")
      updateSelectInput(session, specifications[speci], choices = c("All", specVals), selected="All")
    }
    initialPossibilities <- questions_df %>% distinct(Insurance) %>% pull(Insurance) %>% remove_attributes("names")
    updateSelectInput(session, "Target", choices =initialPossibilities, selected=initialPossibilities[0])
    observeEvent(input$Determinant, {
      targetPossibilities <- eventReactive(input$Determinant, {questions_df %>% distinct(!!as.symbol(input$Determinant)) %>% pull(!!as.symbol(input$Determinant)) %>% remove_attributes("names")})
      updateSelectInput(session, "Target", choices =targetPossibilities(), selected=targetPossibilities()[0])
    })
    output$DiagnosticGraph <- renderPlot({diagnosticGraph(input$Determinant, input$Outcome, input$Setting, input$Campus)})
    observeEvent(input$diveDeeper, {
      show("Target")
      if (input$Target!=""){
        show("Dimension")
        show("Question")
        show("DimLabel")
        show("heading")
      }
      hide("diveDeeper")
      show("macroView")
      macro=FALSE
      updateSelectInput(session, "Target", selected="")
      updateSelectInput(session, "Dimension", choices = c("", "Zip Code", "Disease"), selected="")
    })
    observeEvent(input$macroView, {
      updateSelectInput(session, "Target", selected="")
      hide("Target")
      hide("Dimension")
      hide("Question")
      hide("DimLabel")
      hide("heading")
      show("diveDeeper")
      hide("macroView")
      macro=TRUE
      updateSelectInput(session, "Target", selected="")
      updateSelectInput(session, "Dimension", choices = c("", "Zip Code", "Disease"), selected="")
    })
    observeEvent(input$Target, {
      if (input$Target!="") {
        show("Dimension")
        show("Question")
        show("DimLabel")
        show("heading")
      }
    })
    
    toListen <- reactive({
      list(input$Dimension,input$Determinant,input$Target,input$Outcome)
    })
    
    observeEvent(toListen(), {
      if (input$Display == "") {
        default <- "Graph"
      } else {
        default <- input$Display
      }
      averageActual <- paste("I want to look for disparities in",average_units[input$Outcome],"within each",input$Dimension,"across",paste(input$Determinant,".",sep=""))
      specificDifference <- paste("Which",input$Dimension,"contributes most to the disparity in total",input$Outcome,"for patients with",input$Determinant,"of",paste(input$Target,"?",sep=""))
      relativeDifference <- paste("Which",input$Dimension,"has the highest relative disparity in",input$Outcome,"for patients with",input$Determinant,"of",paste(input$Target,"?",sep=""))
      fullDifference <- paste("Which",input$Dimension,"contributes most to the disparity in",input$Outcome,"across",paste(input$Determinant,"?",sep=""))
      newChoices = list()
      newChoices[[ averageActual ]] <- "ActualAvg"
      newChoices[[ specificDifference ]] <- "Graph"
      newChoices[[ relativeDifference ]] <- "PartialDiff"
      newChoices[[ fullDifference ]] <- "GroupedGraph"
      updateSelectInput(session,"Display",choices=newChoices, selected=default)
    })
    
    observe({
      if (input$Dimension=="" | input$Target=="") {
        hide("specificHighlight")
        hide("relativeHighlight")
        if (input$Target=="") {
          hide("Dimension")
        } 
        show("DiagnosticGraph")
        hide("Display")
        for (id in 1:length(ids)) {
          hide(ids[id])
        }
      } else {
        show("Dimension")
        show("Question")
        show("DimLabel")
        show("heading")
        show("Display")
        hide("DiagnosticGraph")
        outputs <- createTables(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus)
        output$ActualAvg <- renderDataTable({data.frame(outputs[1], check.names=FALSE)})
        total <- data.frame(outputs[2], check.names=FALSE)
        expected <- data.frame(outputs[3], check.names=FALSE)
        difference <- data.frame(outputs[4], check.names=FALSE)
        output$ActualTot <- renderDataTable({total})
        output$Expected <- renderDataTable({expected})
        output$Difference <- renderDataTable({difference})
        output$RelativeTable <- renderDataTable({data.frame(outputs[5], check.names=FALSE)})
        output$SpecificTable <- renderDataTable({createSpecific(total, expected, difference, input$Target)})
        output$GroupedGraph <- renderPlot({groupedBar(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus)})
        output$GroupedActualGraph <- renderPlot({groupedActual(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus)})
        output$Graph <- renderPlot({isolatedBar(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus, input$Target)})
        output$RelativeGraph <- renderPlot({isolatedRelative(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus, input$Target)})
        if (!is.null(input$specificClick)) {
          specx <- input$specificClick$x
          specy <- input$specificClick$y
          if (input$Display=="Graph" & abs(specy - round(specy)) < 0.4 & specy > 0 & round(specy) <= nrow(difference)){
            actualIdx <- match(arrange(difference,!!as.symbol(input$Target))[round(specy),input$Target], pull(difference,!!as.symbol(input$Target)))
            actualVal = difference[actualIdx,input$Target]
            if (specx > min(actualVal, 0) & specx < max(0,actualVal)) {
              output$specificHighlight <- renderDataTable({createHighlight(outputs, 2, actualIdx, input$Dimension, input$Determinant,input$Target,input$Outcome)})
              show("specificHighlight")
            }
            else {
              hide("specificHighlight")
            }
          } else {
            hide("specificHighlight")
          }
        }
        if (!is.null(input$relativeClick)) {
          relx <- input$relativeClick$x
          rely <- input$relativeClick$y
          if (input$Display=="PartialDiff" & abs(rely - round(rely)) < 0.2 & rely > 0 & round(rely) <= nrow(difference)){
            ratio <- data.frame(outputs[5], check.names=FALSE)
            actualIdx <- match(arrange(ratio,!!as.symbol(input$Target))[round(rely),input$Target], pull(ratio,!!as.symbol(input$Target)))
            actualVal = ratio[actualIdx,input$Target]
            if (relx > min(actualVal, 0) & relx < max(0,actualVal)) {
              output$relativeHighlight <- renderDataTable({createHighlight(outputs, 1, actualIdx, input$Dimension, input$Determinant,input$Target,input$Outcome)})
              show("relativeHighlight")
            }
            else {
              hide("relativeHighlight")
            }
          } else {
            hide("relativeHighlight")
          }
          
        }
        for (id in 1:length(ids)) {
          if (displayDf[id,match(input$Display, colnames(displayDf))]==1) {
            show(ids[id])
          } else {
            if (ids[id]=="Graph") {
              hide("specificHighlight")
            }
            if (ids[id]=="RelativeGraph") {
              hide("relativeHighlight")
            }
            hide(ids[id])
          }
        }
      }
    })
})
