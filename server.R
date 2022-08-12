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


questions_df <- read_csv("processed_patient.csv")
specifications <- c("Setting", "Campus")
average_units = c("Length of Stay"="Length of Stay (Hours)", "7 Day Unplanned Readmissions"="7 Day Unplanned Readmission Rate")
absolute_units = c("Length of Stay"="Hours", "7 Day Unplanned Readmissions"="Occurences")
directions = c("Length of Stay"=TRUE, "7 Day Unplanned Readmissions"=TRUE)
#companies <- questions_df %>% distinct(company_name) %>% pull(company_name)
company_replacements <- c("30305", "30306", "30307", "30308", "30309")
#sections <- questions_df %>% distinct(section) %>% pull(section)
section_replacements <- c("Asthma", "Sickle Cell", "Cystic Fibrosis", "Hemophilia", "Myocarditis", "Kawasaki disease", "Leukemia")
normalText <- "<span style='font-weight:normal;'>"


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

#questions_df <- questions_df %>% mutate(Disease=full_section(section), "Zip Code"=full_company(company_name), Insurance=sapply(question, rand_insurance), "Length of Stay"=nchar(question)+nchar(answer), Readmissions=sapply(question, rand_admit), Setting=sapply(question, rand_setting), Campus=sapply(question, rand_campus)) %>% subset(select=c("Disease", "Zip Code", "Insurance", "Length of Stay", "Readmissions", "Setting", "Campus")) %>% na.omit()

get_actual <- function(dimension, options, setting="All", campus="All") {
  if (setting != "All") {
    questions_df <- questions_df %>% filter(Setting==setting)
  }
  if (campus != "All") {
    questions_df <- questions_df %>% filter(Campus==campus)
  }
  if (!(options[1] %in% pull(questions_df, !!as.symbol(dimension)))) {
    dimPossibilities = questions_df %>% distinct(!!as.symbol(dimension)) %>% pull(!!as.symbol(dimension)) %>% remove_attributes("names")
    options <- c(dimPossibilities[1])
  }
  questions_df <- questions_df %>% filter(!!as.symbol(dimension) %in% options)
  return(questions_df)
}

diagnosticGraph <- function(determinant, metric, setting, campus, equitable) {
  usableData <- questions_df
  determinantPossibilities <- usableData %>% distinct(!!as.symbol(determinant)) %>% pull(!!as.symbol(determinant)) %>% remove_attributes("names")
  graphTable <- data.frame(determinantPossibilities)
  names(graphTable)[1] <- 'determinant'
  metricValues <- c()
  for (row in 1:length(determinantPossibilities)) {
    if (equitable) {
      metricValues[row] <- usableData %>% pull(!!as.symbol(metric)) %>% calcMean()
    } else {
      metricValues[row] <- usableData %>% filter(!!as.symbol(determinant)==determinantPossibilities[row]) %>% pull(!!as.symbol(metric)) %>% calcMean()
    }
    num <- usableData %>% filter(!!as.symbol(determinant)==graphTable[row,1]) %>% nrow()
    graphTable[row,1] <- paste(graphTable[row,1], paste("n=",num,sep=""),sep="\n")
  }
  graphTable['metric'] <- metricValues
  nudge = metricValues %>% sapply(abs) %>% mean() / 15
  return(ggplot(data=graphTable, aes(x=determinant, y=metric)) + geom_bar(aes(x=determinant), stat='identity') + geom_text(aes(y=metric+nudge*sign(metric), label = signif(metric, digits=2))) + coord_flip() + ggtitle(toTitleCase(paste(metric, "by", determinant))) + labs(x=toTitleCase(determinant), y=toTitleCase(paste("Average",average_units[metric]))))
}

getOptions <- function(dimension, determinant, metric, setting, campus, target) {
  allDims <- questions_df %>% distinct(!!as.symbol(dimension)) %>% pull(!!as.symbol(dimension))
  differences <- c()
  for (dim in 1:length(allDims)) {
    dimVec <- questions_df %>% filter(!!as.symbol(dimension)==allDims[dim], !!as.symbol(determinant)==target) %>% pull(!!as.symbol(metric))
    actual <- dimVec %>% sum()
    equitable <- questions_df %>% filter(!!as.symbol(dimension)==allDims[dim]) %>% pull(!!as.symbol(metric)) %>% calcMean()
    differences <- append(differences, actual-(equitable*length(dimVec)))
  }
  tempFrame <- data.frame(dim=allDims, diff=differences)
  return(arrange(tempFrame, desc(differences))[1:6,1])
}

createTables <- function(dimension, determinant, metric, options, setting, campus, target) {
  questions_df <- get_actual(dimension, options, setting, campus)
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

groupedBar <- function(dimension, determinant, metric, options, setting, campus, target, equitable=FALSE) {
  questions_df <- get_actual(dimension, options, setting, campus)
  rawData <- data.frame(createTables(dimension, determinant, metric, options, setting, campus, target)[4], check.names = FALSE)
  graphData <- data.frame(graphDim=character(), graphDet=character(), graphMet=numeric())
  names(graphData)[1] <- dimension
  names(graphData)[2] <- determinant
  names(graphData)[3] <- metric
  dims <- rawData %>% pull(!!as.symbol(dimension))
  dets <- rawData %>% colnames()
  for (dim in 1:length(dims)) {
    count <- questions_df %>% filter(!!as.symbol(dimension)==dims[dim]) %>% nrow()
    dims[dim] = paste(dims[dim],paste("n=",count,sep=""),sep="\n")
  }
  counter = 1
  allVals <- c()
  for (row in 1:nrow(rawData)) {
    for (col in 2:ncol(rawData)) {
      if (equitable) {
        graphData[counter,] <- list(dims[row], dets[col], 0)
      } else {
        graphData[counter,] <- list(dims[row], dets[col], rawData[row,col])
      }
      allVals[counter] <- rawData[row,col]
      counter = counter+1
    }
  }
  title <- paste("Which",dimension,"contributes most to the disparity in",metric,"across",paste(determinant,"?",sep=""))
  yLabel1 <- toTitleCase(paste("Actual - Equitable*","Total",metric,paste("(",absolute_units[metric],")",sep=""),"across all visits in each",dimension))
  yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the difference between the actual cumulative",metric, "and the equitable cumulative",metric,"for all visits in each",paste(dimension,".",sep=""),"If the average",average_units[metric],"was equal across", determinant,"for patients with a specific",paste(dimension,",",sep=""), "the bar would be at 0.")
  textX = 1.5 + rawData %>% pull(!!as.symbol(dimension)) %>% length()
  textY1 = min(allVals) + (max(allVals) - min(allVals)) * 0.2
  textY2 = min(allVals) + (max(allVals) - min(allVals)) * 0.8
  arrowOffset = (max(allVals) - min(allVals)) / 16
  arrowLength = (max(allVals) - min(allVals)) * 3 / 8
  minText = paste("Reduced", metric)
  maxText = paste("Excess", metric)
  if (directions[metric]) {
    maxText <- paste(maxText, "(disadvantaged)")
  } else {
    minText <- paste(minText, "(disadvantaged)")
  }
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
         + coord_cartesian(xlim = c(-0.5, length(dims)+0.5), ylim = c(min(allVals)-0.5, max(allVals)+0.5))
         + coord_flip(clip="off")
         #+ geom_text(position=position_dodge(width=0.75),aes(y=!!as.symbol(metric)+sign(!!as.symbol(metric))*nudge - mini_nudge,fill=!!as.symbol(determinant),label=signif(!!as.symbol(metric), digits=2),hjust=0))
         + ggtitle(title)
         + labs(x=dimension, y=paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

groupedActual <- function(dimension, determinant, metric, options, setting, campus, target, equitable=FALSE) {
  questions_df <- get_actual(dimension, options, setting, campus)
  rawData <- data.frame(createTables(dimension, determinant, metric, options, setting, campus, target)[1], check.names = FALSE)
  graphData <- data.frame(graphDim=character(), graphDet=character(), graphMet=numeric())
  names(graphData)[1] <- dimension
  names(graphData)[2] <- determinant
  names(graphData)[3] <- metric
  dims <- rawData %>% pull(!!as.symbol(dimension))
  dets <- rawData %>% colnames()
  dimsList <- c()
  detsList <- c()
  for (dim in 1:length(dims)) {
    count <- questions_df %>% filter(!!as.symbol(dimension)==dims[dim]) %>% nrow()
    dims[dim] = paste(dims[dim],paste("n=",count,sep=""),sep="\n")
  }
  counter = 1
  allVals <- c()
  for (row in 1:nrow(rawData)) {
    if (equitable) {
      avgVal <- questions_df %>% filter(!!as.symbol(dimension)==rawData[row,1]) %>% pull(!!as.symbol(metric)) %>% calcMean()
    }
    for (col in 2:ncol(rawData)) {
      if (equitable) {
        graphData[counter,] <- list(dims[row], dets[col], avgVal)
      } else {
        graphData[counter,] <- list(dims[row], dets[col], rawData[row,col])
      }
      allVals[counter] <- rawData[row,col]
      counter = counter+1
    }
  }
  title <- paste("I want to look for disparities in",average_units[metric],"within each",dimension,"across",paste(determinant,".",sep=""))
  nudge = allVals %>% sapply(abs) %>% mean() / 8
  mini_nudge = nudge / 2
  return(ggplot(graphData, aes(fill=!!as.symbol(determinant), y=!!as.symbol(metric), x=!!as.symbol(dimension)))
         + geom_bar(position=position_dodge(0.75), width=0.75, stat="identity")
         + coord_cartesian(xlim = c(-0.5, length(dims)+0.5), ylim = c(min(allVals)-0.5, max(allVals)+0.5))
         + coord_flip(clip="off")
         #+ geom_text(position=position_dodge(width=0.75),aes(y=!!as.symbol(metric)+sign(!!as.symbol(metric))*nudge - mini_nudge,fill=!!as.symbol(determinant),label=signif(!!as.symbol(metric), digits=2),hjust=0))
         + ggtitle(title)
         + labs(x=dimension, y=paste("Average",average_units[metric])))
}

positive <- function(number) {
  if (number > 0) {
    return("")
  }
}

isolatedBar <- function(dimension, determinant, metric, options, setting, campus, target, equitable=FALSE, index=4) {
  questions_df <- get_actual(dimension, options, setting, campus)
  initialData <- data.frame(createTables(dimension, determinant, metric, options, setting, campus, target)[index], check.names = FALSE)
  initialValues <- initialData %>% pull(!!as.symbol(names(initialData)[1])) %>% remove_attributes("names")
  rawData <- data.frame(initialValues)
  names(rawData)[1] <-  names(initialData)[1]
  for (dim in 1:nrow(rawData)) {
    count <- questions_df %>% filter(!!as.symbol(dimension)==rawData[dim,1], !!as.symbol(determinant)==target) %>% nrow()
    rawData[dim,1] = paste(rawData[dim,1],paste("n=",count,sep=""),sep="\n")
  }
  rawData["Difference"] <- initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  if (equitable) {
    for (row in 1:nrow(rawData)) {
      rawData[row,"Difference"] <- 0
    }
  }
  rawData <- rawData %>% mutate(color=sapply(Difference, positive))
  title <- paste("Which",dimension,"should I focus on to improve the disparity in",metric,"by",determinant,"for",target, "patients?")
  yLabel1 <- toTitleCase(paste("Actual - Equitable* total",metric,paste("(",absolute_units[metric],")",sep=""),"across all",target,"visits in each",dimension))
  #yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the difference between the actual cumulative",metric, "for",target,"visits and the equitable cumulative",metric, "for each",paste(dimension,".",sep=""),"If the average",average_units[metric],"was equal across", determinant,"for patients with a specific",paste(dimension,",",sep=""), "the bar would be at 0.")
  textX = 1.5 + rawData %>% pull(!!as.symbol(dimension)) %>% length()
  targetValues = initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  textY1 = min(targetValues) + (max(targetValues) - min(targetValues)) * 0.2
  textY2 = min(targetValues) + (max(targetValues) - min(targetValues)) * 0.8
  arrowOffset = (max(targetValues) - min(targetValues)) / 16
  arrowLength = (max(targetValues) - min(targetValues)) * 3 / 8
  minText = paste("Reduced", metric, "for", target, "patients")
  maxText = paste("Excess", metric, "for", target, "patients")
  if (directions[metric]) {
    maxText <- paste(maxText, "(disadvantaged)")
  } else {
    minText <- paste(minText, "(disadvantaged)")
  }
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
         + coord_cartesian(xlim = c(-0.5, nrow(rawData)+0.5), ylim = c(min(targetValues)-0.5, max(targetValues)+0.5))
         + coord_flip(clip="off")
         + geom_text(aes(y=Difference + nudge*sign(Difference),label = signif(Difference, digits=2)))
         + ggtitle(title)
         + labs(x=dimension, y=yLabel1))#paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

isolatedRelative <- function(dimension, determinant, metric, options, setting, campus, target, equitable=FALSE,  index=5) {
  questions_df <- get_actual(dimension, options, setting, campus)
  initialData <- data.frame(createTables(dimension, determinant, metric, options, setting, campus, target)[index], check.names = FALSE)
  initialValues <- initialData %>% pull(!!as.symbol(names(initialData)[1])) %>% remove_attributes("names")
  rawData <- data.frame(initialValues)
  names(rawData)[1] <-  names(initialData)[1]
  for (dim in 1:nrow(rawData)) {
    count <- questions_df %>% filter(!!as.symbol(dimension)==rawData[dim,1], !!as.symbol(determinant)==target) %>% nrow()
    rawData[dim,1] = paste(rawData[dim,1],paste("n=",count,sep=""),sep="\n")
  }
  rawData["Ratio"] <- initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  if (equitable) {
    for (row in 1:nrow(rawData)) {
      rawData[row,"Ratio"] <- 1
    }
  }
  title <- paste("Which",dimension,"has the highest relative disparity in",metric,"for patients with",determinant,"of",paste(target,"?",sep=""))
  yLabel1 <- toTitleCase(paste("Relative",average_units[metric],"for patients with",determinant,"of",target,"compared with all other",determinant,"groups."))
  #yLabel2 <- paste("*In an equitable system, we would expect the average",average_units[metric],"to be the same across",paste(determinant,".",sep=""),"The graph above shows the ratio between the actual average",average_units[metric], "for",target,"visits and the equitable average",average_units[metric], "for each",paste(dimension,".",sep=""),"In a perfectly equitable",paste(dimension,",",sep=""),"the bar would be at 1.")
  targetValues = initialData %>% pull(!!as.symbol(target)) %>% remove_attributes("names")
  nudge = targetValues %>% sapply(abs) %>% mean() / 15
  return(ggplot(data=rawData, aes(x=reorder(!!as.symbol(dimension), Ratio), y=Ratio))
         + geom_bar(stat="identity")
         + coord_flip(clip="off")
         + geom_text(aes(y=Ratio + nudge*sign(Ratio),label = signif(Ratio, digits=2)))
         + ggtitle(title)
         + labs(x=dimension, y=yLabel1))#paste(yLabel1,str_wrap(yLabel2, width=120),sep="\n\n")))
}

oldCreateHighlight <- function(outputs, index, xVal, dimension, determinant, target,metric) {
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
  names(results)[3] <- paste("Equitable",metric)
  names(results)[4] <- paste(paste("Actual","Equitable",sep=separator),metric)
  return(results)
}

createHighlight <- function(outputs, index, dimension, determinant, target,metric, options) {
  firstPart <- paste("The bars represent the difference in total",absolute_units[metric],"of",metric,"among children with",determinant,"of",target,"compared to what it would be if average",average_units[metric],"was equal across",determinant,"groups.")
  secondPart <- "Click on a bar to see more details of how this graph works."
  result <- paste("<h3>",normalText,firstPart,"</span><b> ",secondPart,"</b></h3>")
  return(HTML(result))
}

createFullHighlight <- function(outputs, index, xVal, dimension, determinant, target,metric, options, setting, campus) {
  questions_df <- get_actual(dimension, options, setting, campus)
  actualTable <- data.frame(outputs[1], check.names=FALSE)
  totalTable <- data.frame(outputs[2], check.names=FALSE)
  actualDifference <- data.frame(outputs[6-index], check.names=FALSE)
  dimVal <- actualTable[xVal,1]
  allDim <- questions_df %>% filter(!!as.symbol(dimension)==dimVal) %>% pull(!!as.symbol(metric))
  avgDim <- mean(allDim)
  totalDim <- sum(allDim)
  numDim <- length(allDim)
  actual <- actualTable[xVal,target]
  if (index==1) {
    keyword <- "ratio"
    symbol <- "/"
  } else {
    keyword <- "difference"
    symbol <- "-"
  }
  if (directions[metric]) {
    directionality <- "highest"
  } else {
    directionality <- "lowest"
  }
  total <- totalTable[xVal,target]
  numPatients <- signif(total / actual, 6)
  difference <- actualDifference[xVal,target]
  equitable <- total - difference
  header <- "<h4>How this graph works:</h4><ol>"
  item1 <- paste("<li>The average",average_units[metric],"across all patients with",dimension,"of",dimVal, "was",round(avgDim,2),"for",paste("n=",numDim,sep=""),"patients,
yielding a total of",round(totalDim, 2),absolute_units[metric],"of",paste(metric,".</li>",sep=""))
  item2 <- paste("<li>Among the",numDim,"patients with",dimension,"of",paste(dimVal,",",sep=""),paste(round(100*numPatients/numDim,1),"%",sep=""), "are",target,"patients. Imagine that the average",average_units[metric], "was equal across all",determinant,"groups. In that case, we would expect the",numPatients,target,"patients to have",round(equitable, 2),"total",absolute_units[metric],"of",paste(metric,".</li>",sep=""))
  item3 <- paste("<li>In reality,",target,"patients had",round(total, 2), "total",absolute_units[metric],"of",paste(metric,".</li>",sep=""))
  item4 <- paste("<li>The",keyword,"of Actual",symbol,"Equitable is",paste(round(difference, 2),".</li>",sep=""))
  item5 <- paste("<li>The",dimension,"with the",directionality,"Actual",symbol,"Equitable value may give you the most \"bang for your buck\" to resolve the disparity in",metric,"by",determinant,"group globally.</li></ol>")
  return(HTML(paste(header, item1, item2, item3, item4, item5,sep="\n")))
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
    hide("Sorting")
    hide("Question")
    hide("DimLabel")
    hide("heading")
    hide("macroView")
    hide("Options")
    hide("Search")
    #for (speci in 1:length(specifications)) {
    #  specVals <- questions_df %>% distinct(!!as.symbol(specifications[speci])) %>% pull(!!as.symbol(specifications[speci])) %>% remove_attributes("names")
    #  updateSelectInput(session, specifications[speci], choices = c("All", specVals), selected="All")
    #}
    initialPossibilities <- questions_df %>% distinct(Insurance) %>% pull(Insurance) %>% remove_attributes("names")
    updateSelectInput(session, "Target", choices =initialPossibilities, selected=initialPossibilities[0])
    observeEvent(input$Determinant, {
      targetPossibilities <- eventReactive(input$Determinant, {questions_df %>% distinct(!!as.symbol(input$Determinant)) %>% pull(!!as.symbol(input$Determinant)) %>% remove_attributes("names")})
      updateSelectInput(session, "Target", choices =targetPossibilities(), selected=targetPossibilities()[0])
    })
    output$DiagnosticGraph <- renderPlot({diagnosticGraph(input$Determinant, input$Outcome, input$Setting, input$Campus, input$equitable=="Equitable")})
    observeEvent(input$Search, {
      
    })
    observeEvent(input$diveDeeper, {
      show("Target")
      if (input$Target!=""){
        show("Dimension")
        show("Question")
        show("Sorting")
        show("DimLabel")
        show("heading")
      }
      hide("diveDeeper")
      show("macroView")
      macro=FALSE
      updateSelectInput(session, "Target", selected="")
      updateSelectInput(session, "Dimension", choices = c("", "Zip Code", "Billing Diagnosis", "Admit Diagnosis"), selected="")
    })
    observeEvent(input$macroView, {
      updateSelectInput(session, "Target", selected="")
      hide("Target")
      hide("Dimension")
      hide("Sorting")
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
        show("Sorting")
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
        default <- "ActualAvg"
      } else {
        default <- input$Display
      }
      averageActual <- paste("I want to look for disparities in",average_units[input$Outcome],"within each",input$Dimension,"across",paste(input$Determinant,".",sep=""))
      specificDifference <- paste("Which",input$Dimension,"should I focus on to improve the disparity in",input$Outcome,"by",input$Determinant,"for",input$Target, "patients?")
      relativeDifference <- paste("Which",input$Dimension,"has the highest relative disparity in",input$Outcome,"for patients with",input$Determinant,"of",paste(input$Target,"?",sep=""))
      fullDifference <- paste("Which",input$Dimension,"contributes most to the disparity in",input$Outcome,"across",paste(input$Determinant,"?",sep=""))
      newChoices = list()
      newChoices[[ averageActual ]] <- "ActualAvg"
      newChoices[[ specificDifference ]] <- "Graph"
      newChoices[[ relativeDifference ]] <- "PartialDiff"
      newChoices[[ fullDifference ]] <- "GroupedGraph"
      updateSelectInput(session,"Display",choices=newChoices, selected=default)
    })
    
    observeEvent(input$Dimension, {
      output$SearchVal <- renderText({paste("Search", input$Dimension)})
      if (input$Dimension != "" & input$Target != "") {
        dimPossibilities = questions_df %>% distinct(!!as.symbol(input$Dimension)) %>% pull(!!as.symbol(input$Dimension)) %>% remove_attributes("names")
        updateCheckboxGroupInput(session, "Options", choices =dimPossibilities, selected=dimPossibilities[1])
        click("Options")
      }
    })
    
    observe({
      if (input$Dimension=="" | input$Target=="" | !(isTruthy(input$Options))) {
        if (input$Dimension=="" | input$Target=="") {
          hide("Options")
          hide("Search")
          hide("SelectAll")
        } else {
          show("Options")
          show("Search")
          show("SelectAll")
        }
        hide("specificHighlight")
        hide("relativeHighlight")
        hide("fullSpecific") 
        hide("fullRelative") 
        if (input$Target=="") {
          hide("Sorting")
          hide("Dimension")
        } 
        show("DiagnosticGraph")
        hide("Display")
        for (id in 1:length(ids)) {
          hide(ids[id])
        }
      } else {
        if (input$Sorting=="Custom") {
          show("Options")
          show("Search")
        } else {
          hide("Options")
          hide("Search")
        }
        show("Dimension")
        show("Sorting")
        show("Question")
        show("DimLabel")
        show("heading")
        show("Display")
        hide("DiagnosticGraph")
        if (input$Sorting=="Custom") {
          options <- input$Options
        } else {
          options <- getOptions(input$Dimension, input$Determinant, input$Outcome, input$Setting, input$Campus, input$Target)
        }
        outputs <- createTables(input$Dimension, input$Determinant, input$Outcome, options, input$Setting, input$Campus, input$Target)
        output$ActualAvg <- renderDataTable({data.frame(outputs[1], check.names=FALSE)})
        total <- data.frame(outputs[2], check.names=FALSE)
        expected <- data.frame(outputs[3], check.names=FALSE)
        difference <- data.frame(outputs[4], check.names=FALSE)
        output$ActualTot <- renderDataTable({total})
        output$Expected <- renderDataTable({expected})
        output$Difference <- renderDataTable({difference})
        output$RelativeTable <- renderDataTable({data.frame(outputs[5], check.names=FALSE)})
        output$SpecificTable <- renderDataTable({createSpecific(total, expected, difference, input$Target)})
        output$GroupedGraph <- renderPlot({groupedBar(input$Dimension, input$Determinant, input$Outcome, options, input$Setting, input$Campus, input$Target, input$equitable=="Equitable")})
        output$GroupedActualGraph <- renderPlot({groupedActual(input$Dimension, input$Determinant, input$Outcome, options, input$Setting, input$Campus, input$Target, input$equitable=="Equitable")})
        output$Graph <- renderPlot({isolatedBar(input$Dimension, input$Determinant, input$Outcome, options, input$Setting, input$Campus, input$Target, input$equitable=="Equitable")})
        output$RelativeGraph <- renderPlot({isolatedRelative(input$Dimension, input$Determinant, input$Outcome, options, input$Setting, input$Campus, input$Target, input$equitable=="Equitable")})
        output$specificHighlight <- renderUI({createHighlight(outputs, 2, input$Dimension, input$Determinant, input$Target,input$Outcome, options)})
        output$relativeHighlight <- renderUI({createHighlight(outputs, 1, input$Dimension, input$Determinant, input$Target,input$Outcome, options)})
        click("Options")
        if (!is.null(input$specificClick)) {
          specx <- input$specificClick$x
          specy <- input$specificClick$y
          if (input$Display=="Graph" & abs(specy - round(specy)) < 0.4 & specy > 0 & round(specy) <= nrow(difference)){
            actualIdx <- match(arrange(difference,!!as.symbol(input$Target))[round(specy),input$Target], pull(difference,!!as.symbol(input$Target)))
            actualVal = difference[actualIdx,input$Target]
            if (specx > min(actualVal, 0) & specx < max(0,actualVal)) {
              output$fullSpecific <- renderUI({createFullHighlight(outputs, 2, actualIdx, input$Dimension, input$Determinant, input$Target,input$Outcome, options, input$Setting, input$Campus)})
              show("fullSpecific")
            }
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
              output$fullRelative <- renderUI({createFullHighlight(outputs, 1, actualIdx, input$Dimension, input$Determinant, input$Target,input$Outcome, options, input$Setting, input$Campus)})
              show("fullRelative")
            }
          } 
        } 
        for (id in 1:length(ids)) {
          if (displayDf[id,match(input$Display, colnames(displayDf))]==1) {
            show(ids[id])
            if (ids[id]=="Graph") {
              show("specificHighlight")
            } 
            if (ids[id]=="RelativeGraph") {
              show("relativeHighlight")
            }
          } else {
            if (ids[id]=="Graph") {
              hide("specificHighlight")
              hide("fullSpecific")
            }
            if (ids[id]=="RelativeGraph") {
              hide("relativeHighlight")
              hide("fullRelative")
            }
            hide(ids[id])
          }
        }
      }
    })
})
