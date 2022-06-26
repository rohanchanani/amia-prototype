library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(readr)
library(stringr)
library(tableHTML)

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

fillPage(theme = shinytheme("united"),
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  headerPanel("Equity Dashboard"),
                  sidebarPanel(
                             useShinyjs(),
                             HTML("<h5 style='font-weight:bold;'>My population of interest is:</h5>"),
                             
                             selectInput("Setting", label = "Setting:", 
                                         choices = c()),
                             
                             selectInput("Campus", label = "Campus:", 
                                         choices = c()),
                             
                             selectInput("Outcome", label = "My outcome of interest is:", 
                                         choices = c("Readmissions", "Length of Stay"), selected="Readmissions"),
                             
                             selectInput("Determinant", label = "I want to look for disparities by:", 
                                         choices = c("Insurance"), 
                                         selected = "Insurance"),
                             tags$head(tags$style(make_css(list('.btn', 'white-space', 'pre-wrap')))),
                             
                             actionButton("diveDeeper", "After you have found a disparity, dive deeper to look at sub-populations contributing to that disparity", width="100%"),
                             actionButton("macroView", "View Summary"),
                             
                             selectInput("Target", label="The group I worry may be disadvantaged is:", choices=c()),
                             
                             HTML("<h5 id='heading' style='font-weight:bold;'>Refine by Sub-Populations:</h5>", ),
                             
                             tags$div(id = "inline", selectInput("Dimension", label = "Which",choices = c("", "Zip Code", "Disease"), selected="")),
                             
                             HTML(paste("<label for='Dimension' id='DimLabel'>",textOutput("Question"),"</label>", sep="")),
                             
                             selectInput("Display", label="Display:", choices=list("Graph"="Graph", "Full Graph"="GroupedGraph", "Actual (Average)"="ActualAvg", "Actual (Total)"="ActualTot", "Expected"="Expected", "Difference"="Difference"), selected="Graph")
         
                           ), # sidebarPanel
                           mainPanel(
                             dataTableOutput("ActualAvg"),
                             dataTableOutput("ActualTot"),
                             dataTableOutput("Expected"),
                             dataTableOutput("Difference"),
                             plotOutput("DiagnosticGraph", width="80%"),
                             plotOutput("GroupedGraph", width="80%"),
                             plotOutput("Graph", width="80%")
                           ) # mainPanel
          )
