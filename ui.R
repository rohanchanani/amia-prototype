library(shiny)
library(shinythemes)
library(shinyjs )

fillPage(theme = shinytheme("united"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Sample App",
                  tabPanel("Categorical",
                           sidebarPanel(
                             useShinyjs(),
                             HTML("<h3>Input parameters</h3>"),
                             
                             selectInput("categorical1", label = "First Strata:", 
                                         choices = list("Section" = "Section", "Company" = "Company", "Type" = "Type"), 
                                         selected = "Section"),
                             
                             selectInput("categorical2", label = "Second Strata:", 
                                         choices = list("Type" = "Type", "Company" = "Company", "Section" = "Section"), 
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
                                         choices = list("Section" = "Section", "Company" = "Company", "Type" = "Type"), 
                                         selected = "Section"),
                             
                             selectInput("numeric2", label = "Second Strata:", 
                                         choices = list("Type" = "Type", "Company" = "Company", "Section" = "Section"), 
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