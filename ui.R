library(shiny)
library(shinythemes)
library(shinyjs )

fillPage(theme = shinytheme("united"),
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  headerPanel("Equity Dashboard"),
                  sidebarPanel(
                             useShinyjs(),
                             HTML("<h3>Input parameters</h3>"),
                             
                             selectInput("dimension", label = "Dimension:", 
                                         choices = list("Disease"="Disease", "Zip Code"="Zip Code"), selected="Zip Code"),
                             
                             selectInput("determinant", label = "Social Determinant of Health:", 
                                         choices = list("Insurance"="Insurance"), 
                                         selected = "Insurance"),
                             
                             selectInput("outcome", label = "Outcome Metric:", 
                                         choices = list("Readmissions"="Readmissions", "Length of Stay"="Length of Stay"), 
                                         selected = "Readmissions"),
                             
                             selectInput("display", label="Display:", choices=list("Graph"="graph", "Actual"="value", "Expected"="expected","Difference"="difference"), selected="Graph")
                           ), # sidebarPanel
                           mainPanel(
                             dataTableOutput("value"),
                             dataTableOutput("expected"),
                             dataTableOutput("difference"),
                             plotOutput("graph", width="80%")
                           ) # mainPanel
          )
