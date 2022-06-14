library(shiny)
library(shinythemes)
library(shinyjs )

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
