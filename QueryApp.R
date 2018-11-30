source("packageInstaller.R")

library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)
library(visNetwork)
library(DT)
library(shinythemes)
library(shinyjs)
library(vcfR)

config <- fromJSON(file="config.json")
credentials <- fromJSON(file="credentials.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(credentials$graphUrl, username=credentials$username, password=credentials$password)



ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  # App title ----
  headerPanel("Query Explorer"),
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput('suggestionsInput', 'Enter the Phenotype', choices = NULL, multiple=TRUE, selected = NULL, options=list(placeholder="eg: Elevated sweat chloride")),
      uiOutput("suggestions"),
      uiOutput("temp"),
      hidden(
        p(
          id = "hiddenCredElement",
          paste0('{"url":"',credentials$graphUrl,'","username":"',credentials$username,'","password":"',credentials$password,'"','}')
        )
      ),
      fileInput("file1", "Upload Genotype VCF File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".vcf")
      ),
      actionButton("createSearchBtn", "Search", class = "btn-primary"),
      width = 4
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      tableOutput("contents")
      # tabsetPanel(
      #   id="outputTabs",
      #   tabPanel("Plot", visNetworkOutput("plot")),
      #   tabPanel("Table", dataTableOutput("results")),
      #  selected = "Plot"
      # )
    )
  )
)
server <- function(input, output, session) {
  
  output$suggestions <- renderUI({
    entity <<- input$entityLabel
    entityField <<- input$entityField
    updateSelectizeInput(session, 'suggestionsInput', choices = NULL, selected = NULL, options = list(
      options = list(),
      create = FALSE,
      valueField = 'row',
      labelField = 'row',
      searchField = 'row',
      placeholder = 'Type for suggestions',
      render = I("{
                 option: function(item, escape) {
                 return '<div>' + item.row[0] + '</div>';
                 }
  }"),
      load = I("function(query, callback) {
               if (!query.length) return callback();
               var cred = JSON.parse($('#hiddenCredElement').html());
               var authorisation = 'Basic ' + btoa(cred.username + ':' + cred.password);
               var entity = $('#entityLabel').val();
               var entityField = $('#entityField').val();
               var neoQuery = 'MATCH (n:Phenotype) WHERE toString(n.id) CONTAINS '+JSON.stringify(query)+' RETURN n.id AS field LIMIT 100';
               $.ajaxSetup({
               headers: {
               'Authorization': authorisation
               }
               });
               $.ajax({
               url: cred.url+'transaction/commit',
               type: 'POST',
               data: JSON.stringify({ 'statements': [{ 'statement': neoQuery }] }),
               contentType: 'application/json',
               accept: 'application/json; charset=UTF-8' 
               }).done(function (data) {
               callback(data.results[0].data)
               });
      }")
    ))
    return(NULL)
    })
  output$contents <- renderTable({
    req(input$file1)
    tryCatch({
      df <- read.vcfR(input$file1$datapath)
      print(df)
    },
    error = function(e) {
      stop(safeError(e))
    })
    return(head(df))
  })
  }

shinyApp(ui,server)