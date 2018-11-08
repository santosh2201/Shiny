source("packageInstaller.R")

library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)

config <- fromJSON(file="config.json")
credentials <- fromJSON(file="credentials.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(credentials$graphUrl, username=credentials$username, password=credentials$password)

# Variables
entity <- NULL
selectField <- "Select"
df <- NULL
nodesPerRequest <- 500

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Data Uploader"),
  sidebarLayout(
    sidebarPanel(
      selectInput("entityLabel","Entity Type", config$labels, selected = "Gene"),
      uiOutput("fileInputWrapper"),
      checkboxInput("createNode", "Create Node", value = FALSE, width = NULL),
      uiOutput("nodeFields"),
      checkboxInput("createRelation", "Create Relationship", value = FALSE, width = NULL),
      uiOutput("relationshipNameWrapper"),
      uiOutput("ToFieldInFromNodeWrapper"),
      uiOutput("showError"),
      actionButton("upload", "Upload", class = "btn-primary")
    ),
    
    mainPanel(
      tableOutput("contents")
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Gets updated on change in entity select box
  output$fileInputWrapper <- renderUI({
    entity <<- input$entityLabel
    df <<- NULL
    fileInput("file1", paste0("Upload ", entity," file"), multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  })
  
  output$contents <- renderTable({
    req(input$file1)
    tryCatch({
      df <<- fread(input$file1$datapath)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    output$nodeFields <- renderUI({
      if (is.null(df) || !input$createNode) return(NULL)
      nodeProperties <- config[[entity]]$properties
      lapply(1:length(nodeProperties), function(i) {
        selectInput(nodeProperties[i], strong(paste0('Select ', nodeProperties[i])), c(list(selectField), names(df)))
      })
    })
    
    output$ToFieldInFromNodeWrapper <- renderUI({
      if (is.null(df) || !input$createRelation) return(NULL)
      selectInput("ToFieldInFromNode","Select TO field in FROM node", c(list(selectField), names(df)))
    })
    
    output$relationshipNameWrapper <- renderUI({
      if (is.null(df) || !input$createRelation) return(NULL)
      selectInput("relationshipName", "Select Name of the Relationship", c(list(selectField), config$Relationships))
    })
    
    return(head(df))
  })
  
  
  observeEvent(input$upload, {
    # Below line to make this reactive element work
    error <- FALSE
    errorMsg <- NULL
    if (is.null(df)){
      error = TRUE
      errorMsg = "Please upload an input file"
    }else if(!error && !input$createNode && !input$createRelation){
      error = TRUE
      errorMsg = "Please create either Node or Relation"
    }
    
    nodeProperties <- config[[entity]]$properties
    if(!error && input$createNode){
      for (field in nodeProperties) {
        if(is.null(input[[field]]) || input[[field]] == selectField){
          error = TRUE
          errorMsg = "Please select all node properties"
        }
      } 
    }
    if(!error && input$createRelation && (is.null(input[["relationshipName"]]) || input[["relationshipName"]] == selectField || is.null(input[["ToFieldInFromNode"]]) || input[["ToFieldInFromNode"]] == selectField)){
      error = TRUE
      errorMsg = "Please select relationship fields"
    }
    
    output$showError <- renderText({
      if(error){
        stop(safeError(errorMsg))
      }
      return(NULL)
    })
    if(error){
      return(NULL)
    }
    
    progress <- Progress$new(session, min=1, max=nrow(df)/nodesPerRequest)
    on.exit(progress$close())
    progress$set(message = paste("Uploading ",entity," data to Neo4j"), detail = 'This may take a while...')
    
    constraint <- getConstraint(graph, entity)
    if(is.null(constraint) || nrow(constraint)==0){
      addConstraint(graph, entity, "id")
    }
    
    propertiesList <- list()
    relationshipList <- list()
    nodeProperties <- config[[entity]]$properties
    if(input$createRelation){
      relationshipName <- input[["relationshipName"]]
      relationshipConfig <- config[[relationshipName]]
    }
    counter <- 0
    for (i in 1:nrow(df)) {
      properties <- list()
      if(is.na(df[[input[["id"]]]][i])){
        next
      }
      for (field in nodeProperties) {
        properties[[field]] <- df[[input[[field]]]][i]
      }
      
      counter <- counter+1
      propertiesList[[counter]] = properties
      if(input$createRelation){
        relationship <- list()
        relationship[["fromId"]] = df[[input[[relationshipConfig$fromField]]]][i]
        relationship[["toId"]] = df[[input[["ToFieldInFromNode"]]]][i]
        relationshipList[[counter]] = relationship
      }
      if (counter==nodesPerRequest) {
        nodeQuery <- paste0("UNWIND {propertiesList} AS properties MERGE (n:",entity," {id: properties.id}) SET n = properties")
        cypher(graph, nodeQuery, propertiesList = propertiesList)
        if(input$createRelation){
          relationQuery <- paste0("UNWIND {relationshipList} AS relationship
                                  MATCH (a:",relationshipConfig$fromNode," {",relationshipConfig$fromField,": relationship.fromId}),
                                  (b:",relationshipConfig$toNode," {",relationshipConfig$toField,": relationship.toId})
                                  MERGE (a)-[r:",relationshipName,"]->(b)")
          cypher(graph, relationQuery, relationshipList = relationshipList)
        }
        progress$set(value = i %/% nodesPerRequest)
        counter <- 0
      }
    }
  })
}

shinyApp(ui, server)