library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)

config <- fromJSON(file="config.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(config$graphUrl, username=config$username, password=config$password)



ui <- fluidPage(
  titlePanel("Data Uploader"),
  sidebarLayout(
    sidebarPanel(
      selectInput("entityLabel","Entity Type", config$labels, selected = "Gene"),
      uiOutput("fileInputWrapper"),
      tags$hr(),
      uiOutput("requiredFields"),
      tags$hr(),
      uiOutput("relationshipNameWrapper"),
      uiOutput("ToFieldInFromNodeWrapper"),
      uiOutput("uploadBtn")
    ),
    
    mainPanel(
      tableOutput("contents")
    )
  )
)


entity <- NULL

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  output$fileInputWrapper <- renderUI({
    entity <<- input$entityLabel
    fileInput("file1", paste0("Upload ",input$entityLabel," file"), multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  })
  
  output$contents <- renderTable({
    
    req(input$file1)
    tryCatch(
      {
        df <- fread(input$file1$datapath)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    
    selectField <- "Select"
    
    output$requiredFields <- renderUI({
      if (is.null(df)) return(NULL)
      nodeProperties <- config[[entity]]$properties
      lapply(1:length(nodeProperties), function(i) {
        selectInput(nodeProperties[i], strong(paste0('Select ', nodeProperties[i])), c(list(selectField), names(df)))
      })
    })
    
    output$ToFieldInFromNodeWrapper <- renderUI({
      if (is.null(df) || !config[[entity]]$createRelation) return(NULL)
      selectInput("ToFieldInFromNode","Select TO field in FROM node", c(list(selectField), names(df)))
    })
    
    output$relationshipNameWrapper <- renderUI({
      if (is.null(df) || !config[[entity]]$createRelation) return(NULL)
      selectInput("relationshipName", "Select Name of the Relationship", c(list(selectField), config$Relationships))
    })

    output$uploadBtn <- renderUI({
      if (is.null(df)){
        return(NULL)
      }
      nodeProperties <- config[[entity]]$properties
      for (field in nodeProperties) {
        if(is.null(input[[field]]) || input[[field]] == selectField){
          return(NULL)
        }
      }
      if(config[[entity]]$createRelation && (is.null(input[["relationshipName"]]) || input[["relationshipName"]] == selectField || is.null(input[["ToFieldInFromNode"]]) || input[["ToFieldInFromNode"]] == selectField)){
        return(NULL)
      }
      actionButton("upload", "Upload", class = "btn-primary")
    })
    
    observeEvent(input$upload, {
      print("uploading")
      if (is.null(df)){
        return(NULL)
      }
      nodeProperties <- config[[entity]]$properties
      for (field in nodeProperties) {
        if(is.null(input[[field]]) || input[[field]] == selectField){
          return(NULL)
        }
      }
      if(config[[entity]]$createRelation && (is.null(input[["relationshipName"]]) || input[["relationshipName"]] == selectField || is.null(input[["ToFieldInFromNode"]]) || input[["ToFieldInFromNode"]] == selectField)){
        return(NULL)
      }
      nodesPerRequest <- 100
      progress <- Progress$new(session, min=1, max=nrow(df)/nodesPerRequest)
      on.exit(progress$close())
      progress$set(message = paste("Uploading ",input$entityLabel," data to Neo4j"), detail = 'This may take a while...')
      
      constraint <- getConstraint(graph, input$entityLabel)
      if(is.null(constraint) || nrow(constraint)==0){
        addConstraint(graph,input$entityLabel, "id")
      }
      
      propertiesList <- list()
      relationshipList <- list()
      entity <- input$entityLabel
      nodeProperties <- config[[entity]]$properties
      if(config[[entity]]$createRelation){
        relationshipName <- input[["relationshipName"]]
        relationshipConfig <- config[[relationshipName]]
      }
      counter <- 0
      print(input[["id"]])
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
        if(config[[entity]]$createRelation){
          relationship <- list()
          relationship[["fromId"]] = df[[input[[relationshipConfig$fromField]]]][i]
          relationship[["toId"]] = df[[input[["ToFieldInFromNode"]]]][i]
          relationshipList[[counter]] = relationship
        }
        if (counter==nodesPerRequest) {
          nodeQuery <- paste0("UNWIND {propertiesList} AS properties MERGE (n:",entity," {id: properties.id}) SET n = properties")
          print(i)
          cypher(graph, nodeQuery, propertiesList = propertiesList)
          if(config[[entity]]$createRelation){
            relationQuery <- paste0("UNWIND {relationshipList} AS relationship
              MATCH (a:",relationshipConfig$fromNode," {",relationshipConfig$fromField,": relationship.fromId}),
                (b:",relationshipConfig$toNode," {",relationshipConfig$toField,": relationship.toId})
                MERGE (a)-[r:",relationshipName,"]->(b)")
            cypher(graph, relationQuery, relationshipList = relationshipList)
          }
          progress$set(value = i %/% nodesPerRequest)
          counter <- 0
          break
        }
      }
    })
    return(head(df))
  })
}

#, "frequency", "dBType", "modifier", "sex", "aspect"
shinyApp(ui, server)