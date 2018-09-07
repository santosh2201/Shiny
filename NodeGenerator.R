library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)

config <- fromJSON(file="config.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(config$graphUrl, username=config$username, password=config$password)


# Define UI for data upload app ----
createNodeUI <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("entityLabel","Entity Type", config$labels,selected = "Gene"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      uiOutput("requiredFields"),

      uiOutput("uploadBtn")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
  )
)



createRelationshipUI <- fluidPage(
  # App title ----
  titlePanel("Creating Relationships"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("node1","Select Node 1", config$labels),
      selectInput("node2","Select Node 2", config$labels),
      
      uiOutput("node1Fields"),
      uiOutput("node2Fields"),
      
      selectInput("relationshipName", "Select Name of the Relationship", config$Relationships),
      
      uiOutput("createRelationElement")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      #uiOutput("relationshipUI")
      
    )
  )
  
)



QueryUI <- fluidPage(
  # App title ----
  titlePanel("Search Query"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("searchType","Select search Criteria", config$SearchType),
      textInput("searchValue", "Input the Fiels", value = "", width = NULL, placeholder = NULL),
      
      tags$hr(),
      uiOutput("searchbtn")
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      tableOutput("results")
    )
  )
)



# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  selectField <- "Select"
  error <- FALSE
  output$node1Fields <- renderUI({
    selectInput("node1Fields","Select a property from node 1", config[[input$node1]])
  })
  
  output$node2Fields <- renderUI({
    selectInput("node2Fields","Select a property from node 2", config[[input$node2]])
  })
  
  output$createRelationElement <- renderUI({
    actionButton("createRelationBtn", "Generate Relationships", class = "btn-primary")
  })
  

  
  
    
  output$searchbtn <- renderUI({
    actionButton("createSearchBtn", "Search", class = "btn-primary")
  })
  
  observeEvent(input$createSearchBtn, {
    output$results <- renderTable({
      print(input$searchType)
      if(input$searchType == "EnsembleID"){
        query <- "MATCH p=(n:GeneBody)-[]->()-[]->() where n.id ={id} RETURN p"
        query <- paste0("MATCH p=(n:GeneBody)-[]->()-[]->() where n.id ='",input$searchValue,"' RETURN n.name,n.id,n.start,n.end,n.strand")
      }
      df <- cypher(graph,query)
      return (df)
    })
  })
  
  
 
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- fread(input$file1$datapath)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    output$requiredFields <- renderUI({
      if (is.null(df)) return(NULL)
      choices <- c(list(selectField), names(df))
      lapply(1:length(config[[input$entityLabel]]), function(i) {
        selectInput(config[[input$entityLabel]][i], strong(paste0('Select ', config[[input$entityLabel]][i])), choices)
      })
    })
    
    output$uploadBtn <- renderUI({
      if (is.null(df)) return(NULL)
      actionButton("upload", "Upload", class = "btn-primary")
    })
    
    observeEvent(input$upload, {

      progress <- Progress$new(session, min=1, max=nrow(df))
      on.exit(progress$close())
      progress$set(message = paste("Uploading ",input$entityLabel," data to Neo4j"), detail = 'This may take a while...')

      #adding constraint
      constraint <- getConstraint(graph, input$entityLabel)
      if(is.null(constraint)){
        addConstraint(graph,input$entityLabel, "id")
      }

      for (i in 1:nrow(df)) {
        propertiesList <- list()
        if(is.na(df[[input[["id"]]]][i])){
          next
        }
        for (field in config[[input$entityLabel]]) {
          propertiesList[[field]] <- df[[input[[field]]]][i]
        }
        getOrCreateNode(graph, input$entityLabel, propertiesList)
        progress$set(value = i)
      }
    })
    return(head(df))
  })
  
  observeEvent(input$createRelationBtn, {
    label1 <- input$node1
    label2 <- input$node2
    field1 <- input$node1Fields
    field2 <- input$node2Fields
    query <- paste0("MATCH (l1:",label1,") RETURN l1.",field1)
    label1Ids <- cypherToList(graph, query)
    
    progress <- Progress$new(session, min=1, max=length(label1Ids))
    on.exit(progress$close())
    progress$set(message = paste("Creating relationships"), detail = 'This may take a while...')
    
    count <- 0
    for(i in label1Ids){
      id = i[[1]]
      if(id=="" || is.na(id)){
        next
      }
      if(is.character(id)){
        id = paste0("'", id, "'")
      }
      query <- paste0("MATCH (l1:",label1,") WHERE l1.",field1," = ",id," RETURN l1")
      leftNodes <- cypherToList(graph, query)
      query <- paste0("MATCH (l2:",label2,") WHERE l2.",field2," = ",id," RETURN l2")
      rightNodes <- cypherToList(graph, query)
      count <- count+1
      progress$set(value = count)
      if(length(rightNodes)==0){
        next
      }
      for (leftNode in leftNodes) {
        for (rightNode in rightNodes) {
          createRel(leftNode$l1, input$relationshipName, rightNode$l2)
        }
      }
    }
  })
  
}

ui <- fluidPage(tabsetPanel(
  tabPanel("Create Nodes", createNodeUI),
  tabPanel("Create Relationships", createRelationshipUI),
  tabPanel("Query", QueryUI)))

# Create Shiny app ----
shinyApp(ui, server)