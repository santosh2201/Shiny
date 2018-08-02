library(shiny)
library(RNeo4j)
options(shiny.maxRequestSize=30*1024^2)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="nsr")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),

      uiOutput("properties"),
      uiOutput("nameFieldProperty"),
      uiOutput("constraint"),
      uiOutput("uploadBtn")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        
        stop(safeError(e))
      }
    )
    output$properties <- renderUI({
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("properties","Select properties",items,multiple=TRUE)
    })
    
    output$nameFieldProperty <- renderUI({
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("nameFieldProperty","Select name of the node from properties",items)
    })
    
    output$constraint <- renderUI({
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("constraint","Select constraint which acts as unique key from properties",items)
    })
    
   #show upload button only when properties are selected
    observe({
      x <- input$properties
      
      output$uploadBtn <- renderUI({
        if (is.null(x) || length(x)==0) {
          x <- character(0)
          return(NULL)
        } else {
          actionButton("upload", "Upload", class = "btn-primary") 
        }
      })
    })
    

    
    observeEvent(input$upload, {
      x <- input$properties
      progress <- Progress$new(session, min=1, max=nrow(df))
      on.exit(progress$close())
      
      progress$set(message = 'Uploading OMIM data to Neo4j',
                   detail = 'This may take a while...')
      
      nameField = input$nameFieldProperty
      if(is.null(nameField)){
        stop(safeError("Please select a name field"))
      }
      constraint = input$constraint
      df <- df[, x]
      for (i in 1:nrow(df)) {
        propertiesList <- list()
        for (j in names(df)) {
          if(j==nameField){
            propertiesList[["name"]] <- df[[j]][i]
          }else if(j==constraint){
            propertiesList[["id"]] <- df[[j]][i]
          }else{
            propertiesList[[j]] <- df[[j]][i]
          }
        }
        #createNode(graph, "OMIM", name=df$Symbol[i], id=df$MimNumber[i], prefix=df$Prefix[i])
        createNode(graph, "OMIM", propertiesList)
        progress$set(value = i)
        #Sys.sleep(0.5)
      }
      if(!is.null(constraint)){
        addConstraint(graph, "OMIM", constraint) 
      }
    })
    return(head(df))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)