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