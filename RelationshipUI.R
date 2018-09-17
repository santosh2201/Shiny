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