library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)


config <- fromJSON(file="config.json")
graph = startGraph(config$graphUrl, username=config$username, password=config$password)
entity <- "OMIM"
entityField <- NULL

ui <- fluidPage(
  titlePanel("Query Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("entityLabel","Entity Type", config$labels, selected = entity),
      uiOutput("entityFields"),
      uiOutput("suggestions")
    ),
    
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output, session) {
  output$entityFields <- renderUI({
    entity <<- input$entityLabel
    selectInput("entityField", strong(paste0('Select a ', entity, ' property')), config[[entity]]$searchProperties)
  })
  
  output$suggestions <- renderUI({
    entity <<- input$entityLabel
    entityField <<- input$entityField
    print("before")
    if(is.null(entityField)) return(NULL)
    print("after")
    suggestionsQuery <- paste0("MATCH (n:",entity,") RETURN DISTINCT n.",entityField," as choices")
    choices <- cypher(graph, suggestionsQuery)
    selectizeInput('suggestionsInput', 'Select search input', choices = c("", choices), multiple = FALSE, selected = NULL, options = list(placeholder = 'Type for suggestions'))
  })
  
}

shinyApp(ui, server)