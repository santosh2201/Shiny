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
      textInput('PreSuggestionsText', 'Enter to populate suggestions'),
      selectizeInput('suggestionsInput', 'Select search input', choices = NULL, multiple = FALSE, selected = NULL),
      uiOutput("defaultSuggestions"),
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
  
  output$defaultSuggestions <- renderUI({
    entityField <<- input$entityField
    if(is.null(entityField)) return(NULL)
    suggestionsQuery <- paste0("MATCH (n:",entity,") RETURN DISTINCT n.",entityField," as choices LIMIT 100")
    #print(suggestionsQuery)
    choices <- cypher(graph, suggestionsQuery)
    #print(choices)
    #selectizeInput('suggestionsInput', 'Select search input', choices = choices, multiple = FALSE)
    updateSelectizeInput(session, 'suggestionsInput', choices = c("", choices), selected = NULL, options = list())
    return(NULL)
  })
  
  
  output$suggestions <- renderUI({
    selectText <<- input$PreSuggestionsText
    print("test")
    print(selectText)
    if(is.null(entityField)) return(NULL)
    #selectText <<- selectText
    suggestionsQuery <- paste0("MATCH (n:",entity,") WHERE toString(n.",entityField,") CONTAINS {selectText} RETURN DISTINCT n.",entityField," as choices LIMIT 100")
    print(suggestionsQuery)
    choices <- cypher(graph, suggestionsQuery, selectText = selectText)
    print(choices)
    #selectizeInput('suggestionsInput', 'Select search input', choices = choices, multiple = FALSE)
    updateSelectizeInput(session, 'suggestionsInput', choices = c("", choices), selected = NULL, options = list())
    return(NULL)
  })
}

shinyApp(ui, server)