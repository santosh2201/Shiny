library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)
library(assertive)

config <- fromJSON(file="config.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(config$graphUrl, username=config$username, password=config$password)





ui <- fluidPage(
  # App title ----
  titlePanel("Query Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("entityLabel","Entity Type", config$labels),
      uiOutput("entityFields"),
      textInput('PreSuggestionsText', 'Enter to populate suggestions'),
      selectizeInput('suggestionsInput', 'Select search input', choices = NULL, multiple = FALSE, selected = NULL),
      uiOutput("defaultSuggestions"),
      uiOutput("suggestions"),
      selectInput("searchType","Select search Criteria", config$SearchType),
      tags$hr(),
      uiOutput("tempCode"),
      uiOutput("searchbtn")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("results")
      
    )
  )
  
)

id <- NULL
searchType <- NULL
entity <- "OMIM"
entityField <- NULL

# Define server logic to read selected file ----
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
  
  
  output$searchbtn <- renderUI({
    actionButton("createSearchBtn", "Search", class = "btn-primary")
  })
  
  
  output$tempCode <- renderUI({
    id <<- input$suggestionsInput
    searchType <<- input$searchType
    df <- data.frame()
    if(input$searchType=="Gene-Name" || input$searchType == "EnsembleID" || input$searchType == "Phenotype-Series-Number" ){
      id <<- paste0("'", id, "'")
    }
    return(NULL)
  })
  
  
  observeEvent(input$createSearchBtn, {
    output$results <- renderTable({
    
      if(is.null(id) || id==""){
        return(NULL)
      }
        
        query <-
        paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(MIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where Gene.id = ",id," or Gene.ensembleID = ",id," or Gene.symbol = ",id," or MIM.id = ",id," or Phenotype.id = ",id,"
                RETURN MIM.name,MIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Phenotype.id,Phenotype.aspect,Gene.summary")
        
      df <- cypher(graph,query)
      
      if( searchType != "Phenotype-Series-Number")
      if(is.null(nrow(df))){
        query <-
        paste0("MATCH (MIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where Gene.id = ",id," or Gene.ensembleID = ",id," or Gene.symbol = ",id," or MIM.id = ",id,"
                RETURN MIM.name,MIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Gene.summary")
        df <- cypher(graph,query)
      }
      
      
      if(searchType != "EnsembleID")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(MIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                  where Gene.id = ",id," or Gene.symbol = ",id," or MIM.id = ",id," or Phenotype.id = ",id,"
                RETURN MIM.name,MIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Phenotype.id,Phenotype.aspect,Gene.summary")
        df <- cypher(graph,query)
      }

      if( searchType != "EnsembleID" && searchType != "Phenotype-Series-Number")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (MIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                 where Gene.id = ",id," or Gene.symbol = ",id," or MIM.id = ",id,"
                 RETURN MIM.name,MIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Gene.summary")
        df <- cypher(graph,query)
      }

      if(searchType == "Phenotype-Series-Number" || searchType == "mimID")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(MIM:OMIM)
                  where MIM.id = ",id," or Phenotype.id = ",id,"
                RETURN MIM.name,MIM.id,Phenotype.id,Phenotype.aspect")
        df <- cypher(graph,query)
      }
      
      if(searchType != "Phenotype-Series-Number" || searchType != "mimID")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where Gene.id = ",id," or Gene.ensembleID = ",id," or Gene.symbol = ",id,"
                RETURN Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Gene.summary")
        df <- cypher(graph,query)
        }
     
      
      if(searchType == "EntrezID" || searchType == "Gene-Name")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Gene:Gene)
                  where Gene.id = ",id," or Gene.symbol = ",id,"
                RETURN Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Gene.summary")
        df <- cypher(graph,query)
        }
      
      
      if(searchType != "mimID")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (MIM:OMIM)
                 where MIM.id = ",id,"
                 RETURN MIM.name,MIM.id,MIM.Prefix")
        df <- cypher(graph,query)
      }
      
      
      if(searchType == "Phenotype-Series-Number")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)
                  where Phenotype.id = ",id,"
                RETURN Phenotype.id,Phenotype.aspect,Phenotype.frequency,Phenotype.dBType")
        df <- cypher(graph,query)
      }
      
      
      if(is.null(nrow(df))){
        return("No results Found")
      }
      
      
      
      return (df)
      
    })
    
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)