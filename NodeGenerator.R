library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)
library(visNetwork)

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
      tags$hr(),
      uiOutput("searchbtn")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      visNetworkOutput("results")
      
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
  
  
  
  observeEvent(input$createSearchBtn, {
    output$results <- renderVisNetwork({
      
      id <<- input$suggestionsInput
      
      if(is.null(id) || id==""){
        return(NULL)
      }
        entity <<- input$entityLabel
        entityField <<- input$entityField
        df <- data.frame()
        
        
        
        if((entity == "Gene" || entity == "OMIM") && entityField == "id"){
          print('numeric')
        }else {
          id <<- paste0("'", id, "'")
        }
        
        
        query <-
        paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where ",entity,".",entityField ," = ",id," 
                RETURN OMIM.name,OMIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Phenotype.id,Phenotype.aspect,Gene.summary")
        
        
      df <- cypher(graph,query)
      nodes <- data.frame(id=union(df$OMIM.id, union(df$Gene.id,  df$Phenotype.id)),label=union(df$OMIM.name, union(df$Gene.name,  df$Phenotype.id)))
      print("dataframe is");
      print(nodes)
      
      query <- paste0( "MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]->(OMIM:OMIM)<-[t:GeneToOMIM]
      -(Gene:Gene)-[s:GeneLocates]->(GeneBody:GeneBody) where ",entity,".",entityField ," = ",id," 
      RETURN Gene.id as from, OMIM.id as to, Type(t) As label 
      UNION 
      MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]->(OMIM:OMIM)<-[t:GeneToOMIM]
      -(Gene:Gene)-[s:GeneLocates]->(GeneBody:GeneBody) where ",entity,".",entityField ," = ",id," 
      RETURN Phenotype.id as from, OMIM.id as to, Type(u) As label" )
      
      edges <- cypher(graph,query)
      print(edges)
      
      visNetwork(nodes, edges)
      
      if( entity != "Phenotype")
      if(is.null(nrow(df))){
        query <-
        paste0("MATCH (OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where ",entity,".",entityField ," = ",id,"
                RETURN OMIM.name,OMIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Gene.summary")
        df <- cypher(graph,query)
      }
      
      
      if(entity != "GeneBody")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                  where ",entity,".",entityField ," = ",id,"
                RETURN OMIM.name,OMIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Phenotype.id,Phenotype.aspect,Gene.summary")
        df <- cypher(graph,query)
      }

      if( entity != "GeneBody" && entity != "Phenotype")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                 where ",entity,".",entityField ," = ",id,"
                 RETURN OMIM.name,OMIM.id,Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Gene.summary")
        df <- cypher(graph,query)
      }

      if(entity == "Phenotype" || entity == "OMIM")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)
                  where ",entity,".",entityField ," = ",id,"
                RETURN OMIM.name,OMIM.id,Phenotype.id,Phenotype.aspect")
        df <- cypher(graph,query)
      }
      
      if(entity != "Phenotype" &&
         entity != "OMIM")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)
                  where ",entity,".",entityField ," = ",id,"
                RETURN Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Gene.summary")
        df <- cypher(graph,query)
        }
     
      
      if(entity == "Gene")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Gene:Gene)
                  where ",entity,".",entityField ," = ",id,"
                RETURN Gene.name,Gene.id,Gene.ensembleID,Gene.Cytolocation,Gene.summary")
        df <- cypher(graph,query)
        }
      
      
      if(entity != "OMIM")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (OMIM:OMIM)
                 where ",entity,".",entityField ," = ",id,"
                 RETURN OMIM.name,OMIM.id,OMIM.Prefix")
        df <- cypher(graph,query)
      }
      
      
      if(entity == "Phenotype")
      if(is.null(nrow(df))){
        query <-
          paste0("MATCH (Phenotype:Phenotype)
                  where ",entity,".",entityField ," = ",id,"
                RETURN Phenotype.id,Phenotype.aspect,Phenotype.frequency,Phenotype.dBType")
        df <- cypher(graph,query)
      }
      
      
      if(is.null(nrow(df))){
        return("No results Found")
      }
      
      
      
      #return (df)
      visNetwork(nodes, edges)
    })
    
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)