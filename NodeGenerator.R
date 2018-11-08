source("packageInstaller.R")

library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)
library(visNetwork)
library(DT)
library(shinythemes)
library(shinyjs)


config <- fromJSON(file="config.json")
credentials <- fromJSON(file="credentials.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))
graph = startGraph(credentials$graphUrl, username=credentials$username, password=credentials$password)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  # App title ----
  headerPanel("Query Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("entityLabel","Entity Type", config$searchLabels),
      uiOutput("entityFields"),
      selectizeInput('suggestionsInput', 'Select search input', choices = NULL, multiple = FALSE, selected = NULL),
      uiOutput("suggestions"),
      uiOutput("temp"),
      hidden(
        p(
          id = "hiddenCredElement",
          paste0('{"url":"',credentials$graphUrl,'","username":"',credentials$username,'","password":"',credentials$password,'"','}')
        )
      ),
      actionButton("createSearchBtn", "Search", class = "btn-primary"),
      width = 4
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      tabsetPanel(
        tabPanel("Plot", visNetworkOutput("plot")),
        tabPanel("Table", dataTableOutput("results")),
        selected = "Table"
      )
    )
  )
)

id <- NULL
searchType <- NULL
entity <- "OMIM"
entityField <- NULL
df <- data.frame()

server <- function(input, output, session) {

  output$entityFields <- renderUI({
    entity <<- input$entityLabel
    selectInput("entityField", strong(paste0('Select a ', entity, ' property')), config[[entity]]$searchProperties)
  })

  output$suggestions <- renderUI({
    entity <<- input$entityLabel
    entityField <<- input$entityField
    updateSelectizeInput(session, 'suggestionsInput', choices = NULL, selected = NULL, options = list(
      options = list(),
      create = FALSE,
      valueField = 'row',
      labelField = 'row',
      searchField = 'row',
      placeholder = 'Type for suggestions',
      render = I("{
                  option: function(item, escape) {
                    return '<div>' + item.row[0] + '</div>';
                  }
      }"),
      load = I("function(query, callback) {
                  if (!query.length) return callback();
                  var cred = JSON.parse($('#hiddenCredElement').html());
                  var authorisation = 'Basic ' + btoa(cred.username + ':' + cred.password);
                  var entity = $('#entityLabel').val();
                  var entityField = $('#entityField').val();
                  var neoQuery = 'MATCH (n:'+entity+') WHERE toString(n.'+entityField+') CONTAINS '+JSON.stringify(query)+' RETURN n.'+entityField+' AS field LIMIT 100';

                  $.ajaxSetup({
                    headers: {
                      'Authorization': authorisation
                    }
                  });

                  $.ajax({
                    url: cred.url+'transaction/commit',
                    type: 'POST',
                    data: JSON.stringify({ 'statements': [{ 'statement': neoQuery }] }),
                    contentType: 'application/json',
                    accept: 'application/json; charset=UTF-8' 
                  }).done(function (data) {
                    callback(data.results[0].data)
                  });
      }")
    ))
    return(NULL)
  })

  observeEvent(input$createSearchBtn, {
    output$temp <- renderUI({
      
      id <<- input$suggestionsInput
      if(is.null(id) || id==""){
        return(NULL)
      }
      entity <<- input$entityLabel
      entityField <<- input$entityField
      df <<- data.frame()
      if(!((entity == "Gene" || entity == "OMIM") && entityField == "id")){
        id <<- paste0("'", id, "'")
      }
      
      
      query <-
        paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)-[r:locateTranscript]-(Transcript:Transcript)-[q:locateExon]-(Exon:Exon)
               where ",entity,".",entityField ," = ",id," 
               RETURN OMIM.name,OMIM.id,Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand,Phenotype.id,Phenotype.aspect, Transcript.id, Transcript.name, Transcript.start, Transcript.end, Exon.id, Exon.start, Exon.end,Exon.exonNumber")
      
      
      df <<- cypher(graph,query)
      
      if( entity != "Phenotype")
        if(is.null(nrow(df))){
          query <-
            
            paste0("MATCH (OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)-[r:locateTranscript]-(Transcript:Transcript)-[q:locateExon]-(Exon:Exon)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN OMIM.name,OMIM.id,Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand, Transcript.id, Transcript.name, Transcript.start, Transcript.end, Exon.id, Exon.start, Exon.end,Exon.exonNumber")
          df <<- cypher(graph,query)
        }
      
      
      if(entity != "GeneBody")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN OMIM.name,OMIM.id,Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation,Phenotype.id,Phenotype.aspect  ")
          df <<- cypher(graph,query)
        }
      
      if(entity != "Phenotype")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (OMIM:OMIM)-[t:GeneToOMIM]-(Gene:Gene)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN OMIM.name,OMIM.id,Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation  ")
          df <<- cypher(graph,query)
        }
      
      if(entity != "Gene")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]-(OMIM:OMIM)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN OMIM.name,OMIM.id,Phenotype.id,Phenotype.aspect")
          df <<- cypher(graph,query)
        }
      
      if(entity != "Phenotype" &&
         entity != "OMIM")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (Gene:Gene)-[s:GeneLocates]-(GeneBody:GeneBody)-[r:locateTranscript]-(Transcript:Transcript)-[q:locateExon]-(Exon:Exon)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation,GeneBody.start,GeneBody.end,GeneBody.strand, Transcript.id, Transcript.name, Transcript.start, Transcript.end, Exon.id, Exon.start, Exon.end,Exon.exonNumber ")
          df <<- cypher(graph,query)
        }
      
      
      if(entity == "Gene")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (Gene:Gene)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN Gene.name,Gene.symbol,Gene.id,Gene.ensembleID,Gene.Cytolocation  ")
          df <<- cypher(graph,query)
        }
      
      
      if(entity == "OMIM")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (OMIM:OMIM)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN OMIM.name,OMIM.id,OMIM.Prefix")
          df <<- cypher(graph,query)
        }
      
      
      if(entity == "Phenotype")
        if(is.null(nrow(df))){
          query <-
            paste0("MATCH (Phenotype:Phenotype)
                   where ",entity,".",entityField ," = ",id,"
                   RETURN Phenotype.id,Phenotype.aspect,Phenotype.frequency,Phenotype.dBType")
          df <<- cypher(graph,query)
        }
      
      
      if(is.null(nrow(df))){
        return("No results Found")
      }
      
      
      output$results <- renderDataTable({
        
        if(is.null(df) || is.null(nrow(df)) || nrow(df)==0){
          return(NULL)
        }
        
        
        output$plot <- renderVisNetwork({
          
          omim <- data.frame()
          gene <- data.frame()
          phenotype <- data.frame()
          
          if(!is.null(df$OMIM.id)){
            omim <- data.frame(id=df$OMIM.id,label=df$OMIM.name,group="OMIM")
          }
          
          if(!is.null(df$Gene.id)){
            gene <- data.frame(id=df$Gene.id,label=df$Gene.symbol,group="Gene")
          }
          
          if(!is.null(df$Phenotype.id)){
            phenotype <- data.frame(id=df$Phenotype.id,label=df$Phenotype.id,group="phenotype")
          }
          
          
          nodes <- rbind(unique(omim),unique(gene),unique(phenotype))
          
          
          query <- paste0( "MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]->(OMIM:OMIM)<-[t:GeneToOMIM]
                           -(Gene:Gene) where ",entity,".",entityField ," = ",id," 
                           RETURN Gene.id as from, OMIM.id as to, Type(t) As label 
                           UNION 
                           MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]->(OMIM:OMIM)<-[t:GeneToOMIM]
                           -(Gene:Gene) where ",entity,".",entityField ," = ",id," 
                           RETURN Phenotype.id as from, OMIM.id as to, Type(u) As label" )
          edges <- cypher(graph,query)

          if(entity != "Phenotype" && is.null(edges)){
            query <- paste0   ( "MATCH (OMIM:OMIM)<-[t:GeneToOMIM]
                                  -(Gene:Gene)-[s:GeneLocates]->(GeneBody:GeneBody) where ",entity,".",entityField ," = ",id," 
                                  RETURN Gene.id as from, OMIM.id as to, Type(t) As label" )
            edges <- cypher(graph,query)
          }
          if(entity != "Gene" && is.null(edges)){
      
            query <- paste0   ( " MATCH (Phenotype:Phenotype)-[u:PhenotypeCauses]->(OMIM:OMIM) where ",entity,".",entityField ," = ",id," 
                                  RETURN Phenotype.id as from, OMIM.id as to, Type(u) As label " )
            edges <- cypher(graph,query)
            
          }
          
          visNetwork(nodes, edges, height = "700px", width = "200%") %>% 
            visOptions(highlightNearest = TRUE) %>%
            visLayout(randomSeed = 123) %>% visEdges(arrows = 'to',hoverWidth = 4, length=400, color = list(color = "red", highlight = "yellow"),
                                                     font =list(color= '#393434', size = 20))
        })
        
        
        datatable(df,filter = 'top', options = list(scrollX =TRUE))
      })
      return(NULL)
    })
    
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)