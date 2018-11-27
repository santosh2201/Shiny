shinyUI(
  navbarPage(title=strong("GraphDB"), windowTitle = "GraphDB - connecting variants to phenotypes through genes and diseases",
             fluid=TRUE, footer=includeHTML("tools/footer.html"),id="nav",
             source('Server.R',local = TRUE)$value
             
  )
)



