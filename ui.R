ui <- fluidPage(
  # These are the manual styles
  htmlOutput('tags'),
  titlePanel("Testing timeline visualisation using timevis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('person', "Choose person", choices = person),
      htmlOutput("legend"),
      width = 3
    ),
    
    mainPanel(
      timevisOutput("mytime", width = "100%", height = "auto"),
      span(textOutput("headline"), style="font-weight:bold;font-size:20px"),
      textOutput("desc"),
      textOutput("classN")
    )
  )
)
