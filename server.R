server <- function(input, output) {
  ### Data prep ----
  
  ev_df <- reactive({
    print(paste0('events: ', lubridate::now() - log_start))
    events <- copy(events) %>%
      .[person == input$person] %>%
      .[, person := NULL]
    
    if (ncol(events) < 8) {
      events <- empty
    }
    return(events)
  })
  
  per_df <- reactive({
    print(paste0('periods: ', lubridate::now() - log_start))
    periods <- copy(periods) %>%
      .[person == input$person] %>%
      .[, person := NULL]
    
    if (ncol(periods) < 8) {
      periods <- empty
    }
    return(periods)
  })
  
  all_data <- reactive({
    print(paste0('combine data: ', lubridate::now() - log_start))
    do.call("rbind", list(ev_df(), per_df()))
  })
  
  # Create group IDs -------
  groups <- reactive({
    print(paste0('generate groups: ', lubridate::now() - log_start))
    data.frame(content = unique(all_data()[[group_col]]),
               id = as.numeric(as.factor(unique(all_data()[[group_col]]))),
               title = unique(all_data()[[group_col]]))
  })
  
  prep_data <- reactive({
    print(paste0('prep final data: ', lubridate::now() - log_start))
    prep_data <- copy(all_data())  %>%
      merge(., groups(), how = 'left', by.x = group_col, by.y = "content") %>%
      .[, .(id = rownames(.),
            content = content,
            start = start,
            group = id,
            subgroup = subgroups,
            end = end,
            headline = headline,
            desc = desc,
            className = className)]
    return(prep_data)
  })
  
  output$mytime <- renderTimevis(timevis::timevis(prep_data(), groups = groups(), fit = TRUE, 
                                                  options = list(stack = FALSE)))
  
  output$tags <- renderUI({
    tags$head(tags$style(HTML(CSS_colors)))
  })
  output$legend <- renderUI({ HTML(paste0(grouped_html$group_html,
                                          collapse = " ")) })
  
  output$headline <- renderText({
    req(input$mytime_selected)
    as.character(copy(prep_data())[id == input$mytime_selected]$headline)
  })
  output$desc <- renderText({
    req(input$mytime_selected)
    as.character(copy(prep_data())[id == input$mytime_selected]$desc)
  })
  output$classN <- renderText({
    req(input$mytime_selected)
    as.character(copy(prep_data())[id == input$mytime_selected]$className)
  })
}
