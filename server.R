server = function(input, output) {
  
  reactiveDate <- reactive({
    covid_data %>% 
      filter(., date == input$date, na.rm = TRUE) 
    
  })
  
  output$map <- renderGvis({
    gvisGeoChart(reactiveDate(), "state", 
                 input$selected,
                 options=list(region="US", displayMode="regions",
                              resolution="provinces",
                              width = "900", height = "400",
                              tooltip = "{textStyle: {color: '#FF0000'}, showColorCode: true}",
                              legend = "{textStyle: {color: 'black', fontSize: 12}}",
                              colorAxis="{colors:['#FFFFFF', '#FF0000']}",
                              defaultColor = '#f5f5f5'
                              ))

  })
  
  output$no1 <- renderUI({
    a<-reactiveDate() %>% 
      select(., state, input$selected) %>% 
      group_by(state) %>% 
      arrange(desc(get(input$selected)))
    no1 <- unique(a$state)[1:3][1]
    HTML(paste(
      p(HTML("1. "), no1)
    )
    )
    
  })

  output$no2 <- renderUI({
    b<-reactiveDate() %>%
      select(., state, input$selected) %>%
      group_by(state) %>%
      arrange(desc(get(input$selected)))
  no2 <- unique(b$state)[1:3][2]
  HTML(paste(
    p(HTML("2. "), no2)
  )
  )

  })
  
  output$no3 <- renderUI({
    c<-reactiveDate() %>%
      select(., state, input$selected) %>%
      group_by(state) %>%
      arrange(desc(get(input$selected)))
    no3 <- unique(c$state)[1:3][3]
    HTML(paste(
      p(HTML("3. "), no3)
    )
    )

  })

  output$state <- renderPlotly({
    plot_ly(covid_data, x = ~date, y = ~get(input$incidence)) %>%
      filter(state %in% input$states) %>% 
      add_lines(color = ~state) %>% 
      layout(xaxis = list(title ="date"),
             yaxis = list(title = input$incidence))
      
  })
    
  reactiveMaster <- reactive({
      covid_data %>% 
        filter(., date == input$h) 
    
  })
  
  output$plot <- renderPlotly({
    p<- ggplot(reactiveMaster(), aes_string(x=input$condition, y='hospitalizedPerMil', na.rm = TRUE)) +
      geom_point(aes(color = state), alpha=0.5, size=2) +
      stat_smooth(method=lm, linetype="dash",
                  color="black", se = FALSE, show.legend = TRUE) 

  })
  
  output$max <- renderValueBox({
    max_value <- max(reactiveMaster()[, input$condition])
    c <- covid_data %>%
      select(., state, COPD) %>%
      group_by(state) %>%
      arrange(desc(COPD))
    
    max_state <- c$state[1]
    
    valueBox(value = p(max_value, "%", style = "font-size: 125%"), 
             subtitle = p(max_state, "(Unhealthiest state)"), icon = icon("frown"),
             color = "red")
             
  })

  output$min <- renderValueBox({
    min_value <- min(reactiveMaster()[, input$condition])
    d <- covid_data %>%
      select(., state, COPD) %>%
      group_by(state) %>%
      arrange((COPD))

    min_state <- d$state[1]

    valueBox(value = p(min_value, "%", style = "font-size: 125%"), 
             subtitle = p(min_state, "(Healthiest state)"), icon = icon("smile")
    )

  })
  
  output$avg <- renderValueBox({
    # avg_value <- mean(reactiveMaster()[, input$condition], na.rm = TRUE)
    avg_value <- reactiveMaster() %>% 
      select(., input$condition) %>% 
      summarise(., mean = mean(get(input$condition), na.rm = TRUE))
    
    valueBox(value = p(avg_value, "%", style = "font-size: 125%"), 
             subtitle = "Average US rate", icon = icon("bar-chart"),
             color = "yellow"
    )
    
  })
  
  output$plot2 <- renderPlotly({
    ggplot(reactiveMaster(), aes_string(x=input$condition, y='deathPerMil', na.rm = TRUE)) +
      geom_point(aes(color = state), alpha=0.5, size=2) +
      stat_smooth(method=lm, linetype="dash",
                  color="black", se = FALSE) 

  })
  
  output$cor <- renderPlot({
    correlation <- covid_data %>%
      group_by(., state) %>%
      filter(., date == "2020-11-25") %>%
      select(., COPD, Hypertension, Obesity, Flu, Cancer, Diabetes, hospitalizedPerMil, deathPerMil)

    correlation<- correlation[, c(2,3,4,5,6,7,8,9)]
    res <- cor(correlation)
    round(res,2)
    corrplot(re, type = "upper", order = "hclust",
             tl.col = "black", tl.srt = 45)
    # d <- na.omit(covid_data[, c(2,3,4,5,6,7,11,13)])
    # res <- cor(d)
    # round(res,2)
    # library(corrplot)
    # corrplot(res, type = "upper", order = "hclust",
    #          tl.col = "black", tl.srt = 45)

  })
  
  output$pie <- renderPlotly({
    colors <- c('rgb(200,34,56)', 'rgb(255,215,0)', 'rgb(0,0,0)', 'rgb(114,147,203)','rgb(255,127,80)')
    plot_ly(death_data, labels=~ethnicity,values=~death_percentage, 
            # marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
            marker = list(colors = colors), type = "pie",
            textposition = ifelse(death_data$death_percentage<4,"outside","inside"),textinfo = 'text',
            hoverinfo = 'text',source = "subset",
            text=~paste(sub(" ","<br>",ethnicity),":","<br>",paste0(death_percentage,"%") ),
            insidetextfont = list(color = '#FFFFFF')) %>%
      
      layout(title = 'US Covid19 mortality based on ethnicity', showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
    
  })
  
  output$ethnicity <- renderPlotly({
    plot_ly(adjusted_death, x = ~ethnicity1, y =~death_permil) %>% 
      add_trace(type = 'bar',
                marker = list(color = c('rgb(114,147,203)',
                                        'rgb(0,0,0)',
                                        'rgb(200,34,56)', 
                                        'rgb(255,215,0)',
                                        'rgb(255,127,80)',
                                        'silver')))%>% 
      layout(title = 'National demographic breakdown of Covid mortality',
             xaxis = list(title = "Ethnicity"),
                          yaxis = list(title = 'Death per million'), 
                          barmode = 'group', showlegend = FALSE)
   
  })
  
  output$ethnicity2 <- renderPlotly({
    df <- death %>% 
      filter(., state == input$state) %>% 
      
      gather(key = Key, value = Value, white_death_per_100K:asian_death_per_100K)
    
    ggplot(df, aes(y= Value, x=Key)) + 
      geom_bar(stat="identity", fill = c("blue","black", "red", "yellow"), na.rm = TRUE) +
      ggtitle("Statewide demographic breakdown of Covid death") +
      xlab(input$state) + ylab("Death per 100K")

  })
  
  # output$demcor <- renderPlot({
  #   c <- na.omit(death[, c(6,7,8,9,10)])
  #   c <- cor(c)
  #   round(c,2)
  #   library(corrplot)
  #   corrplot(c, type = "upper", order = "hclust", 
  #            tl.col = "black", tl.srt = 45,
  #            addCoef.col="black", addCoefasPercent = TRUE,
  #            p.mat = 1-abs(c), sig.level=0.50, insig = "blank")
  # 
  # })
  
  output$table <- DT::renderDataTable({
    DT::datatable(covid_data, rownames=FALSE) %>% 
      DT::formatStyle("state",background="#E88E8E",fontWeight='bold')
    
  })
  
  output$demo <- DT::renderDataTable({
    DT::datatable(death, rownames=FALSE) %>% 
      DT::formatStyle("state",background="#E88E8E",fontWeight='bold')
    
  })
  
}
    


 