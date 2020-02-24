#-------------------------------------------------
#  title: Spotify Popularity Predictor and Explorer
#  authors: Adarsh Salapaka, Harsh Tandon, Wenping (Fiona) Du, Xiyu Wu, Xiaoyu Guo 
#--------------------------------------------------
  
server = function(input, output, session) {
  
  
  #read inputs for tab1 containging plots 
  output$distPlot <- renderPlot({
    minPopularity <- input$popularity[1]
    maxPopularity <- input$popularity[2]
    
    
    # Apply filters
    subData <- data %>%
      filter(
        popularity >= minPopularity,
        popularity <= maxPopularity
      )
    
    # Optional: filter by genre
    if (input$genre != "All") {
      subData <- subData %>% filter(genre == input$genre)
    }
    # Optional: filter by artist name
    if (!is.null(input$artistName) && input$artistName != "") {
      subData <- data %>% filter(grepl(input$artistName,artist_name))
    }
    
    g1 = ggplot(subData, aes(x = acousticness))  + 
      labs(subtitle = "Acoustiness for Songs for Popularity between selected values") + 
      geom_histogram(aes(y = ..density..), breaks = seq(-1,1, by=0.02), color = "white", fill = "#2896d7") +
      geom_density() +
      xlim(-0.2,1.2) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18,face="bold"),
            plot.subtitle = element_text(size=18,face="bold"))
    
    g2 = ggplot(subData, aes(x = valence, y = danceability)) +
      geom_point(aes(color = "white", fill = "#d05126")) +
      labs(subtitle = "Danceability vs valence plot") + 
      geom_smooth(method='lm') +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18,face="bold"),
            plot.subtitle = element_text(size=18,face="bold"),
            legend.position = "none")
    
    grid.arrange(g1,g2, ncol = 2)
    
  })
  #read inputs for regression  
  output$summary <- renderText({
    
    #store inputs as numeric
    inputValues = as.numeric(c(input$acousticness,
                               input$danceability,
                               input$duration_ms,
                               input$instrumentalness,
                               input$key,
                               input$liveness,
                               input$loudness,
                               input$mode,
                               input$speechiness,
                               input$tempo,
                               input$time_signature,
                               input$valence))
    inputs = as.data.frame(rbind(inputValues))
    colnames(inputs) = c("acousticness","danceability","duration_ms","instrumentalness","key","liveness","loudness",
                         "mode","speechiness","tempo","time_signature","valence")
    
    #run prediction on input values
    prediction = predict(LR,newdata = inputs)
    
    #print the output
    paste("<font size=\"4\">",
          "Popularity of this song with these features is: ", 
          "<font color=\"#FF0000\"><b>",round(prediction,2), 
          "</b></font></font>")
  })
  
  #lets work with tables
  output$table = renderDataTable({
    
    subDataTable <- data %>%
      filter(
        popularity >= input$popularity1[1],
        popularity <= input$popularity1[2]
      )
    
    subDataTable = data[,-4]
    if (input$artist_name1 != "All") {
      subDataTable <- subDataTable[subDataTable$artist_name == input$artist_name1,]
    }
    if (input$genre1 != "All") {
      subDataTable <- subDataTable[subDataTable$genre == input$genre1,]
    }
    subDataTable
    
  })
  
}
