#-------------------------------------------------
#  title: Spotify Popularity Predictor and Explorer
#  authors: Adarsh Salapaka, Harsh Tandon, Wenping (Fiona) Du, Xiyu Wu, Xiaoyu Guo 
#--------------------------------------------------
# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

ui = fluidPage(
  
  titlePanel("Spotify Feature Explorer"),
  
  tabsetPanel(type = "tabs",
              #tab1 = Plots            
              tabPanel("Just some plots",
                       
                       plotOutput("distPlot", height = 600),
                       
                       hr(), #insert a horizontal row
                       
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h4("Select Popularity"),
                                  sliderInput(inputId = "popularity",
                                              label = "Popularity between selected values:",
                                              min = 0,
                                              max = 100,
                                              value = c(40,50))
                                )),
                         column(6,
                                wellPanel(
                                  selectInput(inputId = "genre",
                                              label = "Genre",
                                              choices = c("All",levels(data$genre))),
                                  textInput("artistName", "Artist name contains (e.g., Britney)")
                                ))
                       )
              ),
              #tab2 = Regression Summary
              
              tabPanel("Lets try Regression", 
                       
                       h4("Enter data to test against our trained model"),
                       
                       fluidRow(
                         column(4,
                                wellPanel(
                                  
                                  sliderInput(inputId = "duration_ms",
                                              label = "Durantion in milli Seconds:",
                                              min = 15000,
                                              max = 1400000,
                                              value = 15000),
                                  sliderInput(inputId = "acousticness",
                                              label = "Acousticness:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  sliderInput(inputId = "danceability",
                                              label = "Danceability:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  sliderInput(inputId = "instrumentalness",
                                              label = "Instrumentalness:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  sliderInput(inputId = "liveness",
                                              label = "Liveness:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  selectInput(inputId = "key",
                                              label = "Key:",
                                              choices = c("A" = 1,
                                                          "A#" = 2,
                                                          "B" = 3,
                                                          "C" = 4,
                                                          "C#" = 5,
                                                          "D" = 6,
                                                          "D#" = 7,
                                                          "E" = 8,
                                                          "F" = 9,
                                                          "F#" = 10,
                                                          "G" = 11,
                                                          "G#" = 12))
                                )
                                
                         ),
                         
                         column(4,
                                
                                wellPanel(htmlOutput("summary"))
                         ),
                         column(4,
                                wellPanel(
                                  
                                  sliderInput(inputId = "loudness",
                                              label = "Loudness:",
                                              min = -52,
                                              max = 2,
                                              value = -52),
                                  sliderInput(inputId = "speechiness",
                                              label = "Speechiness:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  sliderInput(inputId = "tempo",
                                              label = "Tempo:",
                                              min = 20,
                                              max = 240,
                                              value = 20),
                                  sliderInput(inputId = "valence",
                                              label = "Valence:",
                                              min = 0,
                                              max = 1,
                                              value = 0),
                                  selectInput(inputId = "mode",
                                              label = "Mode:",
                                              choices = c("Major" = 0, 
                                                          "Minor" = 1)),
                                  selectInput(inputId = "time_signature",
                                              label = "Time Signature:",
                                              choices = c(levels(data$time_signature)))
                                )
                         )
                       )
              ),
              #tab3 = Tables
              
              tabPanel("Table",
                       
                       fluidRow(
                         column(4,
                                wellPanel(
                                  selectInput(inputId = "artist_name1",
                                              label = "Artists:",
                                              choices = c("All",levels(data$artist_name)),
                                              selected = 1)
                                )),
                         column(4,
                                wellPanel(
                                  selectInput(inputId = "genre1",
                                              label = "Genre:",
                                              choices = c("All",levels(data$genre)))
                                )),
                         column(4,
                                wellPanel(
                                  sliderInput(inputId = "popularity1",
                                              label = "Popularity between selected values:",
                                              min = 0,
                                              max = 100,
                                              value = c(0,100))
                                ))
                       ),
                       
                       hr(), #insert a horizontal row
                       
                       DT::dataTableOutput("table")
                       
              )
  )
)