
# Capstone Project - WordPredictor

library(shiny)
library(sqldf)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

filter <- function(sampleText, filterWords){
  filteredDoc <- vector(length = length(sampleText))
  j = 1
  for (i in sampleText) {
    words <- unlist(strsplit(i, split = " "))
    filteredWords <- gsub(pattern = paste(paste(paste0('^', filterWords, '$'), collapse = "|"), collapse = "|"), x = words, replacement = "")
    filteredText <- paste(as.vector(filteredWords[filteredWords != ""]), collapse = " ")
    filteredDoc[j] <- trim(filteredText)
    j = j + 1
  }
  return(filteredDoc)
}

profanityEnglish <- c('anal','anus','arse','ass','ballsack','balls','bastard','bitch','biatch','bloody','blowjob','blowjob','bollock','bollok','boner','boob','bugger','bum','butt','buttplug','clitoris','cock','coon','crap','cunt','damn','dick','dildo','dyke','fag','feck','fellate','fellatio','felching','fuck','f u c k','fudgepacker','fudgepacker','flange','Goddamn','Goddamn','hell','homo','jerk','jizz','knobend','labia','lmao','lmfao','muff','nigger','nigga','omg','penis','piss','poop','prick','pube','pussy','queer','scrotum','sex','shit','shit','sh1t','slut','smegma','spunk','tit','tosser','turd','twat','vagina','wank','whore','wtf')

if (!exists("db/dfUni")) {
  dfUni <- read.csv("db/dfUni600k.csv")
  dfUni <- dfUni[, 2:4]
  }

if (!exists("db/dfBi")) {
  dfBi <- read.csv("db/dfBi600k.csv")
  dfBi <- dfBi[, 2:4]
  dfBi <- dfBi[(dfBi$Freq > 7),] # Pruning for speed
}

if (!exists("db/dfTri")) {
  dfTri <- read.csv("db/dfTri600k.csv")
  dfTri <- dfTri[, 2:4]
  dfTri <- dfTri[(dfTri$Freq > 6),] # Pruning for speed
}

if (!exists("db/dfFour")) {
  dfFour <- read.csv("db/dfFour600k.csv")
  dfFour <- dfFour[, 2:4]
}

if (!exists("db/dfFive")) {
  dfFive <- read.csv("db/dfFive600k.csv")
  dfFive <- dfFive[, 2:4]
}

if (!exists("dfUni")) {
  dfUni <- read.csv("dfUni600k.csv")
  dfUni <- dfUni[, 2:4]
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("WordPredictor"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       textInput(inputId="text1", label = "Enter Words (default is blank string):"),
       actionButton("goButton", "Go!")
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Predictions",
            h4("Top 3 Words:"),
            tags$hr(),
            tags$h3(textOutput("text1")),
            tags$hr(),
            tags$h4(textOutput("text2")),
            tags$hr(),
            tags$h5(textOutput("text3"))
            ),
          tabPanel("Instructions",
            tags$br(),
            p("1 - Enter a word, or a couple of words."),
            tags$hr(),
            p("2 - Hit the Go! button."),
            tags$hr(),
            p("3 - Look at the results on the right!")
          ),
        tabPanel("Source Code",
          p(""),
          p("All R code can be found in the following GitHub repository:"),
          tags$a(href="https://github.com/LoekL/DSCapstone", "https://github.com/LoekL/DSCapstone")
        )
        )
      )
   )
))

server <- shinyServer(function(input, output) {
  predictor <- reactive({
    sentence <- input$text1
    
    sentence <- gsub('[^a-zA-Z ]', '', tolower(sentence))
    sentence <- filter(sentence, profanityEnglish)
    wordVector <- unlist(strsplit(sentence, " "))
    size <- length(wordVector)
    
    if (size >= 4) {
      start <- size - 3
      stop <- size
      fourGram <- paste0(wordVector[start:stop], collapse = " ")
      threeGram <- paste0(wordVector[(start+1):stop], collapse = " ")
      twoGram <- paste0(wordVector[(start+2):stop], collapse = " ")
      oneGram <- paste0(wordVector[(start+3):stop], collapse = " ")
    } else if (size == 3) {
      threeGram <- sentence
      twoGram <- paste0(wordVector[2:size], collapse = " ")
      oneGram <- paste0(wordVector[3:size], collapse = " ")
    } else if (size == 2) {
      twoGram <- sentence
      oneGram <- paste0(wordVector[2:size], collapse = " ")
    } else {
      oneGram <- sentence
    }
    
    if (size >= 4) {
      regexFourGram <- paste0('^', fourGram, ' .*')
      regexTriGram <- paste0('^', threeGram, ' .*')
      regexBiGram <- paste0('^', twoGram, ' .*')
      regexUniGram <- paste0('^', oneGram, ' .*')
      regexFinal <- paste(paste0('^', tail(wordVector, 5), '$'), collapse = "|")
      subGram <- paste0()
      df5 <- dfFive[grepl(regexFourGram, as.vector(dfFive[,1])),]
      df5 <- head(df5[order(-df5$Freq),], 10)
      df4 <- dfFour[grepl(regexTriGram, as.vector(dfFour[,1])),]
      df4Sub <- df4[order(-df4$Freq),]
      df4 <- rbind(head(df4Sub, 10), dfFour[(dfFour$ngram == fourGram),])
      df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
      df3Sub <- df3[order(-df3$Freq),]
      df3 <- rbind(head(df3Sub, 10), dfTri[(dfTri$ngram == threeGram),])
      df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
      df2Sub <- df2[order(-df2$Freq),]
      df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
      df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
      df1 <- df1[order(-df1$Freq),]
      df0 <- dfUni[1:5,]
      df <- rbind(df5, df4, df3, df2, df1, df0)
    } else if (size == 3) {
      regexTriGram <- paste0('^', threeGram, '.*')
      regexBiGram <- paste0('^', twoGram, '.*')
      regexUniGram <- paste0('^', oneGram, '.*')
      regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
      df4 <- dfFour[grepl(regexTriGram, as.vector(dfFour[,1])),]
      df4 <- head(df4[order(-df4$Freq),], 10)
      df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
      df3Sub <- df3[order(-df3$Freq),]
      df3 <- rbind(head(df3Sub, 10), dfTri[(dfTri$ngram == threeGram),])
      df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
      df2Sub <- df2[order(-df2$Freq),]
      df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
      df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
      df1 <- df1[order(-df1$Freq),]
      df0 <- dfUni[1:5,]
      df <- rbind(df4, df3, df2, df1, df0)
    } else if (size == 2) {
      regexBiGram <- paste0('^', twoGram, '.*')
      regexUniGram <- paste0('^', oneGram, '.*')
      regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
      df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
      df3 <- head(df3[order(-df3$Freq),], 10)
      df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
      df2Sub <- df2[order(-df2$Freq),]
      df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
      df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
      df1 <- df1[order(-df1$Freq),]
      df0 <- dfUni[1:5,]
      df <- rbind(df3, df2, df1, df0)
    } else {
      regexUniGram <- paste0('^', oneGram, '.*')
      regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
      df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
      df2 <- head(df2[order(-df2$Freq),], 10)
      df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
      df1 <- df1[order(-df1$Freq),]
      df0 <- dfUni[1:5,]
      df <- rbind(df2, df1, df0)
    }
    
    df[,1] <- as.character(df[,1])
    
    for (i in 1:dim(df)[1]) {
      df$recommendation[i] <- tail(unlist(strsplit(df[i,1], " ")), 1)
      df$subgram[i] <- paste(unlist(strsplit(df[i,1], " "))[1:(df[i,3] - 1)], collapse = " ")
    }
    
    df <- sqldf("
    SELECT DISTINCT 
      a.*,
      b.Freq AS subgramFreq
    FROM df a
    LEFT JOIN df b
    ON a.subgram = b.ngram
    WHERE subgramFreq IS NOT NULL
    ")
    
    lambda <- 0.4
    df$score <- (lambda^(5 - df$order)) * (df$Freq / df$subgramFreq)
    df[(df$order == 1), 'score'] <- 0
    df <- df[order(-df$score),]
    df <- df[!grepl(regexFinal, as.vector(df[,1])),]
    df <- df[!grepl(regexFinal, as.vector(df[,4])),]
    topFive <- head(unique(df$recommendation), 5)
    predictor <- topFive
    })
  
  output$text1 <- renderText({
    input$goButton
    isolate(paste0("1 - ", predictor()[1]))})
  
  output$text2 <- renderText({
    input$goButton
    isolate(paste0("2 - ", predictor()[2]))})
  
  output$text3 <- renderText({
    input$goButton
    isolate(paste0("3 - ", predictor()[3]))})
})

# Run the application 
shinyApp(ui = ui, server = server)
