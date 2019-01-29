#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for data upload app ----

library(rtweet)
library(data.table)
library(shinydashboard)
library(shiny)
library(DT)
library(cleanNLP)
library(tm)
library(spacyr)
library(textclean)
library(stringr)
library(dplyr)
library(rtweet)
library(tidytext)
library(utf8)
library(wordcloud)

####################
# Helper functions
####################
library(jsonlite)
library(httr)
library(ggplot2)
library(gplots)
source("Helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "Twitter ADR tool"),
  dashboardSidebar(
    # Input: Select a file ----
    tags$hr(),
    
    actionButton("do", "connect to mongodb"),
    
    fileInput(
      "file1",
      "Choose twitter json File",
      multiple = TRUE,
      accept = c("json/json",
                 "json/json files",
                 ".json")
    ),
    
    
    fileInput(
      "file2",
      "Choose symptome file",
      multiple = TRUE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    fileInput(
      "file3",
      "Choose drug file",
      multiple = TRUE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    
    
    
    # Horizontal line ----
    tags$hr(),
    
    
    checkboxInput("symptomes", "generate symptomes", value = FALSE),
    
    
    # Input: Select number of rows to display ----
    radioButtons(
      "disp",
      "Display",
      choices = c(Head = "head",
                  All = "all"),
      selected = "head"
    )
  ),
  
  
  # App title ----
  dashboardBody(
    navbarPage(
      "Views",
      tabPanel("Messages",
               
               fluidRow(
                 column(
                   6,
                   box(
                     title = "messages and found drugs / symptomes",
                     status = "primary",
                     height =
                       "750",
                     width = "600",
                     solidHeader = T,
                     column(
                       width = 12,
                       DT::dataTableOutput("trace_table"),
                       style = "height:630px; overflow-y: scroll;"
                     )
                   )
                 ),
                 column(6, plotOutput("heatmap")),
                 column(6, tableOutput("wordlist"))
               )),
      tabPanel(
        "Frequent used words",
        sliderInput(
          "wordOCc",
          "word occurence",
          min = 1,
          max = 100,
          value = 2
        ),
        plotOutput("wordplot" , width = "100%")
        
        
      ),
      tabPanel(
        "Inverse Document Trm Frequency",
        sliderInput(
          "wordcloud_freq",
          "word occurence",
          min = 1,
          max = 100,
          value = 2
        ),
        plotOutput("wordcloud" , width = "100%")
        
        
      ),
      tabPanel(
        "Drug / symptom frequency",
        DT::dataTableOutput("aggr_table"),
        style = "height:630px; overflow-y: scroll;"
      )
      
    )
  )
)

options(shiny.maxRequestSize = 30 * 1024 ^ 3)
# Define server logic to read selected file ----
server <- function(input, output) {
  
  isDBConnect <- reactiveVal(FALSE)
  
  mongo_r <- reactive({
    print("mongodb")
    collection <- connectMongoDB("twitter","twitterdb")
    rt <- collection$find()
  })
  
  
  file_r <- reactive({
    req(input$file1)
    print("file")
    rt <- parse_stream(input$file1$datapath)
    rt <- rt %>% dplyr::filter(lang == "en")
  })
  
  rs_r <- reactive({
    
    #rt <- file_r()
    if(isTruthy(isDBConnect()))
    {
      rt <- mongo_r()
    }
    else
    {
      
      rt <- file_r()
    }
    
  })
  
    
    
  drug_r <- reactive({
    #req(input$file3)
    drugfile <-
      ifelse(is.null(input$file3$datapath), " ", input$file3$datapath)
    fread(
      file = ifelse(file.exists(drugfile), drugfile, "daten/drugnames.csv"),
      sep = ",",
      header = TRUE
    )
    
  })
  
  symptom_r <- reactive({
    #req(input$file2)
    symptomfile <-
      ifelse(is.null(input$file2$datapath), " ", input$file2$datapath)
    fread(
      file = ifelse(file.exists(symptomfile), symptomfile, "daten/adrs.csv"),
      sep = ",",
      header = TRUE
    )
    
  })
  
  
  
  
  
  ps <- reactive({
    
    rt <- rs_r()
    
    
    rt$text <- resolveInternetSlang(rt$text)
    
    tbl_drug <- drug_r()
    tbl_symptomes <- symptom_r()
    
    tbl_slangterms <-
      fread(file = "daten/twitter_slang_terms.csv",
            sep = ",",
            header = TRUE)
    
    ############################
    # enrich symptom list
    ############################
    library(dplyr)
    if (input$symptomes == TRUE)
    {
      tbl_symptomes <- GetsymptomesFromWordNet(tbl_symptomes$symptome)
    }
    
    tbl_symptomes$synonymes <-
      unlist(tbl_symptomes$synonymes,
             recursive = TRUE,
             use.names = TRUE)
    tbl_symptomes$unique_synonymes <-
      tbl_symptomes %>% dplyr::distinct(synonymes) %>% dplyr::pull()
    write.table(tbl_symptomes$unique_synonymes, file = "collected_symptomes.csv")
    
    
    if ('extended_tweet.full_text' %in% names(rt))
    {
      print('attribute extended_tweet.full_text exists')
      
    }
    else
    {
      print('attribute text exists')
    }
    
    
    
    ################################
    # find ratio between advertise
    ################################
    
    rt$isAdvertise <-
      str_detect(rt$text, "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
    s <-
      rt %>% dplyr::group_by(isAdvertise) %>% dplyr::count(isAdvertise)
    
    
    
    # remove advertising
    noadvertise <-
      rt %>% dplyr::select(isAdvertise, text) %>% dplyr::filter(isAdvertise == FALSE)
    
    
    outcome <-
      TagDrugAndsymptomes(rt$text, tbl_symptomes, tbl_drug[1:100], tbl_slangterms)
    #outcome$advertising_ratio <- s
    
    
  })
  
  
  
  ################################
  # find ratio between advertise
  ################################
  aggr_data_text_r <- reactive({
    rt <- rs_r()
    rt$text <- resolveInternetSlang(rt$text)
    df <- ps()
    outcome <- AggregateDrugsAndsymptomesWithText(rt$text, df)
    
  })
  
  ################################
  # find ratio between advertise
  ################################
  aggr_data_r <- reactive({
    df <- ps()
    outcome <- AggregateDrugsAndsymptomes(df)
    
  })
  
  ################################
  # find ratio between advertise
  ################################
  corp_preprocessing_r <- reactive({
    rt <- ps()
    corpus <- CorpusPreProcessing(rt$lemma)
    
    res <- corpus$text
  })
  
    
    
  ################################
  # find ratio between advertise
  ################################
  tfidf_matrix_r <- reactive({
    corpus <- corp_preprocessing_r()
    
    
    #dtm <- DocumentTermMatrix(Corpus(VectorSource(as.vector(corpus))))
    #tf <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    
    
    dtm <-
      DocumentTermMatrix(Corpus(VectorSource(as.vector(corpus))), control = list(weighting = weightTfIdf))
    dtm = removeSparseTerms(dtm, 0.95)
    tf = data.frame(sort(colSums(as.matrix(dtm)), decreasing = TRUE))
    #wordcloud(rownames(tf), tf[,1], max.words=100, colors=brewer.pal(1, "Dark2"))
    
  })
  
  
  ################################
  # find ratio between advertise 
  ################################
  dtm_matrix_r <-  reactive({
    corpus <- corp_preprocessing_r()
    
    
    dtm <-
      DocumentTermMatrix(Corpus(VectorSource(as.vector(corpus))))
    tf <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
    
    
    if (input$disp == "head") {
      res   <- head(data.frame(list(tf = tf, word = names(tf))))
    }
    else
    {
      res   <- data.frame(list(tf = tf, word = names(tf)))
      
    }
    
    
    
  })
    
    
     
  observeEvent(input$do, {
    newValue <- TRUE     # newValue <- rv$value - 1
    isDBConnect(newValue) 
    print("EVent Button clicked")
  })  
  
  output$trace_table <-
    DT::renderDataTable(
      data.table(
        text = aggr_data_text_r()$text,
        "drug(s)" = aggr_data_text_r()$drugs,
        "symptome" = aggr_data_text_r()$symptomes
      )
      ,
      server = FALSE,
      selection = "single"
    )
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  
  
  
  library(RColorBrewer)
  
  # client-side processing
  output$heatmap = renderPlot({
    s = input$trace_table_rows_selected
    if (length(s)) {
      tbl_sym <- symptom_r()
      txt <- aggr_data_text_r()$tokens[s]
      
      
      print(txt)
      
      
      c <- CorpusPreProcessing(txt)
      sd <-
        stringdist::stringdistmatrix(
          as.character(tokens(c$text)),
          as.character(tbl_sym$synonyme),
          useNames = "strings",
          method = "lv",
          weight = c(
            d = 1,
            i = 1,
            s = 1,
            t = 1
          )
        )
      sdm <- as.matrix(sd)
      
      sdm <- sdm[sdm[, 1] < 6,]
      if (is.matrix(sdm))
      {
        sdm <- sdm[, sdm[1, ] < 6]
        sdm <- sdm[order(sdm[, 1], decreasing = TRUE), , drop = FALSE]
        
      }
      
      
      
      Colors = brewer.pal(11, "Spectral")
      heatmap(
        sdm,
        col = Colors,
        density.info = "none",
        Colv = NA,
        Rowv = NA,
        trace = "none"
      )
      
      
    }
  })
  #,height = 650, width = 800)
  output$wordlist = output$view <- renderTable({
    tbl_sym <- symptom_r()
    s = input$trace_table_rows_selected
    if (length(s)) {
      txt <- aggr_data_text_r()$tokens[s]
      
      print(txt)
      c <- CorpusPreProcessing(txt)
      print(c$text)
      
      wl <-
        GetSimilarWords(as.character(tokens(c$text)), tbl_sym$synonyme)
      if (is.data.frame(wl))
      {
        print(paste("get data from wordlist", wl, sep = ":"))
        head(wl, n = 6)
      }
    }
    
  })
  
  
  
  # highlight selected rows in the scatterplot
  
  
  
  output$aggr_table <- renderDataTable({
      return(datatable(aggr_data_r()) %>% 
               formatStyle(
                 'count',
                 background = styleColorBar(aggr_data_r()$count, 'orange'),
                 backgroundSize = '100% 90%',
                 backgroundRepeat = 'no-repeat',
                 backgroundPosition = 'center'
               )
      )
    })
    
    
    
    output$wordplot <- renderPlot({
    
      
      t <- dtm_matrix_r()
      
      p <- ggplot(subset(t, tf>input$wordOCc), aes(word, tf))
      p <- p + geom_bar(stat="identity", fill="steelblue", width = 0.6)
      p <- p + geom_text(aes(label=tf), hjust=1.6, color="white", size=3.5)
      p <- p + theme_minimal()
      p <- p + coord_flip()
      return(p)
    
    #wordcloud(rownames(t$tf), t$tf[,1], max.words=100, colors=brewer.pal(1, "Dark2"))
    
  },height = 600)
    
    output$wordcloud <- renderPlot({
      
      
      tf <- tfidf_matrix_r()
      
      wordcloud(rownames(tf), tf[,1], max.words=input$wordcloud_freq, colors=brewer.pal(1, "Dark2"))
      
    })
    
  
}
# Run the app ----
shinyApp(ui, server)
