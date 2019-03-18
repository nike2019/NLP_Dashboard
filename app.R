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
library(xlsx)

####################
# Helper functions
####################
library(jsonlite)
library(httr)
library(ggplot2)
library(gplots)
library(reshape2)
source("Helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "Twitter ADR tool"),
  dashboardSidebar(
    # Input: Select a file ----
    tags$hr(),
    
    actionButton("do", "verbinden mit mongodb"),
    tags$hr(),
    
    actionButton("saveXLS", "speichern als xlsx"),
    
    fileInput(
      "file1",
      "wÃ¤hle twitter json File",
      multiple = TRUE,
      accept = c("json/json",
                 "json/json files",
                 ".json")
    ),
    
    
    fileInput(
      "file2",
      "wÃ¤hle symptome file",
      multiple = TRUE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    fileInput(
      "file3",
      "wÃ¤hle Medikamenten file",
      multiple = TRUE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    
    
    
    # Horizontal line ----
    tags$hr(),
    
    
    checkboxInput("symptomes", "generierung von symptomen aus wordnet", value = FALSE),
    
    
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
                     title = "Tweets gefunden mit Symptomen u. Medikamenten",
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
        "Medikamente / Symptome Zusammenfassung",
        plotOutput("drugsPlot" , width = "100%"),
        plotOutput("symptomePlot" , width = "100%")
        
        
      ),
      tabPanel(
        "Zeitliche Verlauf, Symptome, Medikamente",
        plotOutput("drugsTimePlot" , width = "100%"),
        plotOutput("symptomeTimePlot" , width = "100%")
        
        
      ),
      # tabPanel(
      #   "Frequent used words",
      #   sliderInput(
      #     "wordOCc",
      #     "word occurence",
      #     min = 1,
      #     max = 100,
      #     value = 2
      #   ),
      #   plotOutput("wordplot" , width = "100%")
      #   
      #   
      # ),
      # tabPanel(
      #   "Inverse Document Term Frequency",
      #   sliderInput(
      #     "wordcloud_freq",
      #     "word occurence",
      #     min = 1,
      #     max = 10,
      #     value = 2,
      #     step = 1
      #   ),
      #   plotOutput("wordcloud" , width = "100%")
      #   
        
      #),
      tabPanel(
        "Medikament / Symptom HÃ¤ufigkeit",
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
  isContext <- reactiveVal(TRUE)
  
  mongo_r <- reactive({
    print("mongodb")
    collection <- connectMongoDB("twitter","twitterdb")
    rt <- collection$find()
    
    
    
  })
  
  
  file_r <- reactive({
    req(input$file1)
    print("file")
    rt <- parse_stream(input$file1$datapath)
    
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
    
    rt <- data.frame(rt)
    rt <- rt %>% select(text,lang,created_at,id_str) %>% dplyr::filter(lang == "en") %>%
      mutate(text_without_icons = iconv(text),
             text_without_slang = resolveInternetSlang(text)
             
             ) 
      
    
  })
  
    
    
  drug_r <- reactive({
    #req(input$file3)
    drugfile <-
      ifelse(is.null(input$file3$datapath), " ", input$file3$datapath)
    res <- fread(
      file = ifelse(file.exists(drugfile), drugfile, "daten/drugnames.csv"),
      sep = ",",
      header = FALSE
    )
    
    data.table(drugname = res)
    
  })
  
  symptom_r <- reactive({
    #req(input$file2)
    symptomfile <-
      ifelse(is.null(input$file2$datapath), " ", input$file2$datapath)
    res <- fread(
      file = ifelse(file.exists(symptomfile), symptomfile, "daten/adrs.csv"),
      sep = ",",
      header = FALSE
    )
    
    data.table(symptome = res)
    
  })
  
  TopNSymptomes_r <- reactive({
    
    df <- aggr_data_text_r()
    l <- GetTopNSymptomes(df,40)
    return (l)
    
  })
  
  TopNDrugs_r <- reactive({
    
    df <- aggr_data_text_r()
    l <- GetTopNDrugs(df,40)
    return (l)
    
  })
  
  
  TimelineForDrugs_r <- reactive({
    
    df <- ps()
    l <- GetTimeLineForDrugs(df,40)
    return (l)
    
  })
  
  
  ps <- reactive({
    
    rt <- rs_r()
    
    
    
    
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
      tbl_symptomes <- GetSynonymesFromWordNet(tbl_symptomes$symptome)
    }
    
    # tbl_symptomes$symptome <-
    #   unlist(tbl_symptomes$symptome,
    #          recursive = TRUE,
    #          use.names = TRUE)
    print("Test 100")
    # tbl_symptomes$unique_synonymes <- tbl_symptomes %>% dplyr::distinct(symptome) %>% dplyr::pull()
    # write.table(tbl_symptomes, file = "collected_symptomes.csv",sep = ",", quote = FALSE, row.names = FALSE)
    # print("test 101")
    
    
    ################################
    # find ratio between advertise
    ################################
    
    
    outcome <- TagDrugAndsymptomes(rt, tbl_symptomes, tbl_drug[1:1000], tbl_slangterms,isTruthy(isContext()))
    #outcome$advertising_ratio <- s
    
    
  })
  
  
  
  #######################################################
  # find aggregated drugs and symptomes with twitter text
  #######################################################
  aggr_data_text_r <- reactive({
    rt <- rs_r()
    #rt$text <- resolveInternetSlang(rt$text)
    newValue <- TRUE     # newValue <- rv$value - 1
    isContext(newValue) 
    
    df <- ps()
    outcome <- AggregateDrugsAndsymptomesWithText(rt, df)
    
  })
  
  
  #######################################################
  # find aggregated drugs and symptomes with twitter text
  # with additional context words
  #######################################################
  aggr_data_text_and_context_r <- reactive({
    rt <- rs_r()
    #rt$text <- resolveInternetSlang(rt$text)
    
    df <- ps()
    
    outcome <- AggregateDrugsAndsymptomesWithTextAndContextWords(rt, df)
    
  })
  ################################
  # aggregate drugs and symtomes
  ################################
  aggr_data_r <- reactive({
    df <- ps()
    outcome <- AggregateDrugsAndsymptomes(df)
    
  })
  
  ################################
  # Text processing (cleaning)
  ################################
  corp_preprocessing_r <- reactive({
    rt <- ps()
    corpus <- CorpusPreProcessing(rt$lemma)
    
    res <- corpus$text
  })
  
    
    
  ##################################################
  # term frequency inverse document frequency matrix
  ##################################################
  tfidf_matrix_r <- reactive({
    corpus <- corp_preprocessing_r()
    
    dtm <-
      DocumentTermMatrix(Corpus(VectorSource(as.vector(corpus))), control = list(weighting = weightTfIdf))
    dtm = removeSparseTerms(dtm, 0.95)
    tf = data.frame(sort(colSums(as.matrix(dtm)), decreasing = TRUE))
    #wordcloud(rownames(tf), tf[,1], max.words=100, colors=brewer.pal(1, "Dark2"))
    
  })
  
  
  ################################
  # document term matrix
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
    print("Get data from mongodb")
  })  
  
  observeEvent(input$saveXLS, {
    file <- ExportFile(aggr_data_text_and_context_r(),FALSE)
    print(paste("save to xlsx file to: ",file,sep = ""))
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
  
  library(RColorBrewer)
  
  # client-side processing
  output$heatmap = renderPlot({
    idx = input$trace_table_rows_selected
    if (length(idx)) {
      tbl_sym <- symptom_r()
      txt <- aggr_data_text_r()$text[idx]
      c <- CorpusPreProcessing(txt,is_preprocessing = FALSE)
      token <- tokens(c$text)
      token <- as.vector(unlist(token))
      
      x <- adist(tbl_sym$symptome,counts = TRUE,token)
      colnames(x) <- token
      rownames(x) <- tbl_sym$symptome
      
      
      melted_cormat <- melt(x)
      
      
      df <- data.frame(melted_cormat)
      
      df <- df %>% mutate(dist = round(nchar(as.character(Var1))/5), tweet = Var2, symptomes = Var1)
      
      df <- df %>% distinct(tweet,symptomes,dist,value) %>% filter(value <= dist + 1) %>% top_n(20)
      
      ggplot(data = df, aes(x = symptomes, y = tweet, fill = value)) + 
        geom_tile(aes(fill = value), size = 1) + 
        scale_fill_gradient(low = "white", high = "steelblue") + 
        theme_grey(base_size = 12) + 
        scale_x_discrete(expand = c(0, 0)) + 
        scale_y_discrete(expand = c(0, 0)) +
        theme(axis.ticks = element_blank(), 
              #panel.background = element_blank(), 
              plot.title = element_text(size = 12, colour = "gray50")) + 
        coord_equal() + 
        labs(x = "tweet",y = "symptome") + 
        geom_text(aes(label=value), size=4)
      
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
        GetSimilarWords(as.character(tokens(c$text)), tbl_sym$symptome)
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
  output$symptomePlot <- renderPlot({
    
    
    topNSymtptomes <- TopNSymptomes_r()
    
    return(ggplot(data=topNSymtptomes, aes(x = reorder(symptomes, -count), y=count, fill = symptomes)) +
             theme(legend.position = "none") +
             geom_bar(stat="identity") +
             labs(x = "Symptom",y = "Anzahl Treffer") + 
             coord_flip()
    
    )
  })
  output$drugsPlot <- renderPlot({
    
    
    topNDrugs <- TopNDrugs_r()
    
    return(ggplot(data=topNDrugs, aes(x = reorder(drugs, -count), y=count, fill = drugs)) +
             theme(legend.position = "none") +
             geom_bar(stat="identity") +
             labs(x = "Medikament",y = "Anzahl Treffer") + 
             coord_flip()
           
    )
  })
  output$drugsTimePlot <- renderPlot({
    
    timeline <- TimelineForDrugs_r()
    
    
    return(ggplot(timeline, aes(time_in_days, count)) + geom_line() +
             xlab("") + ylab("Daily Views")
           
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
