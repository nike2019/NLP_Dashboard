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
library(wordcloud2)
####################
# Helper functions
####################
library(jsonlite)
library(httr)
library(ggplot2)
source("Helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "Twitter ADR tool"),
  dashboardSidebar(
    
    # Input: Select a file ----
    
    fileInput("file1", "Choose twitter json File",
              multiple = TRUE,
              accept = c("json/json",
                         "json/json files",
                         ".json")),
    
    
    fileInput("file2", "Choose symptome file",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    fileInput("file3", "Choose drug file",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    
      
      
      # Horizontal line ----
      tags$hr(),
      
      
      checkboxInput("symptomes","generate symptomes", value = FALSE),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")),
    
  
  # App title ----
  dashboardBody(
    navbarPage("Views",
               tabPanel(
                 "Messages",
                 
                   box( title = "messages and found drugs / symptomes", status = "primary", height = 
                          "750",width = "600",solidHeader = T, 
                        column(width = 12,
                               DT::dataTableOutput("trace_table"),style = "height:630px; overflow-y: scroll;"
                        )
                   )
                 
               ),
               tabPanel("Frequent used words",
                        sliderInput("wordOCc","word occurence",min = 1, max = 100, value = 2),
                        plotOutput("wordplot" ,width = "100%")            
                             
                        
               ),
               tabPanel("Drug / symptom frequency",
               DT::dataTableOutput("aggr_table"),style = "height:630px; overflow-y: scroll;")
      
      )))
    
  options(shiny.maxRequestSize=30*1024^3)
  # Define server logic to read selected file ----
  server <- function(input, output) {
  
  
  rs_r <- reactive({
    req(input$file1)
    rt <- parse_stream(input$file1$datapath)
    rt <- rt %>% dplyr::filter(lang == "en")
    
    
  })
  
  drug_r <- reactive({
    
    #req(input$file3)
    file <- ifelse(is.null(input$file3$datapath), " ", input$file3$datapath)
    
  })
  
  symptome_r <- reactive({
    
    req(input$file3)
    tbl_symptome <- fread(file = input$file2$datapath,sep=",", header=TRUE)
    
  })
  
  ps <- reactive(
  {
    
    rt <- rs_r()
    
    #tbl_drug <- drug_r() #fread(file = "Actual Datafiles/drugnames.csv",sep=",", header=TRUE, )
    
    drugfile <- drug_r()
    
    tbl_drug <- fread(file = ifelse(file.exists(drugfile), drugfile, "daten/drugnames.csv"),sep=",", header=TRUE)
    print(drugfile)
    
    tbl_symptomes <- fread(file = "daten/adrs.csv",sep=",", header=TRUE)
    tbl_slangterms <- fread(file = "daten/twitter_slang_terms.csv",sep=",", header=TRUE)
  
    ############################
    # enrich symptom list
    ############################
    library(dplyr)
    if(input$symptomes == TRUE)
    {
      tbl_symptomes <- GetsymptomesFromWordNet(tbl_symptomes$symptome)  
    }
  
    tbl_symptomes$synonymes <-unlist(tbl_symptomes$synonymes, recursive = TRUE, use.names = TRUE)
    tbl_symptomes$unique_synonymes <- tbl_symptomes %>% dplyr::distinct(synonymes) %>% dplyr::pull()
    write.table(tbl_symptomes$unique_synonymes, file = "collected_symptomes.csv")
    
    
    if('extended_tweet.full_text' %in% names(rt))
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
  
    rt$isAdvertise <- str_detect(rt$text,"(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
    s <- rt %>% dplyr::group_by(isAdvertise) %>% dplyr::count(isAdvertise)
  
  
  
    # remove advertising
    noadvertise <- rt %>% dplyr::select(isAdvertise,text) %>% dplyr::filter(isAdvertise == FALSE)
  
  
    outcome <- TagDrugAndsymptomes(noadvertise$text,tbl_symptomes,tbl_drug[1:100],tbl_slangterms)
    #outcome$advertising_ratio <- s
  
    
  })
  
  aggr_data_text_r <- reactive({
    
    outcome <- AggregateDrugsAndsymptomesWithText(ps())
    
  })
  
  aggr_data_r <- reactive({
    
    outcome <- AggregateDrugsAndsymptomes(ps())
    
    })
  
  
  corp_preprocessing_r <- reactive({
    rt <- ps()
    corpus <- CorpusPreProcessing(rt$text)
    
    res <- corpus$text
  }) 
    
    
    
    dtm_matrix_r <-  reactive({
      
      
      corpus <- corp_preprocessing_r()
      dtm <- DocumentTermMatrix(Corpus(VectorSource(as.vector(corpus))))
      tf <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
      
      if(input$disp == "head") {
        
        
        res   <- data.frame(list(tf = tf, word=names(tf)))
      }
      else
      {
        res   <- data.frame(tf = tf, word=names(tf))
        
      }  
      
      
      
    })
    
    
     
  
  
    output$trace_table <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    if(input$disp == "head") {
      return(head(data.table(text = aggr_data_text_r()$text, "drug(s)" = aggr_data_text_r()$drugs, "symptome" = aggr_data_text_r()$symptomes)))
    }
    else {
      return(data.table(text = aggr_data_text_r()$text, "drug(s)" = aggr_data_text_r()$drugs, "symptome" = aggr_data_text_r()$symptomes))
    }
    
  })
    
    
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
    
  },height = 600)
    
    
  
}
# Run the app ----
shinyApp(ui, server)
