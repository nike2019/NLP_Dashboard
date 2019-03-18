library(quanteda)
library(spacyr)
library(wordnet)
library(mongolite)
library(textclean)
library(tm)
library(data.table)
library(syuzhet)
library(bit64)
library(lubridate)


resolveEmotions <- function(text)
{
  emotions <- get_nrc_sentiment(text)
  emo_bar = colSums(emotions)
  emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
  emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
  return (emotions)
}


resolveInternetSlang <- function(text)
{
  
  result <- replace_internet_slang(text)
  replace_internet_slang(text, ignore.case = FALSE)
  replace_internet_slang(text, replacement = '<<SLANG>>')
  replace_internet_slang(
    text, 
    replacement = lexicon::hash_internet_slang[[2]]
  )
  
  result <- replace_non_ascii(result)
  result <- replace_html(result)
  result <- replace_emoticon(result)
  return (result)
  
}

connectMongoDB <- function(collectionName,dbName)
{
  # create connection, database and collectio
  my_collection = mongo(collection = collectionName, db = dbName) 
  return (my_collection)
}
# Prozeduren welche fÃ¼r die WortÃ¤nlichkeit gebraucht werden
#
#

findMax <- function(x, y) {
  max.len <- 0
  for (tok in x)
  {
    sd <- as.vector(stringdist::stringdist(tok, y, method = "lv"))
    m <- nchar(tok) / 3
    len <- length(sd[sd <= m])
    max.len <- ifelse(len > max.len, len, max.len)
  }
  return(max.len)
}

GetSimilarWords <- function(tok, sympt)
{
  max.len <- findMax(tok, sympt)
  df <- data.frame(x = c(rbind(rep(NA, max.len))))
  
  for (i in tok)
  {
    sd <- as.vector(stringdist::stringdist(i, sympt, method = "lv"))
    names(sd) <- sympt
    m <- nchar(i) / 3
    m <- round(m)
    sd <- sd[sd <= m]
    names(sd) <- unique(names(sd))
    l <- length(sd)
    if (length(sd) > 0)
    {
      
      
      df[, ncol(df) + 1] <- c(c(names(sd)), c(rep(NA, max.len - l)))
      names(df)[ncol(df)] <- i
    }
    
  }
  
  # cut the first col
  return (df[, -1])
}


resolveKeyWords <- function(keywords, tokens)
{
  v <-
    stringdist::amatch(t1$token, symptome_table$symptome, maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  keywords <- v
  keywords[which(!is.na(v))] <- tokens[which(!is.na(v))]
  keywords[which(is.na(v))] <- ""
  res <- keywords
  return(res)
  
  
}

CorpusPreProcessing <- function(text, is_preprocessing = TRUE)
{
  library(utf8)
  library(SnowballC)
  
  corpus <- Corpus(VectorSource(as.vector(text)))
  if (is_preprocessing)
  {
    corpus <- tm_map(corpus, content_transformer(tolower))
    tm::inspect(corpus[[1]])
    corpus <- tm_map(corpus, removeNumbers)
    tm::inspect(corpus[[1]])
    
    
    #corpus <- tm_map(corpus, stemDocument)
    tm::inspect(corpus[[1]])
    corpus <-
      tm_map(corpus, content_transformer(function(x) {
        stringr::str_replace_all(x, "\n", " ")
      }))
    
    corpus <-
      tm_map(corpus, content_transformer(function(x) {
        stringr::str_replace_all(x, "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ")
      }))
    corpus <-
      tm_map(corpus, content_transformer(function(x) {
        stringr::str_replace_all(x, "\\brt\\b\\s*\\@", " ")
      }))
    corpus <-
      tm_map(corpus, content_transformer(function(x) {
        stringr::str_replace_all(x, "[.,?,/]", " ")
      }))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
  }
  corp <- corpus(corpus)
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  return(dt)
  
}



GetTopNSymptomes <- function(data,n)
{
  
  l <- data %>% 
    dplyr::select(symptomes) %>%
    dplyr::group_by(symptomes) %>% 
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::arrange(desc(count)) %>%
    dplyr::top_n(n)
  
  return (l)
}

GetTopNDrugs <- function(data,n)
{
  
  l <- data %>% 
    dplyr::select(drugs) %>%
    dplyr::group_by(drugs) %>% 
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::arrange(desc(count)) %>%
    dplyr::top_n(n)
  
  return (l)
}


GetTopSymptomes <- function(data)
{
  
  l <- data %>% 
    dplyr::group_by(symptomes) %>% 
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::arrange(desc(count)) %>%
    dplyr::top_n(50)
  
  return (l)
}


GetTimeLineForDrugs <- function(data,n)
{
  
  l <- data %>% 
    dplyr::mutate(time_in_days = days(created_at)) %>%
    dplyr::group_by(time_in_days) %>% 
    dplyr::summarise(count = dplyr::n()) 
    
  return (l)
}



AggregateDrugsAndsymptomes <- function(data)
{
  master_data <- data %>% dplyr::select(symptome, drugname, doc_id)
  
  master_data <-
    master_data %>% filter(drugname != "" & symptome != "")
  
  
  
  aggr_syn_drug_data <- master_data %>%
    dplyr::group_by(drugname) %>%
    dplyr::summarise(
      symptomes =
        paste(
          data.table(symptome) %>% dplyr::distinct(symptome) %>% dplyr::pull(),
          collapse = " "
        ),
      count = dplyr::n()
    )
  
  return(aggr_syn_drug_data)
}


AggregateDrugsAndsymptomesWithText <- function(text, data)
{
  corp <- quanteda::corpus(text$text)
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  dt$text_without_icons <- text$text_without_icons
  dt$text_without_slang <- text$text_without_slang
  
  dt$has_url <- str_detect(dt$text, "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  
  dt$is_retweet <- stringi::stri_detect_regex(dt$text, "\\brt\\b\\s*\\@", case_insensitive = TRUE)
  # todo date time for time
  #as.POSIXct("Sat Jan 05 14:05:52 +0000 2019", format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
  
  
  print(text$text_without_slang)
  
  data <-
    data %>% dplyr::select(doc_id,
                           tweetid,
                           drugname,
                           symptome,
                           lemma) %>% dplyr::distinct() %>%
    dplyr::inner_join(dt, by = "doc_id")
  
  
  
  
  master_data <-
    data %>% dplyr::select(symptome, drugname, text,text_without_icons,text_without_slang, doc_id, lemma,has_url,is_retweet,tweetid)
  
  
  #gruppieren der daten medikamente, symptome
  data <- master_data %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      drugs =
        paste(
          data.frame(drugname) %>% dplyr::distinct(drugname) %>% dplyr::pull(),
          collapse = ""
        ),
      symptomes =
        paste(
          data.frame(symptome) %>% dplyr::distinct(symptome) %>% dplyr::pull(),
          collapse = ""
        ),
      tokens = paste(
        data.frame(lemma) %>% dplyr::distinct(lemma) %>% dplyr::pull(),
        collapse = " "
      ),
      
    )
  
  # zusammenfÃ¼hren mit text
  master_data <- master_data %>% distinct(tweetid,doc_id, text,has_url,is_retweet,text_without_slang,text_without_icons)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <-
    data %>% dplyr::filter((data$symptomes != "") & (data$drugs != ""))
  
  return(data)
  
  
  
}

AggregateDrugsAndsymptomesWithTextAndContextWords <- function(text, data)
{
  corp <- quanteda::corpus(text$text)
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  dt$text_without_icons <- text$text_without_icons
  dt$text_without_slang <- text$text_without_slang
  
  dt$has_url <- str_detect(dt$text, "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  
  dt$is_retweet <- stringi::stri_detect_regex(dt$text, "\\brt\\b\\s*\\@", case_insensitive = TRUE)
  # todo date time for time
  #as.POSIXct("Sat Jan 05 14:05:52 +0000 2019", format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
  
  
  print(text$text_without_slang)
  
  data <-
    data %>% dplyr::select(doc_id,
                           tweetid,
                           created_at,
                           drugname,
                           symptom_prewords,
                           symptom_postwords,
                           symptome,
                           lemma) %>% dplyr::distinct() %>%
    dplyr::inner_join(dt, by = "doc_id")
  
  
  
  
  master_data <-
    data %>% dplyr::select(symptome, drugname, text,text_without_icons,text_without_slang, doc_id, lemma,has_url,is_retweet,tweetid,created_at,symptom_prewords,symptom_postwords)
  
  
  #gruppieren der daten medikamente, symptome
  data <- master_data %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      drugs =
        paste(
          data.frame(drugname) %>% dplyr::distinct(drugname) %>% dplyr::pull(),
          collapse = ""
        ),
      symptomes =
        paste(
          data.frame(symptome) %>% dplyr::distinct(symptome) %>% dplyr::pull(),
          collapse = ""
        ),
      tokens = paste(
        data.frame(lemma) %>% dplyr::distinct(lemma) %>% dplyr::pull(),
        collapse = " "
      ),
      
    )
  
  # zusammenfÃ¼hren mit text
  master_data <- master_data %>% distinct(tweetid,created_at,doc_id, text,has_url,is_retweet,text_without_slang,text_without_icons,symptom_prewords,symptom_postwords)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <-
    data %>% dplyr::filter((data$symptomes != "") & (data$drugs != ""))
  
  return(data)
  
  
  
}


ExportFile <- function(data,isCSV)
{
  
  
  tf <- data %>% dplyr::select(text_without_slang,text_without_icons,text,symptomes,drugs,has_url,is_retweet,tweetid,created_at,symptom_prewords,symptom_postwords) %>% 
    #dplyr::filter(has_url == FALSE & is_retweet == FALSE) %>%
    dplyr::mutate(tweetid_int = bit64::as.integer64(tweetid)) %>%
    dplyr::mutate(symptomes = c(gsub("\\[|\\]", "", gsub("\\]\\[", ",", symptomes)))) %>%
    dplyr::mutate(drugs = gsub("\\[|\\]", "", gsub("\\]\\[", ",", drugs))) %>%
    dplyr::mutate(symptom_counts = sapply(strsplit(symptomes,","), length)) %>%
    dplyr::mutate(drug_counts = sapply(strsplit(drugs,","), length)) %>%
    dplyr::mutate(strlen =  sapply(text, nchar))
  
  
  
  if(isCSV)
  {
    
    res <- fwrite(tf,filename)
    
  }
  else
  {
    file <- paste(tempdir(), "/export.xlsx", sep="")
    res <- write.xlsx(tf, file)    
    df <- read.xlsx(file,1)
    file_csv <- paste(tempdir(), "/export.csv", sep="")
    res <- fwrite(df,file_csv)
    
  }
  
  return (file)
  
}

format.twitter.date <- function(datestring){
  Sys.setlocale("LC_TIME", "US")
  datestring <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
  return(datestring)
}

TagDrugAndsymptomes <- function(rt, adrs, drugnames, slangterms,is_context_words = TRUE)
{
  spacy_initialize()
  result = NULL
  text <- tolower(rt$text)
  
  df <- data.frame(tweetid = rt$id)
  
  
  df$created_at <- format.twitter.date(rt$created_at)
  print(df$created_at)
  
  corp <- quanteda::corpus(text)
  df$doc_id <- docnames(corp)
  df$text <- texts(corp)
   
  
  
  
  drug_table <-
    data.table(drugname = c(sapply(drugnames$drugname, function(x)
      tolower(x)))) %>% dplyr::distinct()
  symptome_table <-
    data.table(symptome = c(sapply(adrs, function(x)
      tolower(x)))) %>% dplyr::distinct()
  slang_table <-
    data.table(slang_terms = c(sapply(slangterms$slang.V1, function(x)
      tolower(x))),
      resolved_slang = c(sapply(slangterms$exp.V1, function(x)
        tolower(x)))) %>% dplyr::distinct()
  
  
  res <- spacy_parse(
    df$text,
    pos = TRUE,
    dependency = TRUE,
    lemma = TRUE,
    multithread = TRUE,
    additional_attributes = c(
      "like_num",
      "like_email",
      "is_digit",
      "is_stop",
      "is_alpha",
      "like_url"
    )
  ) %>%
    dplyr::select(
      token,
      lemma,
      doc_id,
      pos,
      like_num,
      like_email,
      is_digit,
      is_stop,
      is_alpha,
      like_url
    )
  
  
  
  
  text_cleaned <- CorpusPreProcessing(df$text)
  text_cleaned <- Corpus(VectorSource(as.vector(text_cleaned$text)))
  corp <- corpus(text_cleaned)
  kwic_text <- texts(corp)
  print(symptome_table$symptome)
  print(kwic_text)
  if(is_context_words)
  {
  ann_symptomes <- 
    kwic(
      kwic_text,
      phrase(symptome_table$symptome),
      valuetype = "glob",
      case_insensitive = TRUE
    ) %>%
    dplyr::select(docname, pre, post, keyword) %>%
    dplyr::mutate(symptome = paste0("[", keyword , "]")) %>%
    dplyr::mutate(symptom_prewords = pre) %>%
    dplyr::mutate(symptom_postwords = post) %>%
    dplyr::mutate(doc_id = docname)
  }
  else
  {
    ann_symptomes <- 
      kwic(
        df$text,
        phrase(symptome_table$symptome),
        valuetype = "glob",
        case_insensitive = TRUE
      ) %>%
      dplyr::select(docname, keyword) %>%
      dplyr::mutate(symptome = paste0("[", keyword , "]")) %>%
      dplyr::mutate(doc_id = docname)
    
  }
  
  
  #res <- res %>% filter(pos == "NOUN")
  # variante 1 mit amatch (lookup) (levenstein distance)
  v <- match(res$lemma, drug_table$drugname)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  
  res$drugname <- v
  res$drugname[which(!is.na(v))] <- paste0("[",res$lemma[which(!is.na(v))],"]")
  # maskieren aller medikamente
  res$drugname[which(is.na(v))] <- ""
  
  res <- res %>% filter(pos %in% c("VERB", "NOUN", "ADJ")) %>%
   dplyr::inner_join(ann_symptomes, by = "doc_id") %>%
    dplyr::inner_join(df, by = "doc_id")
    
   
  
  return (res)
}





GetSynonymes <- function(symptomes)
{
  result <- NULL
  for (i in symptomes)
  {
    url_git <-
      paste("http://wikisynonyms.ipeirotis.com/api/", i, sep = "")
    print(paste("lookup synonym", i, sep = " "))
    # add to list
    # Construct API request
    repos <- httr::GET(url = url_git)
    if (repos$status_code == 200)
    {
      t <- prettify(toJSON(httr::content(repos)))
      k <- jsonlite::fromJSON(t, flatten = TRUE)
      print(paste("print", k$synonymes[, 1], sep = " "))
      result <-
        data.table(rbind(result, data.table(
          "synonymes" = k$synonymes[, 1], "base" = i
        )))
    }
    else
    {
      result <-
        data.table(rbind(result, data.table(
          "synonymes" = "", "base" = i
        )))
      
    }
    
    
  }
  
  return(result)
  
}





GetSynonymesFromWordNet <- function(symptomes)
{
  setDict("C:/Program Files (x86)/WordNet/2.1/dict")
  result <- NULL
  for (i in c(symptomes))
  {
    
      x <- c(synonyms(i, "NOUN"),synonyms(i, "VERB"))
      if (length(x) > 0)
      {
        print(paste("print", x, sep = " "))
        result <-
          data.table(rbind(
            result,
            data.table("synonymes" = x, "symptome" = i),
            fill = TRUE
          ))
      }
      else
      {
        result <-
          data.table(rbind(
            result,
            data.table("synonymes" = i, "symptome" = i),
            fill = TRUE
          ))
        
      }
    
  }
  
  return(result)
  
}
