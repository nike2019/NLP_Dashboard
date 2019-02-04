library(quanteda)
library(spacyr)
library(wordnet)
library(mongolite)
library(textclean)
library(tm)
library(data.table)
library(syuzhet)


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
  
  print(result)
  return (result)
  
}

connectMongoDB <- function(collectionName,dbName)
{
  my_collection = mongo(collection = collectionName, db = dbName) # create connection, database and collectio
  return (my_collection)
}
# Prozeduren welche für die Wortänlichkeit gebraucht werden
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
      
      print(paste("add data to dataframe", sd, sep = ":"))
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

CorpusPreProcessing <- function(text)
{
  library(utf8)
  library(SnowballC)
  
  corpus <- Corpus(VectorSource(as.vector(text)))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  corp <- corpus(corpus)
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  return(dt)
  
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
  corp <- quanteda::corpus(text)
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  dt$has_url <- str_detect(dt$text, "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  
  
  
  data <-
    data %>% dplyr::select(doc_id,
                           drugname,
                           symptom_prewords,
                           symptom_postwords,
                           symptome,
                           lemma) %>% dplyr::distinct() %>%
    dplyr::inner_join(dt, by = "doc_id")
  
  
  
  
  master_data <-
    data %>% dplyr::select(symptome, drugname, text, doc_id, lemma,has_url)
  
  
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
  
  # zusammenführen mit text
  master_data <- master_data %>% distinct(doc_id, text,has_url)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <-
    data %>% dplyr::filter((data$symptomes != "") & (data$drugs != ""))
  print(paste("debug 3: ",dt$doc_id,sep = " "))
  return(data)
  
  
  
}






TagDrugAndsymptomes <- function(text, adrs, drugnames, slangterms)
{
  spacy_initialize()
  result = NULL
  text <- tolower(text)
  
  corp <- quanteda::corpus(text)
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
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
    dt$text,
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
  
  
  
  filtered_doc_ids <- res %>% filter(
    like_email == TRUE &
    like_url == TRUE
    
  ) %>% select(doc_id) %>% distinct()
  
  
  
  
  ann_symptomes <-
    kwic(
      dt$text,
      phrase(symptome_table$symptome),
      valuetype = "glob",
      case_insensitive = TRUE
    ) %>%
    dplyr::select(docname, pre, post, keyword) %>%
    dplyr::mutate(keyword = NULL, symptome = paste0("[", keyword , "]")) %>%
    dplyr::mutate(pre = NULL, symptom_prewords = pre) %>%
    dplyr::mutate(post = NULL, symptom_postwords = post) %>%
    dplyr::mutate(docname = NULL, doc_id = docname)
  
  
  #res <- res %>% filter(pos == "NOUN")
  # variante 1 mit amatch (lookup) (levenstein distance)
  v <- stringdist::amatch(res$lemma, drug_table$drugname, maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  res$drugname <- v
  res$drugname[which(!is.na(v))] <- paste0("[",res$lemma[which(!is.na(v))],"]")
  # maskieren aller medikamente
  res$drugname[which(is.na(v))] <- ""
  
  res <- res %>% filter(pos %in% c("VERB", "NOUN", "ADJ")) %>%
   dplyr::inner_join(ann_symptomes, by = "doc_id") #%>%
   #dplyr::filter(!doc_id %in% filtered_doc_ids$doc_id)
    
  
  
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
