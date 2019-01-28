library(quanteda)
library(spacyr)
library(wordnet)
library(mongolite)




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
    m <- nchar(tok) %% 4
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
  master_data <- data %>% dplyr::select(symptom, drugName, doc_id)
  
  master_data <-
    master_data %>% filter(drugName != "" & symptom != "")
  
  
  
  aggr_syn_drug_data <- master_data %>%
    dplyr::group_by(drugName) %>%
    dplyr::summarise(
      symptomes =
        paste(
          data.frame(symptom) %>% dplyr::distinct(symptom) %>% dplyr::pull(),
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
  
  data <-
    data %>% dplyr::select(doc_id,
                           drugName,
                           symptom_prewords,
                           symptom_postwords,
                           symptom,
                           lemma) %>% dplyr::distinct() %>%
    dplyr::inner_join(dt, by = "doc_id")
  
  
  
  
  master_data <-
    data %>% dplyr::select(symptom, drugName, text, doc_id, lemma)
  
  
  #gruppieren der daten medikamente, symptome
  data <- master_data %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      drugs =
        paste(
          data.frame(drugName) %>% dplyr::distinct(drugName) %>% dplyr::pull(),
          collapse = "\n\r"
        ),
      symptomes =
        paste(
          data.frame(symptom) %>% dplyr::distinct(symptom) %>% dplyr::pull(),
          collapse = "\n\r"
        ),
      tokens = paste(
        data.frame(lemma) %>% dplyr::distinct(lemma) %>% dplyr::pull(),
        collapse = " "
      ),
      
    )
  
  # zusammenführen mit text
  master_data <- master_data %>% distinct(doc_id, text)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <-
    data %>% dplyr::filter((data$symptomes != "") & (data$drugs != ""))
  
  return(data)
  
  
  
}




# TagDrugAndsymptomes <- function(text, adrs, drugnames, slangterms)
# {
#
#
#   spacy_initialize()
#   result = NULL
#   corp <- quanteda::corpus(text)
#
#
#
#
#   dt <- data.table(doc_id = docnames(corp), text = texts(corp))
#
#   drug_table <- data.table(drugname = c(sapply(drugnames$drugname,function(x) tolower(x))))
#   symptome_table <- data.table(symptome = c(sapply(adrs$unique_synonymes,function(x) tolower(x))))
#   slang_table <- data.table(slang_terms = c(sapply(slangterms$slang.V1,function(x) tolower(x))),
#                             resolved_slang = c(sapply(slangterms$exp.V1,function(x) tolower(x))))
#
#   t1 <- spacy_parse(dt$text, pos = TRUE, dependency = TRUE) %>%
#     dplyr::select(token,doc_id,pos) %>%
#     dplyr::inner_join(dt, by = "doc_id") #%>%
#     #dplyr::filter(pos == "NOUN" | pos == "PROPN" | pos == "VERB")
#
#
#
#   # variante mit keyword in context (kwic)
#   #t1$token <- sapply(t1$token,function(x){ tolower(x)})
#
#   res <- CorpusPreProcessing(t1$token)
#   t1$token <- res$text
#
#
#   # variante 1 mit amatch (lookup) (levenstein distance)
#   v <- stringdist::amatch(t1$token,drug_table$drugname,maxDist = 1)
#   # extrahieren der indexe welche einen konkreten wert beinhalten
#   t1$drugName <- v
#   t1$drugName[which( !is.na(v) )] <- t1$token[which( !is.na(v),  )]
#   # maskieren aller medikamente
#   t1$drugName[which( is.na(v) )] <- ""
#
#
#   v <- stringdist::amatch(t1$token,symptome_table$symptome,maxDist = 0.1)
#   # extrahieren der indexe welche einen konkreten wert beinhalten
#   t1$symptome <- v
#   t1$symptome[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
#   t1$symptome[which( is.na(v) )] <- ""
#
#   v <- stringdist::amatch(t1$token,slang_table$slang_terms,maxDist = 1)
#   # extrahieren der indexe welche einen konkreten wert beinhalten
#   t1$slang <- v
#   t1$slang[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
#   t1$slang[which( is.na(v) )] <- ""
#
#
#
#   return(t1)
#
#
# }


# TagDrugAndsymptomes <- function(text, adrs, drugnames, slangterms)
# {
#
#
#   spacy_initialize()
#   result = NULL
#   corp <- quanteda::corpus(text)
#
#   dt <- data.table(doc_id = docnames(corp), text = texts(corp))
#
#   drug_table <- data.table(drugname = c(sapply(drugnames$drugname,function(x) tolower(x))))
#   symptome_table <- data.table(symptome = c(sapply(adrs$unique_synonymes,function(x) tolower(x))))
#   slang_table <- data.table(slang_terms = c(sapply(slangterms$slang.V1,function(x) tolower(x))),
#                             resolved_slang = c(sapply(slangterms$exp.V1,function(x) tolower(x))))
#
#
#   t1 <- spacy_parse(dt$text, pos = TRUE, dependency = TRUE) %>%
#     dplyr::select(token,doc_id,pos)
#
#
#
#
#
#   ann_symptomes <- kwic(dt$text, phrase(symptome_table$symptome), valuetype = "glob", case_insensitive = TRUE) %>%
#     dplyr::select(docname,pre,post,keyword) %>%
#     dplyr::mutate(keyword = NULL, symptom = keyword) %>%
#     dplyr::mutate(pre = NULL, symptom_prewords = pre) %>%
#     dplyr::mutate(post = NULL, symptom_postwords = post) %>%
#     dplyr::mutate(docname = NULL, doc_id = docname)
#
#
#
#
#   res <- CorpusPreProcessing(t1$token)
#   t1$token <- res$text
#
#   # variante 1 mit amatch (lookup) (levenstein distance)
#   v <- stringdist::amatch(t1$token,drug_table$drugname,maxDist = 1)
#   # extrahieren der indexe welche einen konkreten wert beinhalten
#   t1$drugName <- v
#   t1$drugName[which( !is.na(v) )] <- t1$token[which( !is.na(v),  )]
#   # maskieren aller medikamente
#   t1$drugName[which( is.na(v) )] <- ""
#
#
#   res <- t1 %>% dplyr::inner_join(ann_symptomes, by = "doc_id")
#
#   result <- res %>% dplyr::select(doc_id,drugName,symptom_prewords,symptom_postwords,symptom) %>% dplyr::distinct() %>%
#     dplyr::inner_join(dt, by = "doc_id")
#
#   return(result)
#
# }


TagDrugAndsymptomes <- function(text, adrs, drugnames, slangterms)
{
  spacy_initialize()
  result = NULL
  text <- tolower(text)
  
  corp <- quanteda::corpus(text)
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  drug_table <-
    data.table(drugname = c(sapply(drugnames$drugname, function(x)
      tolower(x))))
  symptome_table <-
    data.table(symptome = c(sapply(adrs$unique_synonymes, function(x)
      tolower(x))))
  slang_table <-
    data.table(slang_terms = c(sapply(slangterms$slang.V1, function(x)
      tolower(x))),
      resolved_slang = c(sapply(slangterms$exp.V1, function(x)
        tolower(x))))
  
  symptome_table <- symptome_table %>% distinct()
  
  
  t1 <- spacy_parse(
    dt$text,
    pos = TRUE,
    dependency = TRUE,
    lemma = TRUE,
    additional_attributes = c(
      "like_num",
      "like_url",
      "like_email",
      "is_digit",
      "is_stop",
      "is_alpha"
    )
  ) %>%
    dplyr::select(
      token,
      lemma,
      doc_id,
      pos,
      like_num,
      like_email,
      like_url,
      is_digit,
      is_stop,
      is_alpha
    )
  
  
  ann_symptomes <-
    kwic(
      dt$text,
      phrase(symptome_table$symptome),
      valuetype = "glob",
      case_insensitive = TRUE
    ) %>%
    dplyr::select(docname, pre, post, keyword) %>%
    dplyr::mutate(keyword = NULL, symptom = paste0("[", keyword , "]")) %>%
    dplyr::mutate(pre = NULL, symptom_prewords = pre) %>%
    dplyr::mutate(post = NULL, symptom_postwords = post) %>%
    dplyr::mutate(docname = NULL, doc_id = docname)
  
  
  
  # variante 1 mit amatch (lookup) (levenstein distance)
  v <- stringdist::amatch(t1$lemma, drug_table$drugname, maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$drugName <- v
  t1$drugName[which(!is.na(v))] <- t1$token[which(!is.na(v),)]
  # maskieren aller medikamente
  t1$drugName[which(is.na(v))] <- ""
  
  
  
  
  res <- t1 %>% filter(
    is_alpha == TRUE,
    like_num == FALSE,
    like_email == FALSE,
    like_url == FALSE,
    is_digit == FALSE,
    is_stop == FALSE
  )
  
  res <- res %>% filter(pos %in% c("VERB", "NOUN", "ADJ"))
  res <- res %>% dplyr::inner_join(ann_symptomes, by = "doc_id")
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
  for (i in symptomes)
  {
    tryCatch({
      x <- synonyms(i, "NOUN")
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
    },
    error = function(err) {
      next
    })
  }
  
  return(result)
  
}
