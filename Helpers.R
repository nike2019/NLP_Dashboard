library(quanteda)
library(spacyr)



resolveKeyWords <- function(keywords,tokens)
{
  
  v <- stringdist::amatch(t1$token,synonyme_table$synonyme,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  keywords <- v 
  keywords[which( !is.na(v) )] <- tokens[which( !is.na(v) )]
  keywords[which( is.na(v) )] <- ""
  res <- keywords
  return(res)
  
  
}



TagDrugAndSynonymes <- function(text, adrs, drugnames, slangterms)
{
  
  
  spacy_initialize()
  result = NULL
  corp <- quanteda::corpus(text)
  
  
  
  
  
  
  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  drug_table <- data.table(drugname = c(sapply(drugnames$drugname,function(x) tolower(x))))
  synonyme_table <- data.table(synonyme = c(sapply(adrs$synonyme,function(x) tolower(x))))
  slang_table <- data.table(slang_terms = c(sapply(slangterms$slang.V1,function(x) tolower(x))),
                            resolved_slang = c(sapply(slangterms$exp.V1,function(x) tolower(x))))
  
  t1 <- spacy_parse(dt$text, pos = TRUE, dependency = TRUE) %>%
    dplyr::select(token,doc_id,pos) %>%
    dplyr::inner_join(dt, by = "doc_id") %>%
    dplyr::filter(pos == "NOUN" | pos == "PROPN") 
  
  
  
  # variante mit keyword in context (kwic)
  #t1$token <- data.table(token = sapply(t1$token,function(x){ tolower(x)}))
  #t1$token <- unlist(t1$token)
  #corp1 <- quanteda::corpus(t1, docid_field = "doc_id", text_field = "token")
  #corp1 <-  tokens_tolower(as.tokens(corp1$tokens), keep_acronyms = TRUE)
  #y1 <- kwic(corp1, drug_table, valuetype = "glob")
  
  
  # variante 1 mit amatch (lookup) (levenstein distance)
  v <- stringdist::amatch(t1$token,drug_table$drugname,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$drugName <- v 
  t1$drugName[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
  # maskieren aller medikamente
  t1$drugName[which( is.na(v) )] <- ""
  
  
  v <- stringdist::amatch(t1$token,synonyme_table$synonyme,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$synonyme <- v 
  t1$synonyme[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
  t1$synonyme[which( is.na(v) )] <- ""
  
  v <- stringdist::amatch(t1$token,slang_table$slang_terms,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$slang <- v 
  t1$slang[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
  t1$slang[which( is.na(v) )] <- ""
  
  
  master_data <- t1 %>% dplyr::select(synonyme,drugName,text,doc_id)
  #master_data <- master_data %>% dplyr::filter(is.na(drugName) == FALSE && is.na(synonyme) == FALSE)
  
  #gruppieren der daten medikamente, symptome
  data <- master_data %>% 
    dplyr::group_by(doc_id) %>% 
    dplyr::summarise(drugs = 
                       paste(data.frame(drugName) %>% dplyr::distinct(drugName) %>% dplyr::pull(),collapse=" "), 
                     synonymes = 
                       paste(data.frame(synonyme) %>% dplyr::distinct(synonyme) %>% dplyr::pull(),collapse=" "))
  
  # zusammenführen mit text
  master_data <- master_data %>% distinct(doc_id,text)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <- data %>% dplyr::filter((data$synonymes != "") & (data$drugs != ""))
  
  return(data)
         
  #t <- spacy_parse(corp, pos = TRUE) %>%
  #  unnest_tokens(word, token) %>%
  #  dplyr::filter(pos == "NOUN")
  #library(dplyr)
  #library(tidytext)
  #t <- spacy_parse(corp, pos = TRUE) %>%
  #  unnest_tokens(word1, token) %>%
  #  dplyr::filter(pos == "NOUN")
  
  
  
  # find all adrs in the corpus
  #y1 <- kwic(t, adrs, window = 5, valuetype = "glob")
  # find all drugs in the corpus
  #y2 <- kwic(t, drugnames, window = 5, valuetype = "glob")
  # join them together (symptom + drugname)
  #i <- y1 %>% dplyr::inner_join(y2,by="docname")
  #result <- cbind(i,data.table(sapply(i$docname, function(x) corp[x])))
  #return(result)
}


GetSynonymes <- function(synonymes)
{
  result <- NULL
  for(i in synonymes)
  {
    
    url_git <- paste("http://wikisynonyms.ipeirotis.com/api/", i, sep="")
    print(paste("lookup synonym",i,sep = " "))
    # add to list
    # Construct API request
    repos <- httr::GET(url = url_git)
    if(repos$status_code == 200)
    {
      t <- prettify(toJSON(httr::content(repos)))
      k <- jsonlite::fromJSON(t,flatten = TRUE)
      print(paste("print",k$terms[,1],sep = " "))
      result <- data.table(rbind(result,data.table("terms" = k$terms[,1], "base" = i)))
    }
    else
    {
      result <- data.table(rbind(result,data.table("terms" = "", "base" = i)))
      
    }
    
    
  }
  
  return(result)
  
}

CorpusPreProcessing <- function(text)
{
  
  library(utf8)
  corpus <- Corpus(VectorSource(as.vector(text)))
  
  
  #corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  return(corpus)
  
}
