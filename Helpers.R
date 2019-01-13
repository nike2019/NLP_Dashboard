library(quanteda)
library(spacyr)
library(wordnet)





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




AggregateDrugsAndSynonymes <- function(data)
{
  master_data <- data %>% dplyr::select(synonyme,drugName,text,doc_id)
  
  drug_data <- data %>% dplyr::select(drugName,doc_id) %>% filter(drugName != "")
  syn_data <- data %>% dplyr::select(synonyme,doc_id) %>% filter(synonyme != "")
  
  syn_drug_data <- drug_data %>% dplyr::inner_join(syn_data, by = "doc_id")
  
  aggr_syn_drug_data <- syn_drug_data %>% 
    dplyr::group_by(drugName) %>%
    dplyr::summarise(synonymes = 
                       paste(data.frame(synonyme) %>% dplyr::distinct(synonyme) %>% dplyr::pull(),collapse=" "),
                     count = dplyr::n())
  
  
  
  syn_drug_data <- syn_drug_data %>% distinct()
  syn_drug_data %>% dplyr::group_by(drugName) %>% dplyr::summarise(n = n())
  
  return(aggr_syn_drug_data)
  
  
}


AggregateDrugsAndSynonymesWithText <- function(data)
{
  
  master_data <- data %>% dplyr::select(synonyme,drugName,text,doc_id)
  #master_data <- master_data %>% dplyr::filter(is.na(drugName) == FALSE && is.na(synonyme) == FALSE)
  
  #gruppieren der daten medikamente, symptome
  data <- master_data %>% 
    dplyr::group_by(doc_id) %>% 
    dplyr::summarise(drugs = 
                       paste(data.frame(drugName) %>% dplyr::distinct(drugName) %>% dplyr::pull(),collapse="\n\r"), 
                     synonymes = 
                       paste(data.frame(synonyme) %>% dplyr::distinct(synonyme) %>% dplyr::pull(),collapse="\n\r"))
  
  # zusammenführen mit text
  master_data <- master_data %>% distinct(doc_id,text)
  data <- data %>% dplyr::inner_join(master_data, by = "doc_id")
  data <- data %>% dplyr::filter((data$synonymes != "") & (data$drugs != ""))
  
  return(data)
  
  
  
}




TagDrugAndSynonymes <- function(text, adrs, drugnames, slangterms)
{
  
  
  spacy_initialize()
  result = NULL
  corp <- quanteda::corpus(text)
  
  
  

  dt <- data.table(doc_id = docnames(corp), text = texts(corp))
  
  drug_table <- data.table(drugname = c(sapply(drugnames$drugname,function(x) tolower(x))))
  synonyme_table <- data.table(synonyme = c(sapply(adrs$unique_terms,function(x) tolower(x))))
  slang_table <- data.table(slang_terms = c(sapply(slangterms$slang.V1,function(x) tolower(x))),
                            resolved_slang = c(sapply(slangterms$exp.V1,function(x) tolower(x))))
  
  t1 <- spacy_parse(dt$text, pos = TRUE, dependency = TRUE) %>%
    dplyr::select(token,doc_id,pos) %>%
    dplyr::inner_join(dt, by = "doc_id") #%>%
    #dplyr::filter(pos == "NOUN" | pos == "PROPN" | pos == "VERB") 
  
  
  
  # variante mit keyword in context (kwic)
  t1$token <- sapply(t1$token,function(x){ tolower(x)})
  
  
  # variante 1 mit amatch (lookup) (levenstein distance)
  v <- stringdist::amatch(t1$token,drug_table$drugname,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$drugName <- v 
  t1$drugName[which( !is.na(v) )] <- t1$token[which( !is.na(v),  )]
  # maskieren aller medikamente
  t1$drugName[which( is.na(v) )] <- ""
  
  
  v <- stringdist::amatch(t1$token,synonyme_table$synonyme,maxDist = 0.1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$synonyme <- v 
  t1$synonyme[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
  t1$synonyme[which( is.na(v) )] <- ""
  
  v <- stringdist::amatch(t1$token,slang_table$slang_terms,maxDist = 1)
  # extrahieren der indexe welche einen konkreten wert beinhalten
  t1$slang <- v 
  t1$slang[which( !is.na(v) )] <- t1$token[which( !is.na(v) )]
  t1$slang[which( is.na(v) )] <- ""
  
  
  
  return(t1)
         
  
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





GetSynonymesFromWordNet <- function(synonymes)
{
  
  setDict("C:/Program Files (x86)/WordNet/2.1/dict")
  result <- NULL
  for(i in synonymes)
  {
    tryCatch({
      x <- synonyms(i, "NOUN")
      if(length(x) > 0)
      {
        print(paste("print",x,sep = " "))
        result <- data.table(rbind(result,data.table("terms" = x, "synonyme" = i),fill = TRUE))
      }
      else
      {
        result <- data.table(rbind(result,data.table("terms" = i, "synonyme" = i), fill = TRUE))
        
      }
    },
    error = function(err){
      next
    })
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
