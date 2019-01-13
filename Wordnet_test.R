# get synonymes from the wordnet
library("wordnet")

getFilterTypes()

setDict("C:/Program Files (x86)/WordNet/2.1/dict")

synonyms("vomit", "NOUN")

GetSynonymesFromWordNet <- function(synonymes)
{
  
  setDict("C:/Program Files (x86)/WordNet/2.1/dict")
  result <- NULL
  for(i in synonymes)
  {
    
    
    setDict("C:/Program Files (x86)/WordNet/2.1/dict")
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
  
}


GetSynonymesFromWordNet(c("vomit","headache"))



# get medical symptoms from dbpedia
install.packages("maptools")
install.packages("akima")
install.packages("SPARQL")

library("maptools")
library("akima")
library("SPARQL")

query<-"prefix      dbpedia: <http://dbpedia.org/resource/> 
prefix  dbpedia-owl: <http://dbpedia.org/ontology/> 

SELECT DISTINCT ?place 
?latitude 
?longitude 
?population 
WHERE 
{
  ?place          dbpedia-owl:country  <http://dbpedia.org/resource/United_Kingdom> .
  ?place  dbpedia-owl:populationTotal  ?population                                  .
  ?place                      geo:lat  ?latitude                                    .
  ?place                     geo:long  ?longitude                                   .
  FILTER (      ?latitude > 50 
  AND  ?latitude < 60 
  AND ?longitude < 2
  AND ?longitude > -7 
  )
}"

plotmap<-function(map, pops, im) {
  image(im, col=terrain.colors(50))
  points(pops$results$longitude, pops$results$latitude, cex=0.25, col="#ff30000a") 
  contour(im, add=TRUE, col="brown")
  lines(map, xlim=c(-8,3), ylim=c(54,56), col="black")
  }

q100<-paste(query, " limit 100")

map<-readShapeLines("GBR_adm0.shp")

if(!(exists("pops"))) {
  pops<-SPARQL("http://dbpedia.org/sparql/", query=query)
}

data <- pops$results[with(pops$results, order(longitude,latitude)), ]
data <- data[with(data, order(latitude,longitude)), ]
im <- with(data, interp(longitude, latitude, population**.25, duplicate="mean"), xo=seq(-7,1.25, length=200), yo=seq(50,58,200), linear=FALSE)

plotmap(map, pops, im)

fit<-lm(population ~ latitude*longitude, data)
print(summary(fit))

subd<-data[c("latitude","longitude","population")]
print(cor(subd))

