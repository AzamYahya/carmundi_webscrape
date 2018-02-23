library(purrr)
library(RCurl)
library(httr)
library(curl)
library(xml2)
library(XML)
library(stringi)
library(stringr)


url_base <- "https://www.carmudi.pk/cars/"

map_df(1:5, function(i) { #total pages 1155
  

  cat(".")
  
  pg <-  getURL(sprintf(url_base, i), useragent = str_c(R.version$platform,
                 R.version$version.string, sep = ","),
                httpheader = c(from = "azam.yahya@datacollection.com"))
  
  doc2 <- htmlParse(pg)


links <- xpathSApply(doc2, "//a[@href]",
                     fun = function(x) gsub('\n|\t|\r', "", 
                                            xmlGetAttr(x, "href")))

links <- links[grep("^/[[:digit:]]", links)]

links <- links[!duplicated(links)]

links <- paste("carmudi.pk",links)

links <- gsub(" ", "", links, fixed = TRUE)
gtools::smartbind(links)

}) -> linker


links2 <- colnames(linker)
links3 <- lapply(links2, function(x) gsub("[^[:alnum:][:space:]/._:-]", "",x))
linker <- as.vector(unlist(links3))
