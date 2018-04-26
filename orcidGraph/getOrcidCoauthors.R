library(rorcid)
library(igraph)
library(visNetwork)
require(tidyverse)
# https://github.com/agbarnett/work-pals/blob/master/network.R
# functiosn from
#https://downwithtime.wordpress.com/2015/02/12/building-your-network-using-orcid-and-ropensci/
get_doi <- function(x){
  #  This pulls the DOIs out of the ORCiD record:
  list.x <- x$'work-external-identifiers.work-external-identifier'

  #  We have to catch a few objects with NULL DOI information:
  do.call(rbind.data.frame,lapply(list.x, function(x){
    if(length(x) == 0 | (!'DOI' %in% x[,1])){
      data.frame(value=NA)
    } else{
      data.frame(value = x[which(x[,1] %in% 'DOI'),2])
    }
  }))
}

get_papers <- function(x){
  all.papers <- x[[1]]$works # this is where the papers are.
  papers <- data.frame(title = all.papers$'work-title.title.value',
                       doi   = get_doi(all.papers))

  paper.doi <- lapply(1:nrow(papers), function(x){
    if(!is.na(papers[x,2]))return(orcid_doi(dois = papers[x,2], fuzzy = FALSE))
    # sometimes there's no DOI
    # if that's the case then just return NA:
    return(NA)
  })

  your.papers <- lapply(1:length(paper.doi), function(x){
    if(is.na(paper.doi[[x]])){
      data.frame(doi=NA, orcid=NA, name=NA)
    } else {
      data.frame(doi = papers[x,2],
                 orcid = paper.doi[[x]][[1]]$data$'orcid-identifier.path',
                 name = paste(paper.doi[[x]][[1]]$data$'personal-details.given-names.value',
                              paper.doi[[x]][[1]]$data$'personal-details.family-name.value',
                              sep = ' '),
                 stringsAsFactors = FALSE)
    }})
  do.call(rbind.data.frame, your.papers)

}


simon.record <- orcid_id(orcid = '0000-0001-7719-6695',
                         profile="works")

simons <- get_papers(simon.record)

unique.orcids <- unique(simons$orcid)

all.colleagues <- list()

for(i in 1:length(unique.orcids)){
  all.colleagues[[i]] <- get_papers(orcid_id(orcid = unique.orcids[i], profile="works"))
}


myWorks <- works( "0000-0001-7719-6695")
ids <- identifiers(myWorks)

firstID <- orcid_doi(ids[1])

tmp <- sapply(ids, simplify = FALSE, function(x){
  itemRecord <- orcid_doi(x)
  allAuthors <- itemRecord[[1]]$`orcid-identifier.path`
  thisAuthor <- "0000-0001-7719-6695"
  
  otherAuthor <- allAuthors[!grepl(thisAuthor, fixed = TRUE, allAuthors)]
  if(length(otherAuthor) > 0){
    ret <- data.frame(otherAuthor, manuscript = x)
  }else{
    ret <- data.frame(otherAuthor = "noCoAuthors", manuscript = x)
  }

  return(ret)
  
})


# get list of second order relationships

secondOrder <- lapply(tmp, function(x){
  
  NumCoAuthors <- length(x$otherAuthor)
  coAuthors <- x$otherAuthor
  
  for(i in seq_along(coAuthors)){
    thisCoAuthor <- as.character(coAuthors[i])
    coAuthWorks <- works(thisCoAuthor)
    coAuthWorksIDs <- identifiers(coAuthWorks)
    
    coAuthorsCOAUTHORS <- sapply(coAuthWorksIDs, simplify = FALSE, function(x){
      itemRecord <- orcid_doi(x)
      allAuthors <- itemRecord[[1]]$`orcid-identifier.path`
      thisAuthor <- "0000-0001-7719-6695"
      
      otherAuthor <- allAuthors[!grepl(thisAuthor, fixed = TRUE, allAuthors)]
      if(length(otherAuthor) > 0){
        ret <- data.frame(otherAuthor, manuscript = x)
      }else{
        ret <- data.frame(otherAuthor = "noCoAuthors", manuscript = x)
      }
      
      return(ret)
      
    })
    
    coAuthorsCOAUTHORSdf <- bind_rows(coAuthorsCOAUTHORS)
    coAuthorsCOAUTHORSdf$from <- thisCoAuthor
    names(coAuthorsCOAUTHORSdf)[grepl("otherAuthor", names(coAuthorsCOAUTHORSdf))] <- "to"
    thePeople <- data.frame(allAuthors = as.character(unique(c(coAuthorsCOAUTHORSdf$to, thisCoAuthor))))
    tmpGraph <- graph_from_data_frame(coAuthorsCOAUTHORSdf,
                                      directed = FALSE)
    tmppAdj <- as_adj(tmpGraph)
  }
  
})
