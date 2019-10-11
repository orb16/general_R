library(rorcid)
library(igraph)
library(visNetwork)
require(tidyverse)
# https://github.com/agbarnett/work-pals/blob/master/network.R
# functiosn from
#https://downwithtime.wordpress.com/2015/02/12/building-your-network-using-orcid-and-ropensci/
# get_doi <- function(x){
#   #  This pulls the DOIs out of the ORCiD record:
#   list.x <- x$'work-external-identifiers.work-external-identifier'
#
#   #  We have to catch a few objects with NULL DOI information:
#   do.call(rbind.data.frame,lapply(list.x, function(x){
#     if(length(x) == 0 | (!'DOI' %in% x[,1])){
#       data.frame(value=NA)
#     } else{
#       data.frame(value = x[which(x[,1] %in% 'DOI'),2])
#     }
#   }))
# }
#
# get_papers <- function(x){
#   all.papers <- x[[1]]$works # this is where the papers are.
#   papers <- data.frame(title = all.papers$'work-title.title.value',
#                        doi   = get_doi(all.papers))
#
#   paper.doi <- lapply(1:nrow(papers), function(x){
#     if(!is.na(papers[x,2]))return(orcid_doi(dois = papers[x,2], fuzzy = FALSE))
#     # sometimes there's no DOI
#     # if that's the case then just return NA:
#     return(NA)
#   })
#
#   your.papers <- lapply(1:length(paper.doi), function(x){
#     if(is.na(paper.doi[[x]])){
#       data.frame(doi=NA, orcid=NA, name=NA)
#     } else {
#       data.frame(doi = papers[x,2],
#                  orcid = paper.doi[[x]][[1]]$data$'orcid-identifier.path',
#                  name = paste(paper.doi[[x]][[1]]$data$'personal-details.given-names.value',
#                               paper.doi[[x]][[1]]$data$'personal-details.family-name.value',
#                               sep = ' '),
#                  stringsAsFactors = FALSE)
#     }})
#   do.call(rbind.data.frame, your.papers)
#
# }


olivia.record <- orcid_id(orcid = '0000-0001-7719-6695',
                         profile="works")

olivia.paper <- orcid_works('0000-0001-7719-6695')

olivia.paper[[1]][[1]]$`journal-title.value`
tmp <- olivia.paper[[1]][[1]][olivia.paper[[1]][[1]]$type == "journal-article", ]$`external-ids.external-id`
journalDIOs <- sapply(tmp, function(x) x[x$`external-id-type` == "doi", "external-id-value"])

peoplebyDOI <- orcid_doi(journalDIOs)

journalNames <- olivia.paper[[1]][[1]][olivia.paper[[1]][[1]]$type == "journal-article",]$title.title.value

peoplebyDOI[[1]]

mycoAuthors <- unique(do.call("c", sapply(peoplebyDOI, "[[", "orcid-identifier.path")))

mycoAuthorsByNumber <- do.call("c", sapply(peoplebyDOI, "[[", "orcid-identifier.path"))

allcolleagues <- list()

mycoAuthors_asorcids <- lapply(mycoAuthors, "orcid_id")
mycoAuthorsAsNames <- lapply(mycoAuthors_asorcids, function(x) paste(x[[1]]$name$`given-names`$value,
                                                                     x[[1]]$name$`family-name`$value))

directCoAuthorTab <- data.frame(directcoAuthor = mycoAuthors,
                                directcoAuthorName = mycoAuthorsAsNames)
for(i in 1:length(mycoAuthors)){
  allcolleagues[[i]] <- orcid_works(mycoAuthors[i])
}

listCol <- lapply(allcolleagues, function(x) x[[1]][[1]][x[[1]][[1]]$type == "journal-article", ])
coAuthorjournalDOIs <- lapply(listCol, function(x){
  listedPapers <- x$`external-ids.external-id`
  selVec <- sapply(sapply(listedPapers, "[[", "external-id-type"), function(x) "doi" %in% x)

  paperVec <-unique(toupper(sapply(listedPapers[selVec], function(x) x[1, "external-id-value"])))

  return(paperVec)
  })

length(coAuthorjournalDOIs)
names(coAuthorjournalDOIs) <- mycoAuthors

# get coauthors of co-authors

somuch <- lapply(coAuthorjournalDOIs, orcid_doi)
coAuthTocoAuth <- bind_rows(lapply(somuch, bind_rows, .id = "whichPaperID"), .id = "coAuthorsORCID")
coAuthUnique <- unique(coAuthTocoAuth$`orcid-identifier.path`)
cocoAuthors_asOrchids <- sapply(coAuthUnique, orcid_person)
cocoAuthorsAsTable <- bind_rows(lapply(cocoAuthors_asOrchids, function(x) data.frame(
  name = paste(x$name$`given-names`$value, x$name$`family-name`$value),
  orcid.identifier.path = x$name$path)))

puttingTogether <- left_join(coAuthTocoAuth %>% rename(orcid.identifier.path = `orcid-identifier.path`),
                             cocoAuthorsAsTable)




# look up

# myWorks <- works( "0000-0001-7719-6695")
# ids <- identifiers(myWorks)
#
# firstID <- orcid_doi(ids[1])
#
# tmp <- sapply(ids[myWorks$type == "journal-article"], simplify = FALSE, function(x){
#   itemRecord <- orcid_doi(x)
#   allAuthors <- itemRecord[[1]]$`orcid-identifier.path`
#   thisAuthor <- "0000-0001-7719-6695"
#
#   otherAuthor <- allAuthors[!grepl(thisAuthor, fixed = TRUE, allAuthors)]
#   if(length(otherAuthor) > 0){
#     ret <- data.frame(otherAuthor, manuscript = x)
#   }else{
#     ret <- data.frame(otherAuthor = "noCoAuthors", manuscript = x)
#   }
#
#   return(ret)
#
# })
#
#
# # get list of second order relationships
# tmp2 <- tmp[!sapply(tmp, function(x)any(x[["otherAuthor"]] == "noCoAuthors"), USE.NAMES = FALSE)]
#
# secondOrder <- lapply(tmp2, function(x){
#
#   NumCoAuthors <- length(x$otherAuthor)
#   coAuthors <- x$otherAuthor
#
#   for(i in seq_along(coAuthors)){
#     thisCoAuthor <- as.character(coAuthors[i])
#     coAuthWorks <- works(thisCoAuthor)
#     coAuthWorksIDs <- identifiers(coAuthWorks)
#
#     coAuthorsCOAUTHORS <- sapply(coAuthWorksIDs, simplify = FALSE, function(x){
#       itemRecord <- orcid_doi(x)
#       allAuthors <- itemRecord[[1]]$`orcid-identifier.path`
#       thisAuthor <- "0000-0001-7719-6695"
#
#       otherAuthor <- allAuthors[!grepl(thisAuthor, fixed = TRUE, allAuthors)]
#       if(length(otherAuthor) > 0){
#         ret <- data.frame(otherAuthor, manuscript = x)
#       }else{
#         ret <- data.frame(otherAuthor = "noCoAuthors", manuscript = x)
#       }
#
#       return(ret)
#
#     })
#
#     coAuthorsCOAUTHORSdf <- bind_rows(coAuthorsCOAUTHORS)
#     coAuthorsCOAUTHORSdf$from <- thisCoAuthor
#     names(coAuthorsCOAUTHORSdf)[grepl("otherAuthor", names(coAuthorsCOAUTHORSdf))] <- "to"
#     thePeople <- data.frame(allAuthors = as.character(unique(c(coAuthorsCOAUTHORSdf$to, thisCoAuthor))))
#     tmpGraph <- graph_from_data_frame(coAuthorsCOAUTHORSdf,
#                                       directed = FALSE)
#     tmppAdj <- as_adj(tmpGraph)
#   }
#
# })
