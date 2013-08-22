FindBrokenReferenceLinks <- function () {
  endpoint = "http://enipedia.tudelft.nl/sparql"
  queryString = "select distinct ?ref where {
  ?x prop:Reference ?ref . 
}"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  brokenLinks = c()
  count = 1
  for (ref in df$ref){
    if (!url.exists(ref, .opts = list(timeout = 30, maxredirs = 2, verbose = TRUE))){
      print(ref)
      brokenLinks = c(brokenLinks, ref)
    }
    print(count)
    count = count + 1
  }
  
  queryString = "select * where {
  ?x prop:Reference ?ref . 
  ?x prop:Is_reference_and_notes_of ?pp . 
  ?pp prop:Country ?country .
  }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  # do a second pass on the links suspected to be broken, maybe they're ok now
  for (link in brokenLinks){
    if (url.exists(link, .opts = list(timeout = 30, maxredirs = 2, verbose = TRUE))){
      print(paste("removing ", link))
      brokenLinks = brokenLinks[-which(brokenLinks == link)]
    }
  }
  
  brokenLinks = as.data.frame(brokenLinks)
  brokenLinksAndPages = merge(df, brokenLinks, by.x=c("ref"), by.y=c("brokenLinks"))
  
  # do something to show broken links by country and power plant
  brokenLinksAndPages = sqldf("select * from brokenLinksAndPages order by country, pp, ref")
  
  countries = unique(brokenLinksAndPages$country)
}