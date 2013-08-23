####### fix wikimapia links, find the name of the object #######
ReformatWikimapiaLinkToShowName <- function (endpoint, bot) {
  queryString = "select * where {
                  ?x prop:Wikimapia_link ?wikimapiaLink . 
                }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  test = df$wikimapiaLink
  m <- gregexpr("/[0-9]+/", test)
  wikimapiaIdentifiers = gsub("/", "", unlist(regmatches(test, m)))
  
  # pass the URL to Wikimapia, and see where the redirect leads
  urlIndicesToFix = c()
  df$newURL = ""
  i = 1
  for (wikimapiaID in wikimapiaIdentifiers){
    # see if the link needs to be fixed
    if (grepl("&show=", df$wikimapiaLink[i]) == TRUE){
      urlIndicesToFix = c(urlIndicesToFix, i)
      url = paste("http://www.wikimapia.org/", wikimapiaID, sep="")
      response = getURL(url, .opts=curlOptions(followlocation=TRUE))
      doc = htmlParse(response, useInternalNodes=TRUE)
      # the permalinks aren't always specified in consistently
      permalink <- unlist(getNodeSet(doc, "//a[@title='Permalink to this place' or @class='permalink']/@href"))[[1]]
      df$newURL[i] = permalink
      Sys.sleep(5)
    }
    i = i + 1
  }
  
  df$x = gsub("http://enipedia.tudelft.nl/wiki/", "", df$x)
  df$wikimapiaLink = NULL
  colnames(df) = c("page", "Wikimapia_link")
  df$template = "PowerplantTest"
  df = df[c("page", "template", "Wikimapia_link")]
  if (nrow(df[urlIndicesToFix,]) > 0){ #only update if there are links to fix
    errorDFEntries = writeDataFrameToPageTemplates(df[urlIndicesToFix,], bot, editSummary="reformatting Wikimapia link")
  }
}
