####### fix wikimapia links, find the name of the object #######
ReformatWikimapiaLinkToShowName <- function (endpoint, bot) {
  queryString = "select * where {
                  ?x prop:Wikimapia_link ?wikimapiaLink . 
                }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  # pass the URL to Wikimapia, and see where the redirect leads
  urlIndicesToFix = c()
  df$newURL = ""
  
  instancesToFix = which(grepl("lat=", df$wikimapiaLink))
  df = df[instancesToFix,]
  
  i = 0
  for (wikimapiaLink in df$wikimapiaLink){
    i = i + 1
    print(i)
    m <- gregexpr("/[0-9]+/", wikimapiaLink)
    wikimapiaID = gsub("/", "", unlist(regmatches(wikimapiaLink, m)))
    
    if (length(wikimapiaID) > 0){
      # see if the link needs to be fixed
      if (grepl("&show=", wikimapiaLink) == TRUE){
        url = paste("http://www.wikimapia.org/", wikimapiaID, sep="")
        response = getURL(url, .opts=curlOptions(followlocation=TRUE))
        doc = htmlParse(response, useInternalNodes=TRUE)
        # the permalinks aren't always specified consistently
        permalink <- unlist(getNodeSet(doc, "//a[@title='Permalink to this place' or @class='permalink' or @class='btn permalink']/@href"))[[1]]
        if (is.null(permalink)){ # try another method
          permalink <- unlist(getNodeSet(doc, "//meta[@name='twitter:url']/@content"))[[1]]
        }
        if (!is.null(permalink)){
          df$newURL[i] = permalink
          urlIndicesToFix = c(urlIndicesToFix, i)
        } else { # else page probably couldn't be downloaded, try again later
          warning(paste("Could not find the permalink for the Wikimapia page", url))  
        }
        Sys.sleep(5)
      }
    }
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
