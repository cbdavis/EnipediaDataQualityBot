MoveMisplacedWikimapiaLinks <- function (endpoint, bot) {
  
  # fix links where wikimapia is listed instead of wikipedia
  queryString = "select * where {
                  ?x rdf:type cat:Powerplant . 
                  ?x prop:Wikipedia_page ?wikiPage . 
                  FILTER(regex(?wikiPage, \"wikimapia\", \"i\"))
                }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  df$x = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df$x))
  
  if (nrow(df)> 0){
    for (i in c(1:nrow(df))){
      template = getTemplateByName("PowerplantTest", df$x[i], bot)[[1]]
      template$data$Wikimapia_link = template$data$Wikipedia_Page
      template$data$Wikipedia_Page = NULL
      message = "Moving Wikimapia link from the Wikipedia_page field to Wikimapia_link field"
      print(paste("fixing ", df$x[i], ", ", message))
      writeTemplateToPage(template, bot, editSummary=message)
    }
  }
  
}