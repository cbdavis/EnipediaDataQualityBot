CopyWikimapiaLinkFromReferences <- function(endpoint, bot){
  # If no wikimapia link is given, check the references to see if one is there ###
  queryString = "select ?x ?Wikimapia_link where {
                ?x rdf:type cat:Powerplant . 
                OPTIONAL{?x prop:Wikimapia_link ?wikimapia } . 
                ?ref prop:Is_reference_and_notes_of ?x . 
                ?ref prop:Reference ?Wikimapia_link . 
                filter(regex(?Wikimapia_link, \"wikimapia\", \"i\")) . 
                filter(!bound(?wikimapia))
              }"
  
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  if (nrow(df) > 0){ #only update if something has been found  
    df$x = gsub("http://enipedia.tudelft.nl/wiki/", "", df$x)
    df$template = "PowerplantTest"
    df = df[,c("x", "template", "Wikimapia_link")]
    errorDFEntries = writeDataFrameToPageTemplates(df, bot, editSummary="adding Wikimapia link found in the references")
  }
}