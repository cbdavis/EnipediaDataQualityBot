FindURLForCompanyWebsiteBasedOnPowerplantReferences <- function () {
  # figure out possible URLs for company websites where none is listed, but the owner has websites that contain references
  queryString = "select distinct ?x ?referenceURL where {
                  ?x rdf:type cat:Energy_Company . 
                  OPTIONAL{?x prop:Website ?url } .
                  FILTER(!BOUND(?url)) . 
                  ?plant prop:Ownercompany ?x . 
                  ?ref prop:Is_reference_and_notes_of ?plant . 
                  ?ref prop:Reference ?referenceURL . 
                  FILTER(!regex(?referenceURL, \"wikipedia|globalenergyobservatory|bundesnetzagentur|enipedia|wikimapia|industryabout|industcards\", \"i\"))
                } order by ?x"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  m <- gregexpr("http://[-[:alnum:]\\.]+", df$referenceURL)
  df$suggestedURL = unlist(lapply(regmatches(df$referenceURL, m), function(x){x[1]}))
  #remove duplcates
  df$referenceURL = NULL
  df = df[!duplicated(df),]
  write.table(df, "possibleCompanyWebsites.csv", sep="\t", row.names=FALSE)
  
  ############### TODO need to write some methods that write back data that has to be filtered by hand ###############
  
  #okToRun = FALSE
  #if (okToRun==TRUE){
  #  df = read.table("possibleCompanyWebsites.csv", header=TRUE)
  #  writeDataFrameToPageTemplates(df, bot, "adding company website based on hand-filtered list generated from references in use by owned power plants")
  #}
}
