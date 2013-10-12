FillInMissingFuelTypeIfPrimaryFuelTypeListed <- function (endpoint, bot) {
  # fill in missing fuel type if the primary fuel type is specified
  queryString = "select ?x ?primaryFuelType where {
                  ?x rdf:type cat:Powerplant . 
                  ?x prop:Primary_fuel_type ?primaryFuelType .
                  Filter Not Exists {?x prop:Fuel_type ?fuel_type } .
                }"
  
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  df$x = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df$x))
  df$primaryFuelType = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df$primaryFuelType))
  
  if (nrow(df)> 0){
    edit_df = data.frame(page=df$x,
                         template="PowerplantTest",
                         fuel_type=df$primaryFuelType)
    errorDFEntries = writeDataFrameToPageTemplates(edit_df, bot, overWriteConflicts=TRUE, editSummary="setting missing fuel type based on primary fuel type")
  }
}
