FillInMissingPrimaryFuelTypeIfOnlySingleFuelTypeListed <- function (endpoint, bot) {
  # fill in missing primary fuel types if there is only a single fuel type specified
  queryString = "select ?x ?fuelType where {
                    ?x prop:Fuel_type ?fuelType . 
                  {
                    select ?x count(?fuelType) as ?fuelCount where {
                      ?x rdf:type cat:Powerplant . 
                      ?x prop:Fuel_type ?fuelType . 
                      OPTIONAL{?x prop:Primary_fuel_type ?primaryFuelType} . 
                      FILTER(!BOUND(?primaryFuelType)) . 
                    } group by ?x 
                  }
                    FILTER(?fuelCount = 1) . 
                  }"
  
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  df$x = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df$x))
  df$fuelType = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df$fuelType))
  
  if (nrow(df) > 0){
    for (i in c(1:nrow(df))){
      template = getTemplateByName("PowerplantTest", df$x[i], bot)[[1]]
      template$data$primary_fuel_type = df$fuelType[i]
      message = "setting primary fuel type based on only fuel type specified"
      print(paste("fixing ", df$x[i], ", ", message))
      writeTemplateToPage(template, bot, editSummary=message)
    }
  }
}
