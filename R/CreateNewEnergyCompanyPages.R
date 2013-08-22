CreateNewEnergyCompanyPages <- function (endpoint, bot) {
  
  # create pages for energy companies with at least two power plants
  queryString = "select ?owner count(?owner) as ?ownerCount where {
    ?x ?prop ?owner . 
    FILTER(?prop = prop:Ownercompany || ?prop = prop:Operator) . 
    Filter Not Exists {?owner a cat:Energy_Company} .
    } group by ?owner order by DESC(?ownerCount)"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  pagesToCreate = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df[which(df$ownerCount > 1),1]))
  #pagesToCreate = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", df[,1]))
  
  for (pageToCreate in pagesToCreate){
    print(pageToCreate)
    edit(pageToCreate, text="{{EnergyCompany}}", bot, summary="New energy company page")
  }
  
  # create pages for energy companies that are the subsidiary of another company
  queryString = "select * where {
                  ?x prop:Subsidiary ?subsidiary . 
                  Filter Not Exists {?subsidiary a cat:Energy_Company} .
                  Filter Not Exists {?subsidiary a cat:Powerplant} .
                }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  pagesToCreate = unique(df$subsidiary)
  pagesToCreate = gsub("http://enipedia.tudelft.nl/wiki/", "", pagesToCreate)
  for (pageToCreate in pagesToCreate){
    print(pageToCreate)
    edit(pageToCreate, text="{{EnergyCompany}}", bot, summary="New energy company page")
  }
}
