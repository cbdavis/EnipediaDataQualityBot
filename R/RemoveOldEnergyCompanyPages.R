RemoveOldEnergyCompanyPages <- function (endpoint, bot) {
  # remove empty company pages, but check first that there's no text on them
  queryString = "select ?owner where {
  ?owner a cat:Energy_Company . 
  Filter Not Exists {?pp prop:Ownercompany ?owner . } .
  Filter Not Exists {?pp prop:Operator ?owner . } .
  Filter Not Exists {?x prop:Subsidiary ?owner . } .
  }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  pagesToDelete = gsub("_", " ", gsub("http://enipedia.tudelft.nl/wiki/", "", queryResults$results$owner))
  dontDeleteMe = c()
  for (pageToDelete in pagesToDelete){
    pageText = read(pageToDelete, bot)
    if (pageText != "{{EnergyCompany}}"){
      dontDeleteMe = c(dontDeleteMe, pageToDelete)
    } else {
      delete(pageToDelete, bot, reason="Energy Company with no power plants associated with it.  Page consisted of only EnergyCompany template", minor=FALSE)
    }
  }
}