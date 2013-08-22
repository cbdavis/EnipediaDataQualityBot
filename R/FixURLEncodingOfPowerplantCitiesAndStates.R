FixURLEncodingOfPowerplantCitiesAndStates <- function (endpoint, bot) {
  # fix up state and city encoding - "-2D" occasionally shows up
  queryString = "select distinct(?x) where {
                  ?x rdf:type cat:Powerplant .
                  ?x ?y ?z . 
                  FILTER(regex(?z, \"-2D\", \"i\")) . 
                  }"
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  df$x = gsub("http://enipedia.tudelft.nl/wiki/", "", df$x)
  for (page in df$x){
    pageText = read(title=page, bot) 
    edit(title=page, 
         text=pageText, 
         bot, 
         summary="forcing update of to fix url encoding of state")
  }
}