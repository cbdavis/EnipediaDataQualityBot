FindPowerplantsOutsideOfStatedCountries <- function (endpoint) {
  ################ Check if power plants are outside of their stated countries ################
  
  # A buffer can be used to deal with power plants that might be on borders or the ocean
  # http://www.inside-r.org/packages/cran/rgeos/docs/gBuffer
  
  # sourced from http://www.marineregions.org/downloads.php
  # make sure that the EEZ_land_v1 folder is in the working directory
  borders = readOGR("EEZ_land_v1", "EEZ_land_v1")
  
  queryString = "select * where {
                  ?x rdf:type cat:Powerplant . 
                  ?x prop:Point ?point . 
                  ?x prop:Country ?country . 
                  ?country rdfs:label ?countryName . 
                  ?country prop:ISO_3166-1_Alpha-3_code ?isoCountry .
                }"
  
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  coords = colsplit(df$point, ",", names=c("lat", "lon"))
  df$lat = coords$lat
  df$lon = coords$lon
  
  coordinates(df) <- c("lon", "lat")
  proj4string(df) <- proj4string(borders)
  
  # figure out which country this power plant should be in based on the shapefile
  df@data$country_ISO_Shapefile <- over(df, borders)$ISO_3digit
  
  # convert back to data frame
  df = as.data.frame(df)
  
  # ok, who's in the wrong country
  locs = which(df$isoCountry != df$country_ISO_Shapefile)
  
  # Do something to write this to the wiki
  countryCodes = read.table("Enipedia_Country_ISO_Codes.csv", sep="\t", header=TRUE)
  
  df = sqldf("select *, countryCodes.name as countryNameFromShapefile from df JOIN countryCodes ON countryCodes.code == df.country_ISO_Shapefile")
  
  df[locs, c("countryName", "countryNameFromShapefile")]
}
