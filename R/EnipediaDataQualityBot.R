#never ever convert strings to factors
options(stringsAsFactors = FALSE)

library(RSemanticMediaWikiBot)
library(SPARQL)
library(rgdal) # used to find if powerplants are within the shapefiles of their stated countries
library(reshape2) # colsplit
library(sqldf)

# this is the main function
EnipediaDataQualityBot <- function (apiURL, username, password, endpoint) {
  bot = initializeBot(apiURL)
  login(username, password, bot)
  
  # new page if at least two power plants reference it
  # also new page if it is the subsidiary of another company
  CreateNewEnergyCompanyPages(endpoint, bot)
  
  # remove an energy company page if nothing links to it
  RemoveOldEnergyCompanyPages(endpoint, bot)
  
  # if there is only one fuel type, then we can assume that it's also the primary fuel type
  FillInMissingPrimaryFuelTypeIfOnlySingleFuelTypeListed(endpoint, bot)
  
  # The Wikimapia link sometimes is in the Wikipedia field and needs to be moved
  MoveMisplacedWikimapiaLinks(endpoint, bot)
  
  # fix up state and city encoding - "-2D" occasionally shows up
  FixURLEncodingOfPowerplantCitiesAndStates(endpoint, bot)
  
  # If a company page does not have a website listed, try to find it among with power plants that it owns
  FindURLForCompanyWebsiteBasedOnPowerplantReferences()
  
  # If no wikimapia link is given, check the references to see if one is there
  CopyWikimapiaLinkFromReferences(endpoint, bot)
  
  # fix wikimapia links, find the name of the object
  ReformatWikimapiaLinkToShowName(endpoint, bot)
  
  # TODO This code works, but doesn't write back to the wiki yet
  #FindBrokenReferenceLinks()
  
  # Using shapefiles of Economic Exclusive Zones, find all power plants whose coordinates
  # are outside of the country that they're stated as being in.
  FindPowerplantsOutsideOfStatedCountries(endpoint, bot)
}