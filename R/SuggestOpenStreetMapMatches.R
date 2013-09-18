# TODO this code is in progress and doesn't work completely out of the box

# extract the latest OpenStreetMap generator data from Enipedia 
# and run a python script that extracts the data in CSV

path <- paste(system.file(package="EnipediaDataQualityBot"), "OSM_generators_to_CSV.py", sep="/")
command <- paste("python", path)
response <- system(command, intern=T)

#never ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL)
library(EnipediaOpenrefineReconcile)
library(rgdal)
library(RSemanticMediaWikiBot)
# sourced from http://www.marineregions.org/downloads.php
# TODO fix path
borders = readOGR("EEZ_land_v1", "EEZ_land_v1")


nodes_and_tags = read.csv("nodes_and_tags.csv", sep="\t", header=TRUE, comment.char="", quote="")
relations_and_nodes = read.csv("relations_and_nodes.csv", sep="\t", header=TRUE, comment.char="", quote="")
relations_and_tags = read.csv("relations_and_tags.csv", sep="\t", header=TRUE, comment.char="", quote="")
relations_and_ways = read.csv("relations_and_ways.csv", sep="\t", header=TRUE, comment.char="", quote="")
ways_and_nodes = read.csv("ways_and_nodes.csv", sep="\t", header=TRUE, comment.char="", quote="")
ways_and_tags = read.csv("ways_and_tags.csv", sep="\t", header=TRUE, comment.char="", quote="")

# both ways and relations need to have coordinates set - take the average of all the nodes

sqldf("create index nodes_and_tags_objectID on nodes_and_tags(objectID)")
sqldf("create index ways_and_nodes_node on ways_and_nodes(node)")
sqldf("create index ways_and_nodes_objectID on ways_and_nodes(objectID)")

ways_coordinates = sqldf("select ways_and_nodes.objectID AS objectID, 
                         AVG(nodes_and_tags.lat) AS lat, 
                         AVG(nodes_and_tags.lon) AS lon FROM ways_and_nodes 
                         JOIN nodes_and_tags ON nodes_and_tags.objectID = ways_and_nodes.node 
                         GROUP BY ways_and_nodes.objectID")

# get all the coordinates for the ways (and their nodes)
# and then get all the coordiantes for the nodes
# then average them all together
relations_coordinates = sqldf("select objectID, AVG(node_lat) AS lat, AVG(node_lon) AS lon FROM
                              (select relations_and_ways.objectID as objectID, 
                              nodes_and_tags.lat AS node_lat, 
                              nodes_and_tags.lon AS node_lon 
                              FROM relations_and_ways 
                              JOIN ways_and_nodes ON relations_and_ways.way = ways_and_nodes.objectID 
                              JOIN nodes_and_tags ON nodes_and_tags.objectID = ways_and_nodes.node
                              UNION 
                              select relations_and_nodes.objectID as objectID, 
                              nodes_and_tags.lat AS node_lat, 
                              nodes_and_tags.lon AS node_lon 
                              FROM relations_and_nodes 
                              JOIN nodes_and_tags ON nodes_and_tags.objectID = relations_and_nodes.node)
                              GROUP by objectID")

# merge the coordinates back in
relations_and_tags = sqldf("select relations_and_tags.*, 
                           relations_coordinates.lat AS lat, 
                           relations_coordinates.lon AS lon 
                           FROM relations_and_tags 
                           JOIN relations_coordinates 
                           ON relations_coordinates.objectID = relations_and_tags.objectID")

# TODO check if nodes/relations/ways have different wikipedia links specified, other consistency checks

ways_and_tags = sqldf("select ways_and_tags.*, 
                      ways_coordinates.lat AS lat, 
                      ways_coordinates.lon AS lon 
                      from ways_and_tags
                      JOIN ways_coordinates 
                      ON ways_coordinates.objectID = ways_and_tags.objectID")

nodes_and_tags$objectType = "node"
ways_and_tags$objectType = "way"
relations_and_tags$objectType = "relation"

all_osm_data = rbind(nodes_and_tags, ways_and_tags, relations_and_tags)
all_osm_data$objectURL = paste("http://www.openstreetmap.org/browse/",all_osm_data$objectType,"/", all_osm_data$objectID, sep="")

# now figure out which country each of the objects are in
coordinates(all_osm_data) <- c("lon", "lat")
proj4string(all_osm_data) <- proj4string(borders)

# takes under a minute for 300,000 objects
all_osm_data$country_ISO <- over(all_osm_data, borders)$ISO_3digit

endpoint = "http://enipedia.tudelft.nl/sparql"
queryString = "select * where {
?country rdfs:label ?countryName . 
?country prop:ISO_3166-1_Alpha-3_code ?isoCountry .
}"

queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
countryCodes = queryResults$results

# convert back to data frame
all_osm_data = as.data.frame(all_osm_data)
all_osm_data = merge(all_osm_data, countryCodes, by.x=c("country_ISO"), by.y=c("isoCountry"), all.x=TRUE)

# now we can start going through country by country and suggesting matches

# Figure out which power plants can be matched to which OpenStreetMap entries
countries = unique(all_osm_data$countryName)
countries = countries[which(is.na(countries) == FALSE)]

allCandidatesInfo = c()

for (country in countries){
  print(country)
  # match on things that at least have a name
  locs = which(all_osm_data$countryName == country & all_osm_data$soup != "")
  osm_df = all_osm_data[locs,]
  # the python script doesn't completely clean the soup
  osm_df$soup = fingerprint(osm_df$soup)
  
  eni_pp = retrieveCountryDataFromEnipedia(country)
  
  if (nrow(eni_pp) > 0){
    # let there be soup
    eni_pp$soup = fingerprint(paste(eni_pp$CleanedPlantName, eni_pp$CleanedCityName, eni_pp$CleanedOwnerName, eni_pp$CleanedStateName))
  } else {
    warn(paste("No enipedia data found for", country))
  }
  
  # go go gadget matching
  selfInformation = calculateSelfInformation(eni_pp$soup, osm_df$soup)
  if (is.null(selfInformation) == FALSE){ # check that some matching tokens were actually found
    entity1 = selfInformation$entity1
    entity2 = selfInformation$entity2
    
    indices = c(1:length(entity1))
    
    matchingTokens = unlist(lapply(indices, 
                                   function(x){paste(intersect(strsplit(eni_pp$soup[entity1[x]], " ")[[1]], 
                                                               strsplit(osm_df$soup[entity2[x]], " ")[[1]]), 
                                                     collapse=", ")}))
    
    #TODO remove matches that we already know about
    
    candidatesInfo = data.frame(scores = selfInformation$totalSelfInfo,
                                numberOfMatchingTokens = selfInformation$numIntersectingTokens, 
                                matchingTokens = matchingTokens, 
                                eni_name = eni_pp$name[entity1], 
                                osm_name = osm_df$name[entity2], 
                                eni_soup = eni_pp$soup[entity1], 
                                osm_soup = osm_df$soup[entity2], 
                                osm_id = osm_df$objectURL[entity2])
    
    alreadyMatchedOSMLinks = unique(eni_pp$osmLink[which(eni_pp$osmLink != "")])
    alreadyMatchedEnipediaLinks = unique(eni_pp$x[which(eni_pp$osmLink != "")])
    
    locsToRemove = union(which(osm_df$objectURL[entity2] %in% alreadyMatchedOSMLinks), 
                         which(eni_pp$x[entity1] %in% alreadyMatchedEnipediaLinks))
    if (length(locsToRemove) > 0){
      candidatesInfo = candidatesInfo[-locsToRemove,]
    }
    
    # only keep the best 5%
    #cutoffScore = sort(candidatesInfo$scores, decreasing=TRUE)[round(nrow(candidatesInfo)/20)]
    #locs = which(candidatesInfo$scores >= cutoffScore)
    candidatesInfo = candidatesInfo[locs,]
    
    if (nrow(candidatesInfo) > 0){
      
      # make a link for the enipedia page
      candidatesInfo$eni_name = paste("[[", candidatesInfo$eni_name, "]]", sep="")
      # get the wiki table syntax
      wikiTable = getWikiTableTextForDataFrame(candidatesInfo)
      #TODO fill these in based on your own configuration
      
      #bot = initializeBot(apiURL) #initialize the bot
      #login(username, password, bot) #login to the wiki
      
      #edit(title="User:ChrisDavis/Test Automated Matching OSM China",
      #     text=wikiTable,
      #     bot,
      #     summary="working on bot code to generate matching tables for various data sets")
      allCandidatesInfo = rbind(allCandidatesInfo, candidatesInfo)
    }
  }
}
