CopyIndustryAboutLinkFromReferences <- function(endpoint, bot){
  # If no wikimapia link is given, check the references to see if one is there ###
  queryString = "select ?x ?reference ?IndustryAbout where {
                ?x rdf:type cat:Powerplant . 
                OPTIONAL{ ?x prop:Industry_About_link ?IndustryAbout } . 
                ?refNotes prop:Is_reference_and_notes_of ?x . 
                ?refNotes prop:Reference ?reference . 
                filter(regex(?reference, \"industryabout\", \"i\")).
              }"
  
  queryResults = SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
  df = queryResults$results
  
  df$IndustryAbout[which(is.na(df$IndustryAbout))] = ""
  
  df$x = gsub("http://enipedia.tudelft.nl/wiki/", "", df$x)
  
  # there might be multiple entries for a particular plant
  df_to_write = NULL
  plants = unique(df$x)
  for (plant in plants){
    locs = which(df$x == plant)
    # make sure that the references aren't the same as the industryabout link, and that there's something to actually update
    if(!setequal(df$reference[locs],df$IndustryAbout[locs])){
      industryAboutLinks = c(df$reference[locs], df$IndustryAbout[locs])
      industryAboutLinks = unique(sort(industryAboutLinks[which(industryAboutLinks != "")]))
      if (length(industryAboutLinks) > 0){
        industryAboutLinks = paste(industryAboutLinks, collapse=", ")
        df_to_write = rbind(df_to_write, c(plant, "PowerplantTest", industryAboutLinks))
      }
    }
  }
  
  if (!is.null(df_to_write)){
    df_to_write = as.data.frame(df_to_write)
    colnames(df_to_write) = c("x", "template", "Industry_About_link")
    errorDFEntries = writeDataFrameToPageTemplates(df_to_write, bot, editSummary="adding Industry About link found in the references")
  }
  
}