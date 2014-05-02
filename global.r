if(!require("devtools"))
  (install.packages("devtools"))
if(!require("RCurl"))
  (install.packages("RCurl"))
if(!require("rjson"))
  (install.packages("rjson"))
if(!require("shiny"))
  (install.packages("shiny"))
if(!require("plyr"))
  (install.packages("plyr"))
if(!require("ggplot2"))
  (install.packages("ggplot2"))
if(!require("shinyIncubator"))
  devtools::install_github("shiny-incubator", "rstudio")
if(!require("ggmap"))
  (install.packages("ggmap"))
if(!require("mapproj"))
  (install.packages("mapproj"))
if(!require("acs"))
  (install.packages("acs"))
if(!require("gridExtra"))
  (install.packages("gridExtra"))
if(!require("MASS"))
  (install.packages("MASS"))
if(!require("reshape2"))
  (install.packages("reshape2"))
if(!require("spdep"))
  (install.packages("spdep"))
if(!require("class"))
  (install.packages("class"))

# dids <- c(
#   52092152,
#   60837259,
#   60837259,
#   63896732,
#   64421993,
#   65991523,
#   66678640,
#   66678640,
#   75239111,
#   81842486,
#   82655309,
#   85023075,
#   86661337,
#   86661337,
#   87196465,
#   87196465,
#   88623855,
#   93880813,
#   96294244,
#   97346993)


s1 <- list(locus="A", type1="31:01", type2="66:01")
s2 <- list(locus="BPR", type1="40:02", type2="41:02")
s3 <- list(locus="C", type1="03:04", type2="17:03")
s4 <- list(locus="DRB1", type1="07:01", type2="04:04")
s5 <- list(locus="DQB1", type1="03:02", type2="02:01")
mug_ini <- list(s1, s2, s3, s4, s5)
populations_ini <- c("AAFA","AFB", "AINDI", "AMIND", "CARB", "CARHIS", "CARIBI", "FILII", "HAWI",
                     "JAPI", "KORI", "MENAFC", "MSWHIS", "NAMER", "NCHI", "SCAHIS", "SCSEAI", "VIET",
                     "CAU","HIS","NAM","AFA","API")



haplotypeImpute<-function(mug=mug_ini,populations=populations_ini[1]){
  
  host <- 'http://b1haplogic-s1:8080'
  httpHeaders <- c(Accept = "application/json",
                   'Content-Type' = 'application/json;charset=UTF-8')
  
  # Check if the webservice is available
  # when accessing /impute path, it should return a welcome message such as:
  # "Welcome to Imputation Service! (REST Version) AVAILABLE"
  main_url = paste(host, '/impute', sep='')
  available_text <- getURL(main_url)
  if(substring(available_text, 1, 7) != "Welcome") {
    print(paste("Webservice is not available at", main_url))
    quit('no')
  }
  
  # URL for retrieving haplotype pairs
  url = paste(host, '/impute/haplotype', sep='')
  
  # Prepare the request to the webservice.
  # 1. Populations
  populations <- as.list(populations)
  # 2. HLA data
  # Assemble populations and HLA data 
  hla <- list(populations=populations, hlaMUG=mug)
  # Convert the request to JSON before sending to the webservice
  request <- toJSON(hla)
  #print("REQUEST")
  #print(request)
  
  # Send it to the server
  reader <- basicTextGatherer()
  status = curlPerform(url = url,             
                       httpheader = httpHeaders,
                       postfields = request,
                       writefunction = reader$update)
  if(status == 0) {
    # Success
    response <- reader$value()
    #print("Response:")
    #print(response)
    # haplotype 
    imputedHaplotypes <- fromJSON(response)[[1]]
    size = length(imputedHaplotypes)
    options(stringsAsFactors=F)
    haplotypes = data.frame()
    #print(imputedHaplotypes)
    for(i in 1:size) {
      imputedHaplotype <- imputedHaplotypes[[i]]
      haplotypes <- rbind(haplotypes, c(imputedHaplotype$haplotype, imputedHaplotype$population, imputedHaplotype$frequency))
    }
    names(haplotypes) <- c("Haplotype", "Population", "Frequency")
    return(list("haplotypes"=haplotypes,"mug"=mug,"populations"=populations))
    #View(haplotypes)
  } else {
    print("Failed with status:")
    print(status)
  }
  
}


haplotypePairImpute<-function(mug=mug_ini,populations=populations_ini){
  host <- 'http://b1haplogic-s1:8080'
  httpHeaders <- c(Accept = "application/json",
                   'Content-Type' = 'application/json;charset=UTF-8')
  
  # Check if the webservice is available
  # when accessing /impute path, it should return a welcome message such as:
  # "Welcome to Imputation Service! (REST Version) AVAILABLE"
  main_url = paste(host, '/impute', sep='')
  available_text <- getURL(main_url)
  if(substring(available_text, 1, 7) != "Welcome") {
    print(paste("Webservice is not available at", main_url))
    quit('no')
  }
  
  # URL for retrieving haplotype pairs
  url = paste(host, '/impute/haplotype-pairs', sep='')
  
  # Prepare the request to the webservice.
  # Assemble populations and HLA data 
  hla <- list(populations=populations, hlaMUG=mug)
  # Convert the request to JSON before sending to the webservice
  request <- toJSON(hla)
  #print("REQUEST")
  #print(request)
  
  # Send it to the server
  reader <- basicTextGatherer()
  status = curlPerform(url = url,             
                       httpheader = httpHeaders,
                       postfields = request,
                       writefunction = reader$update)
  if(status == 0) {
    # Success
    response <- reader$value()
    #print("Response:")
    #print(response)
    # haplotype 
    imputedHaplotypePairs <- fromJSON(response)[[1]]
    size = length(imputedHaplotypePairs)
    options(stringsAsFactors=F)
    haplotype.pairs = data.frame()
    for(i in 1:size) {
      pairs <- imputedHaplotypePairs[i][[1]]
      haplotype1 <- pairs$haplotype1
      haplotype2 <- pairs$haplotype2
      haplo_pair <- c(as.character(haplotype1$haplotype), as.character(haplotype1$population), as.numeric(haplotype1$frequency),
                      as.character(haplotype2$haplotype), as.character(haplotype2$population), as.numeric(haplotype2$frequency), 
                      as.numeric(pairs$frequency))
      print(haplo_pair)
      haplotype.pairs <- rbind(haplotype.pairs, haplo_pair)
      
      s1 <- paste(haplotype1$haplotype, haplotype1$population, haplotype1$frequency)
      s2 <- paste(haplotype2$haplotype, haplotype2$population, haplotype2$frequency)
      s <- paste(s1, s2, pairs$frequency)
      print(s)
    }
    names(haplotype.pairs) <- c("Haplotype1", "Race1", "Frequency1", "Haplotype2", "Race2", "Frequency2", "Pair Frequency")
    return(list("haplotype_pairs"=haplotype.pairs,"mug"=mug,"populations"=populations))
    #View(haplotype.pairs)
  } else {
    print("Failed with status:")
    print(status)
  }
  
}

Census_block<-function(latitude=28.35975,longitude=-81.421988){
  host <- 'http://data.fcc.gov/api/block/find?format=json'
  url <- paste(host, paste("latitude=", latitude, sep=""), paste("longitude=", longitude, sep=""), "showall=true", sep="&")
  jsonResponse <- getURL(url)
  resultdf <- fromJSON(jsonResponse)
  return(resultdf)
}




Census_meta_data<-function(){
  host <- 'http://api.census.gov/data.json'
  url <- paste(host)
  jsonResponse <- getURL(url)
  resultdf <- fromJSON(jsonResponse)
  return(resultdf)
}


Census_pull_tract_acs<-function(FIPS="270531069001009",get=c("DP05_0032E","DP05_0072E","DP05_0052E",
                                                             "DP05_0077E","DP05_0073E","DP05_0074E",
                                                             "DP05_0075E","DP05_0076E")){
  STATE=paste0("state:",substr(FIPS,1,2))
  COUNTY=paste0("county:",substr(FIPS,3,5))
  TRACT=paste0("tract:",substr(FIPS,6,11))
  BLOCK=paste0("block+group:",substr(FIPS,12,12))
  key='key=b00c79d5be769cd3ed18d5666851b532ff290694'
  target=paste0("for=",TRACT)
  locale=paste0("in=",STATE,"+",COUNTY)
  ####format#####
  #http://api.census.gov/data/2011/acs5?get=[the data code that you found above]&for=
    #tract:[tract code or *]&in=state:[state code]+county:[county code]&key=[your key]
  
  url<-paste0('http://api.census.gov/data/2012/acs5/profile?get=',paste(get,collapse=","))
  url<-paste(url,target,locale,key,sep="&")

  jsonResponse <- getURL(url)
  #print(jsonResponse)
  resultdf <- fromJSON(jsonResponse)
  #print(resultdf)
  tmp<-data.frame(resultdf[[2]])
  rownames(tmp)<-resultdf[[1]]
  colnames(tmp)<-"Census Tract Counts"
  tmp<-data.frame(t(tmp))
  tmp[,1:8]<-as.numeric(tmp[,1:8])
  HIS<-tmp$DP05_0032E-tmp$DP05_0072E+tmp$DP05_0052E-tmp$DP05_0077E
  CAU<-tmp$DP05_0072E
  AFA<-tmp$DP05_0073E
  NAM<-tmp$DP05_0074E
  API<-tmp$DP05_0075E+tmp$DP05_0076E
  TOTAL<-HIS+CAU+AFA+NAM+API
  RACE<-data.frame(HIS,CAU,AFA,NAM,API,TOTAL)
  
  list(QUERY=resultdf,RACE=RACE)
  

}


DID_HLA_Lookup<-function(did=95963823){

host <- 'http://b1haplogic-s1:8080'
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

# URL for retrieving haplotype pairs
url = paste(host, '/hla/did/', did, sep='')

# Get Phenotype+HLA from the server
resultJSON <- getURL(url, httpheader= httpHeaders)

if(length(resultJSON) > 0) {
  result <- fromJSON(resultJSON)
  print(paste("Typings for", result$nmdpId))
  print(paste("Population:", result$population))
  searchTypings <- result$searchTypings
  mug <- list()
  for(i in 1:length(searchTypings)) {
    locus <- searchTypings[[i]]$hlaLocus
    type1 <- searchTypings[[i]]$antigen1
    if(type1 == "null") {
      type1 <- ""
    }
    type2 <- searchTypings[[i]]$antigen2
    if(type2 == "null") {
      type2 <- ""
    }
    s <- list(locus=locus, type1=type1, type2=type2)
    mug[[i]] <- s
    print(paste(locus, type1, type2, sep=","))
  }
}

return(list(result=result,mug=mug))

}


CID_HLA_Lookup<-function(cid=996913803){
host <- 'http://b1haplogic-s1:8080'
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

# URL for retrieving haplotype pairs
url = paste(host, '/hla/cid/', cid, sep='')

# Get Phenotype+HLA from the server
resultJSON <- getURL(url, httpheader= httpHeaders)

if(length(resultJSON) > 0) {
  result <- fromJSON(resultJSON)
  print(paste("Typings for", result$nmdpId))
  print(paste("Population:", result$population))
  searchTypings <- result$searchTypings
  mug <- list()
  for(i in 1:length(searchTypings)) {
    locus <- searchTypings[[i]]$hlaLocus
    type1 <- searchTypings[[i]]$antigen1
    if(type1 == "null") {
      type1 <- ""
    }
    type2 <- searchTypings[[i]]$antigen2
    if(type2 == "null") {
      type2 <- ""
    }
    s <- list(locus=locus, type1=type1, type2=type2)
    mug[[i]] <- s
    print(paste(locus, type1, type2, sep=","))
  }
}

return(list(result=result,mug=mug))

}



RID_HLA_Lookup<-function(rid=1419426){
host <- 'http://b1haplogic-s1:8080'
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

# URL for retrieving haplotype pairs
url = paste(host, '/hla/rid/', rid, sep='')

# Get Phenotype+HLA from the server
resultJSON <- getURL(url, httpheader= httpHeaders)

if(length(resultJSON) > 0) {
  result <- fromJSON(resultJSON)
  print(paste("Typings for", result$nmdpId))
  print(paste("Population:", result$population))
  searchTypings <- result$searchTypings
  mug <- list()
  for(i in 1:length(searchTypings)) {
    locus <- searchTypings[[i]]$hlaLocus
    type1 <- searchTypings[[i]]$antigen1
    if(type1 == "null") {
      type1 <- ""
    }
    type2 <- searchTypings[[i]]$antigen2
    if(type2 == "null") {
      type2 <- ""
    }
    s <- list(locus=locus, type1=type1, type2=type2)
    mug[[i]] <- s
    print(paste(locus, type1, type2, sep=","))
  }
}

return(list(result=result,mug=mug))

}




DID_SIRE_Lookup<-function(DID=095963823){

host <- 'http://b1haplogic-s1:8080'
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

# build the complete URL
url = paste(host, '/sire/did/', DID, sep='')

# Get SIRE from the server
resultJSON <- getURL(url, httpheader= httpHeaders)

if(length(resultJSON) > 0) {
  result <- fromJSON(resultJSON)
  return(result)
  #print(paste("SIRE for:", result$nmdpId))
}


}


RID_SIRE_Lookup<-function(RID=095963823){
  
  
  host <- 'http://b1haplogic-s1:8080'
  httpHeaders <- c(Accept = "application/json",
                   'Content-Type' = 'application/json;charset=UTF-8')
  
  # build the complete URL
  url = paste(host, '/sire/rid/', RID, sep='')
  
  # Get SIRE from the server
  resultJSON <- getURL(url, httpheader= httpHeaders)
  
  if(length(resultJSON) > 0) {
    result <- fromJSON(resultJSON)
  return(result)
  }
  
  
}


CID_SIRE_Lookup<-function(CID=095963823){
  
  host <- 'http://b1haplogic-s1:8080'
  httpHeaders <- c(Accept = "application/json",
                   'Content-Type' = 'application/json;charset=UTF-8')
  
  # build the complete URL
  url = paste(host, '/sire/cid/', CID, sep='')
  
  # Get SIRE from the server
  resultJSON <- getURL(url, httpheader= httpHeaders)
  
  if(length(resultJSON) > 0) {
    result <- fromJSON(resultJSON)
    return(result)
  }
  
  
}



SIRE_map<-function(detailRace = "WEURO",
                   ethnicity = "NHIS"){

host <- 'http://b1haplogic-s1:8080'
httpHeaders <- c('Accept' = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')


url <- paste(host, '/population/search?detailRace=', detailRace, '&ethnicity=', ethnicity, sep='')

# Get Phenotype+HLA from the server
resultJSON <- getURL(url, httpheader= httpHeaders)
if(nchar(resultJSON) > 0) {
  result <- fromJSON(resultJSON)
  return(result)
}


}










#' @import shiny


#' Matrix input
#' 
#' Creates an adjustable-length matrix input.
#' 
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control.
#' @param data The initial values to use for the matrix.
#' 
#' @export
matrixCustom <- function(inputId, label, data) {
  addResourcePath(
    prefix='tableinput', 
    directoryPath=system.file('tableinput', 
                              package='shinyIncubator'))
  
  tagList(
    singleton(
      tags$head(
        tags$link(rel = 'stylesheet',
                  type = 'text/css',
                  href = 'tableinput/tableinput.css'),
        tags$script(src = 'tableinput/tableinput.js')
      )
    ),
    
    tags$div(
      class = 'control-group tableinput-container',
      tags$label(
        class = "control-label",
        label,
        tags$div(
          class = 'tableinput-buttons',
          tags$button(
            type = 'button', class = 'btn btn-mini tableinput-settings hide',
            tags$i(class = 'icon-cog')
          ),
          HTML('<a href="#" class="tableinput-plusrow"><i class="icon-plus-sign"></i></a>'),
          HTML('<a href="#" class="tableinput-minusrow"><i class="icon-minus-sign"></i></a>')
        )
      ),
      tags$table(
        id = inputId,
        class = 'tableinput data table table-bordered table-condensed',
        tags$colgroup(
          lapply(names(data), function(name) {
            tags$col('data-name' = name,
                     'data-field' = name,
                     'data-type' = 'character')
          })
        ),
        tags$thead(
          class = 'hide',
          tags$tr(
            lapply(names(data), function(name) {
              tags$th(name)
            })
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              lapply(names(data), function(name) {
                tags$td(
                  div(tabindex=0, as.character(data[i,name]))
                )
              })
            )
          })
        )
      ),
      tags$div(
        class = 'tableinput-editor modal hide fade',
        tags$div(
          class = 'modal-header',
          HTML('<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>'),
          tags$h3(label)
        ),
        tags$div(
          class = 'modal-body',
          
          HTML('
               <form class="form-horizontal">
               <div class="control-group">
               <label class="control-label">Rows</label>
               <div class="controls">
               <input type="number" class="tableinput-rowcount">
               </div>
               </div>
               <div class="control-group">
               <label class="control-label">Columns</label>
               <div class="controls">
               <input type="number" class="tableinput-colcount">
               </div>
               </div>
               </form>'
          )
        ),
        tags$div(
          class = 'modal-footer',
          tags$a(href = '#', class = 'btn btn-primary tableinput-edit', 'OK'),
          tags$a(href = '#',
                 class = 'btn',
                 'data-dismiss' = 'modal',
                 'aria-hidden' = 'true',
                 'Cancel')
        )
      )
    )
  )
}

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)









