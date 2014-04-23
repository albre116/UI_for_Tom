# 'RCurl' package is used for accessing the webservice.
# Install RCurl package as:
# install.packages('RCurl')
library('RCurl')

# 'rsjon' package is used for converting between R and JSON structures. 
# Install rjson package as:
# install.packages('rjson')
library('rjson')

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
