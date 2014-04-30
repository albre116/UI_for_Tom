###############################
### Bayes Classifier - ui.R ###
###############################

library(shiny) 
library(shinyIncubator)




shinyUI(fluidPage(
  fluidRow(column(3,
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",
                     h6("Automatic HLA lookup based upon DID, RID, or CID"),
                     numericInput("ID","Type ID Here",value=NA),
                     selectInput("type","Type of Lookup",c("DID","RID","CID")),
                     actionButton("lookup", "Go Lookup Typing"),
                     h6("Input MUG Here, You can Add/Remove Loci (A and B Required)"),uiOutput("table_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("update_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='likelihood' ||input.navbar1=='SIRE_map'",uiOutput("mlt_chooser")),
    conditionalPanel(condition="input.navbar1=='individual'",uiOutput("single_chooser")),
    conditionalPanel(condition="input.navbar1=='likelihood' || input.navbar1=='SIRE_map'",uiOutput("naive_prior")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("num_show")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("address_input")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",sliderInput("zoom_res", "Map Zoom Level",
                                                                       min=3,max=21,value=13,step=1)),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("action_button")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",sliderInput("grid", "Grid Segments",
                                                                       min=2,max=20,value=10,step=1),checkboxInput("contour","Points instead of Contour?",value=TRUE))
    
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",
                                  fluidPage(fluidRow(column(4,plotOutput("prior_plot")),
                                                     column(4,plotOutput("lik_plot")),
                                            column(4,plotOutput("call_plot"))),
                                            fluidRow(dataTableOutput("call_table"))),value="likelihood"),
                        tabPanel("SIRE Mapping",fluidPage(
                                 fluidRow(h4("Probability of Race Designation given location [R|X,Y]"),plotOutput("SIRE_map_raw")),
                                 h4("Density Distribution of Individuals for a Given Race [X,Y|R]"),fluidRow(plotOutput("SIRE_map_contour")),
                                 fluidRow(dataTableOutput("census"))
                                 ), conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
                                                     id='progressIndicator',
                                                     "HI I'M IN PROGRESS",
                                                     div(id='progress',includeHTML("C:/svn/malbrech/Shiny_Bayes_Classifier_svn/www/timer.js")
                                 ),
                                 tags$head(tags$style(type="text/css",
                                                      '#progressIndicator {',
                                                      '  position: fixed; top: 8px; right: 8px; width: 200px; height: 50px;',
                                                      '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
                                                      '}'
                                 ))),
                                 
                                 
                                 value="SIRE_map")
  ))
  )
))



    
