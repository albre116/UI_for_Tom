###############################
### Bayes Classifier - ui.R ###
###############################






shinyUI(fluidPage(
  fluidRow(column(3,
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual' || input.navbar1=='likelihood'",
                     h6("Automatic HLA lookup based upon DID, RID, or CID"),
                     numericInput("ID","Type ID Here",value=NA),
                     selectInput("type","Type of Lookup",c("DID","RID","CID")),
                     actionButton("lookup", "Go Lookup Typing"),
                     h6("Input MUG Here, You can Add/Remove Loci (A and B Required)"),uiOutput("table_mug"),tableOutput("db_race")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("update_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='likelihood' ||input.navbar1=='SIRE_map'",uiOutput("mlt_chooser")),
    conditionalPanel(condition="input.navbar1=='individual'",uiOutput("single_chooser")),
    conditionalPanel(condition="input.navbar1=='likelihood' || input.navbar1=='SIRE_map'",uiOutput("naive_prior")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",actionButton("update_prior","Update Prior with Census?")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("num_show")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("address_input")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",sliderInput("zoom_res", "Map Zoom Level",
                                                                       min=3,max=21,value=13,step=1)),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("action_button")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("census_prior")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",sliderInput("grid", "Grid Segments",
                                                                       min=2,max=20,value=10,step=1),
                     checkboxInput("contour","Points instead of Contour?",value=TRUE),
                     checkboxInput("census_prior","Use Census Prior?",value=FALSE),
                     numericInput("k_neigh","How many Nearest Neighbors?",10),
                     checkboxInput("show_support","Show K-Nearest Neighbors",value=FALSE),
                     actionButton("update_knn", "Update K-Nearest Neighbors"),
                     numericInput("dir_prior","What Pseudo Counts for Centered Dirichlet Prior?",10),
                     actionButton("update_dir", "Update Prior")
                     )
    
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",
                                  fluidPage(fluidRow(column(12,showOutput("prior_plot", "highcharts"))),
                                            fluidRow(dataTableOutput("call_table"))),value="likelihood"),
                        tabPanel("SIRE Mapping",fluidPage(
                                 fluidRow(showOutput("class_call", "highcharts")),
                                 fluidRow(h4("Probability of Race Designation given location [R|X,Y]"),plotOutput("SIRE_map_raw")),
                                 h4("Density Distribution of Individuals for a Given Race [X,Y|R]"),fluidRow(plotOutput("SIRE_map_contour")),
                                 fluidRow(dataTableOutput("census"))
                                 ), conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
                                                     id='progressIndicator',
                                                     "HI I'M IN PROGRESS",
                                                     div(id='progress',includeHTML("www/timer.js")
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



    
