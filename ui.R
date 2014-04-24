###############################
### Bayes Classifier - ui.R ###
###############################

library(shiny) 
library(shinyIncubator)




shinyUI(fluidPage(
  fluidRow(column(3,
    h6("Input MUG Here, You can Add/Remove Loci (A and B Required) Unlisted Possibilities Are: DRB3, DRB4, DRB5"),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("update_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='likelihood' ||input.navbar1=='SIRE_map'",uiOutput("mlt_chooser")),
    conditionalPanel(condition="input.navbar1=='individual'",uiOutput("single_chooser")),
    conditionalPanel(condition="input.navbar1=='likelihood' || input.navbar1=='SIRE_map'",uiOutput("naive_prior")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("num_show")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("address_input")),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",sliderInput("zoom_res", "Map Zoom Level",
                                                                       min=3,max=21,value=15,step=1)),
    conditionalPanel(condition="input.navbar1=='SIRE_map'",uiOutput("action_button"))
    
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",
                                  fluidPage(fluidRow(column(4,plotOutput("prior_plot")),
                                                     column(4,plotOutput("lik_plot")),
                                            column(4,plotOutput("call_plot"))),
                                            fluidRow(dataTableOutput("call_table"))),value="likelihood"),
                        tabPanel("SIRE Mapping",plotOutput("SIRE_map"),value="SIRE_map")
  ))
  )
))



    
