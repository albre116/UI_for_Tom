###############################
### Bayes Classifier - ui.R ###
###############################

library(shiny) 
library(shinyIncubator)




shinyUI(fluidPage(
  fluidRow(column(3,
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("mlt_chooser")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("single_chooser")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_multi")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_single")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("naive_prior"))
    
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",
                                  fluidPage(fluidRow(column(6,plotOutput("graph_lik")),
                                            column(6,plotOutput("prior_plot")))),value="likelihood")                
  ))
  )
))



    
