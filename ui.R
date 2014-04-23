###############################
### Bayes Classifier - ui.R ###
###############################

library(shiny) 
library(shinyIncubator)




shinyUI(fluidPage(
  fluidRow(column(3,
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual' || input.navbar1=='likelihood'",uiOutput("table_mug")),
    conditionalPanel(condition="input.navbar1=='pairs'",uiOutput("mlt_chooser")),
    conditionalPanel(condition="input.navbar1=='individual'",uiOutput("single_chooser")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("naive_prior")),
    conditionalPanel(condition="input.navbar1=='likelihood'",uiOutput("num_show"))
    
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",
                                  fluidPage(fluidRow(column(4,plotOutput("prior_plot")),
                                                     column(4,plotOutput("lik_plot")),
                                            column(4,plotOutput("call_plot"))),
                                            fluidRow(dataTableOutput("call_table"))),value="likelihood")                
  ))
  )
))



    
