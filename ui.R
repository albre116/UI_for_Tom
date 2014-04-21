###############################
### Bayes Classifier - ui.R ###
###############################

library(shiny) 
library(shinyIncubator)


shinyUI(fluidPage(
  fluidRow(column(3,
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_mug")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_multi")),
    conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_single"))
  ),column(9,navbarPage(title="",id="navbar1",
                         tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
                         tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
                         tabPanel("Likelihood of Data",plotOutput("graph_lik"),value="likelihood")                
  ))
  )
))



# shinyUI(fluidPage(
#   sidebarLayout(sidebarPanel(
#     conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_mug")),
#     conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_multi")),
#     conditionalPanel(condition="input.navbar1=='pairs' || input.navbar1=='individual'",uiOutput("table_pop_single"))
#     ),mainPanel(navbarPage(title="",id="navbar1",
#                            tabPanel("PairWise Haplotypes",dataTableOutput("haplotypePairs"),value="pairs"),
#                            tabPanel("Individual Haplotypes",dataTableOutput("haplotypes"),value="individual"),
#                            tabPanel("Likelihood of Data",plotOutput("graph_lik"),value="likelihood")                
#                            ))
#   )
# ))

    
