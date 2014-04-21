####################################
#### Bayes Classifier - server.R ###
#### Mark Albrecht               ###
####################################
library(shiny)
library(plyr)
library(ggplot2)
library(shinyIncubator)

###initialize values in array
test<-data.frame(c("10/24/2014","10/22/12"),c(1,2))
init<-haplotypePairImpute()###call the function to get the HLA codes
state_mug<-rbind(data.frame(init$mug[1]),
                 data.frame(init$mug[2]),
                 data.frame(init$mug[3]),
                 data.frame(init$mug[4]),
                 data.frame(init$mug[5]))
state_pop<-data.frame(init$populations)

shinyServer(function(input, output, session){ # pass in a session argument
  
  output$table_mug<-renderUI({
    matrixCustom('mug', 'MUG Typing', state_mug)
    })
  
  output$table_pop_multi<-renderUI({
    matrixCustom('pop_multi', 'Multi Race Haplotype Pairs', state_pop)
    })
  
  
  output$table_pop_single<-renderUI({
    matrixCustom('pop_single', 'Single Race Haplotype List', state_pop[1,1,drop=F])
    })
  

  
  haplotypesData <- reactive({
    tmp<-data.frame(input$mug)
    init$mug[[1]][1:3]<-tmp[1,]
    init$mug[[2]][1:3]<-tmp[2,]
    init$mug[[3]][1:3]<-tmp[3,]
    init$mug[[4]][1:3]<-tmp[4,]
    init$mug[[5]][1:3]<-tmp[5,]
    init$populations<-as.character(input$pop_multi)
    haplotypes<-haplotypeImpute(init$mug,as.list(input$pop_single))
    haplotypePairs<-haplotypePairImpute(init$mug,init$populations)
    class(haplotypePairs$haplotype_pairs[[3]])<-"numeric"
    class(haplotypePairs$haplotype_pairs[[6]])<-"numeric"
    class(haplotypePairs$haplotype_pairs[[7]])<-"numeric"
    ####left off here... need to figure out the 2X rule (urn sampling model)
    ###so in constructing a contingency table and looking at what is margined out
    ###pradeep doesn't double list [H1=a,H2=b|R1=CAU,R2=AFA] for the mirror [H1=b,H2=a|R1=AFA,R2=CAU] so any
    ###mlt need a 2x multiplier whether heterzygote or homozygote
    ###but the compliment of the above [H1=b,H2=a|R1=CAU,R2=AFA] the mirror [H1=a,H2=b|R1=AFA,R2=CAU]
    ###makes the list, so basically the rule will be if non-mlt multiply likelihood by 2X to deal with [b,a] and [a,b]
    ###this is already implicitly taken into account (2 copies will be present) in a multi, so just multipley the likelihood
    ###by 2X for any multi to deal with the magining of the race phasing, not the haploypes because 2 copies are present
    ###(i.e. you do not multiply by 4X like you might think)

    pairs<-haplotypePairs$haplotype_pairs
    index<-paste(pairs$Race1,pairs$Race2,sep="-")
    lik<-data.frame("Race"=unique(index),"likelihood"=0)
    c=0
    for (b in lik$Race){
      c=c+1
      tmp<-pairs[index %in% b,,drop=F]
      for (i in 1:nrow(tmp)){
        if(tmp$Race1[i] == tmp$Race2[i]){
          if(tmp$Haplotype1[i] == tmp$Haplotype2[i]){
            lik$likelihood[c]<-lik$likelihood[c]+ tmp$Frequency1[i]*tmp$Frequency2[i]
            }else{
              lik$likelihood[c]<-lik$likelihood[c]+ 2*tmp$Frequency1[i]*tmp$Frequency2[i]
             }      
        }else{
          lik$likelihood[c]<-lik$likelihood[c]+ 2*tmp$Frequency1[i]*tmp$Frequency2[i]
        }
      }
    }
    
    
      
      
    
      list(haplotypes=haplotypes,haplotypePairs=haplotypePairs,likelihood=lik)
  })
  
  

  output$graph_lik<-renderPlot({
    dat<-haplotypesData ()[['likelihood']]
    thePlot<-ggplot(dat,aes(x=likelihood,y=reorder(Race,likelihood)))+geom_segment(aes(yend=Race),xend=0)+
      geom_point(size=3)+theme_bw()+xlab("Likelihood")+ylab("Race Group")+ggtitle("Likelihood Contribution")+
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.y=element_line(colour="grey60",linetype="dashed"))
    print(thePlot)
    
    
  })
  
  
  output$haplotypePairs <- renderDataTable({
  haplotypesData()$haplotypePairs$haplotype_pairs
  })
  
  
  output$haplotypes <- renderDataTable({
    haplotypesData()$haplotypes$haplotypes
  })
  
  
  output$mug <- renderDataTable({
    data.frame(input$mug)
  })
  
  output$pop_multi <- renderDataTable({
    data.frame(populations=input$pop_multi)
  })
  
  output$pop_single <- renderDataTable({
    data.frame(population=c(input$pop_single,NA))
  })

  
})