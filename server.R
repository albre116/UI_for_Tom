####################################
#### Bayes Classifier - server.R ###
#### Mark Albrecht               ###
####################################
library(shiny)
library(plyr)
library(ggplot2)
library(shinyIncubator)
library(ggmap)
library(acs)
api.key.install(key="a4967dc495fd5b445d602daf6e9626c4bcf2c050") ###key for acs pakages to access census API


###initialize values in array
init<-haplotypePairImpute()###call the function to get the HLA codes
state_mug<-rbind(data.frame(init$mug[1]),
                 data.frame(init$mug[2]),
                 data.frame(init$mug[3]),
                 data.frame(init$mug[4]),
                 data.frame(init$mug[5]))
state_pop<-data.frame(init$populations)
chos_ini<-as.character(state_pop[[1]])

shinyServer(function(input, output, session){ # pass in a session argument
  
  output$mlt_chooser<-renderUI({
    chooserInput("mlt_race_pairs", "Available", "Selected",
                 chos_ini[-c(2,14)],chos_ini[c(2,14)], size = 10, multiple = TRUE
    )
    
  })
  
  
  output$single_chooser<-renderUI({
    selectInput("single_race", "Race for Single Haplotype List",
                 choices=input$mlt_race_pairs$right, selected=NULL)
    
  })
  
  output$table_mug<-renderUI({
    matrixCustom('mug', 'MUG Typing', state_mug)
    })
  
  output$num_show<-renderUI({
    numericInput('cut', 'Number of Classes To Cut Display At', 20)
  })

  
  output$naive_prior<-renderUI({
    start<-data.frame(input$mlt_race_pairs$right,i_prior=1/length(input$mlt_race_pairs$right))
    matrixCustom('naive_prior', 'Priors to Be Applied',start)
  })
  
  
  output$update_mug<-renderUI({
    actionButton("update_mug", "Update Mug")
  })
  

  haplotypeSingle<-reactive({
    input$update_mug
    tmp<-isolate(data.frame(input$mug))
    HLA<-list()
    for (i in 1:nrow(tmp)){
      s <- list(locus=tmp[i,1], type1=tmp[i,2], type2=tmp[i,3])
      HLA[[i]]<-s
    }

    if(input$single_race != c("")){
      haplotypes<-haplotypeImpute(HLA,input$single_race)} else{
        haplotypes=NULL
      }
    haplotypes
    
  })
  
  
  
  
  
  
  haplotypesData <- reactive({
    input$update_mug
    tmp<-isolate(data.frame(input$mug))
    HLA<-list()
    for (i in 1:nrow(tmp)){
      s <- list(locus=tmp[i,1], type1=tmp[i,2], type2=tmp[i,3])
      HLA[[i]]<-s
    }
    init$populations<-input$mlt_race_pairs$right
    haplotypePairs<-haplotypePairImpute(HLA,init$populations)
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

    
    ####Compute likelihood of HLA given Race1 & Race2
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

    
    ####Construct Naive Prior R1 independent of R2#####
    if(!is.null(input$naive_prior)){
      prior_naive<-data.frame("Race"=input$naive_prior[,1],"Prior"=input$naive_prior[,2])
      class(prior_naive[[2]])<-"numeric"} else{
        prior_naive<-NULL
      }
    
    prior<-lik
    colnames(prior)[2]<-"Prior"
    prior$Prior=0
    #####Compute the Priors####
    if(!is.null(prior_naive)){
    for (i in 1:nrow(prior)){
      R1=strsplit(as.character(prior$Race[i]),"-")[[1]][1]
      R2=strsplit(as.character(prior$Race[i]),"-")[[1]][2]
      prior$Prior[i]=prior_naive$Prior[prior_naive$Race==R1]*prior_naive$Prior[prior_naive$Race==R2]
        
    }
    ###normalize Prior
    prior$Prior<-prior$Prior/sum(prior$Prior)}
    
    
    #####compute the Bayes call
    call<-lik
    colnames(call)[2]<-"Probability"
    call$Probability=0
    #####Compute the Priors####
    if(!is.null(prior_naive)){
      for (i in 1:nrow(call)){
        call$Probability[i]=lik$likelihood[i]*prior$Prior[i]
        
      }
    call$Probability<-call$Probability/(sum(call$Probability))
      
      
    }
    
    

    idx<-1:length(call$Probability)
    idx<-idx[order(call$Probability)]
    lik$Race=factor(lik$Race,levels=call$Race[idx])###sort likelihood for display
    prior$Race=factor(prior$Race,levels=call$Race[idx])###sort prior for display
    call$Race=factor(call$Race,levels=call$Race[idx])###sort call for display
    
      
      
    
      list(haplotypePairs=haplotypePairs,likelihood=lik,prior=prior,call=call,idx=idx)
  })
  
  

  output$lik_plot<-renderPlot({
    dat<-haplotypesData ()[['likelihood']]
    idx<-haplotypesData ()[['idx']]
    idx<-abs(order(idx)-(length(idx)+1))###generate ranking 1 to N
    dat<-dat[idx<=input$cut,]
    thePlot<-ggplot(dat,aes(x=likelihood,y=Race))+geom_segment(aes(yend=Race),xend=0)+
      geom_point(size=3)+theme_bw()+xlab("Likelihood")+ggtitle("Likelihood Contribution")+xlim(0,max(dat$likelihood))
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.y=element_line(colour="grey60",linetype="dashed"))
    print(thePlot)
    
  })
  
  output$prior_plot<-renderPlot({
    dat<-haplotypesData ()[['prior']]
    idx<-haplotypesData ()[['idx']]
    idx<-abs(order(idx)-(length(idx)+1))###generate ranking 1 to N
    dat<-dat[idx<=input$cut,]
    thePlot<-ggplot(dat,aes(x=Prior,y=Race))+geom_segment(aes(yend=Race),xend=0)+
      geom_point(size=3)+theme_bw()+xlab("Prior")+ggtitle("Prior Contribution")+xlim(0,max(dat$Prior))
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.y=element_line(colour="grey60",linetype="dashed"))
    print(thePlot)
    
  })
  
  output$call_plot<-renderPlot({
    dat<-haplotypesData ()[['call']]
    idx<-haplotypesData ()[['idx']]
    idx<-abs(order(idx)-(length(idx)+1))###generate ranking 1 to N
    dat<-dat[idx<=input$cut,]
    thePlot<-ggplot(dat,aes(x=Probability,y=Race))+geom_segment(aes(yend=Race),xend=0)+
      geom_point(size=3)+theme_bw()+xlab("Probability")+ggtitle("Bayes Classifier Call")+xlim(0,max(dat$Probability))+
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major.y=element_line(colour="grey60",linetype="dashed"))
    print(thePlot)
    
  })
  
  
  output$call_table<-renderDataTable({
  dat=haplotypesData ()[['call']]
  idx<-haplotypesData ()[['idx']]
  idx<-abs(order(idx)-(length(idx)+1))
  data.frame(dat,"Rank"=idx)
  })
  
  output$haplotypePairs <- renderDataTable({
  haplotypesData()$haplotypePairs$haplotype_pairs
  })
  
  
  output$haplotypes <- renderDataTable({
    haplotypeSingle()$haplotypes
  })
  
  
  output$address_input<-renderUI({
    textInput("Address","Input Search Address",value="3001 Broadway Street NE Suite 100, Minneapolis, MN 55413")
  })
  
  output$action_button<-renderUI({
    actionButton("goButton", "Retrieve Address & Map Zoom")
  })
  
  geo_loc<- reactive({
    
    input$goButton
    loc<-isolate(geocode(input$Address, output="all"))
    
  })

  
  
  
  
  
  
  
  census_dat<-reactive({
  browser()
  input$goButton
  geo_loc<-geo_loc()
  tmp<-acs.fetch(geo=geo.make(state="MN",county="Hennepin", tract="*"), table.number="B04001")
  
  })
  


  
  output$SIRE_map<-renderPlot({


    
    input$goButton
    
    
    geo_loc<-geo_loc()
    #census_dat<-census_dat()
    
    pt<-data.frame(lon=geo_loc$results[[1]]$geometry$location$lng,lat=geo_loc$results[[1]]$geometry$location$lat)
    
    z=isolate(input$zoom_res)
    l=isolate(input$Address)
    map<-do.call("qmap",list(location=l,zoom=z))
    map<-map+geom_point(aes(x=lon,y=lat),size=8,colour="Blue",data=pt)
    print(map)
    
    
  })
  
  
  
  
  


  
})

