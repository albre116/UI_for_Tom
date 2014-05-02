####################################
#### Bayes Classifier - server.R ###
#### Mark Albrecht               ###
####################################
library(shiny)
library(plyr)
library(ggplot2)
library(shinyIncubator)
library(ggmap)
library(mapproj)
library(acs)
library(gridExtra)
library(MASS)
library(reshape2)


#init<-haplotypePairImpute()###call the function to get the HLA codes
s1 <- list(locus="A", type1="31:01", type2="66:01")
s2 <- list(locus="BPR", type1="40:02", type2="41:02")
s3 <- list(locus="C", type1="03:04", type2="17:03")
s4 <- list(locus="DRB1", type1="07:01", type2="04:04")
s5 <- list(locus="DQB1", type1="03:02", type2="02:01")
s6 <- list(locus="DRB3", type1="", type2="")
s7 <- list(locus="DRB4", type1="", type2="")
s8 <- list(locus="DRB5", type1="", type2="")
state_mug <- rbind(data.frame(s1), data.frame(s2), data.frame(s3), 
                   data.frame(s4), data.frame(s5), data.frame(s6), 
                   data.frame(s7), data.frame(s8))
chos_ini<- c("AAFA","AFB", "AINDI", "AMIND", "CARB", "CARHIS", "CARIBI", "FILII", "HAWI",
                     "JAPI", "KORI", "MENAFC", "MSWHIS", "NAMER", "NCHI", "SCAHIS", "SCSEAI", "VIET",
                     "CAU","HIS","NAM","AFA","API")

map<-c("AFA","AFA","API","NAM","AFA","HIS","NAM","API","API","API","API","CAU","HIS",
       "CAU","API","HIS","API","API","CAU","HIS","NAM","AFA","API")

FLAG<<-FALSE###Needed for state changes

shinyServer(function(input, output, session){ # pass in a session argument

  HLA<-reactive({
    input$lookup
    if(is.na(isolate(input$ID))){
      out=state_mug
      HLA=NULL
    }
    else{
      HLA=switch(isolate(input$type),
           DID={DID_HLA_Lookup(isolate(input$ID))},
           CID={CID_HLA_Lookup(isolate(input$ID))},
           RID={RID_HLA_Lookup(isolate(input$ID))},
    )
    
    out<-data.frame()
    for (i in 1:length(HLA$mug)){
      out<-rbind(out,data.frame(HLA$mug[i]))
    }
    }

  list(HLA=out,fullresult=HLA)
  
  })
  
  output$db_race<-renderTable({
    input$lookup
    if(!is.na(isolate(input$ID))){
      SIRE=switch(isolate(input$type),
                  DID={DID_SIRE_Lookup(isolate(input$ID))},
                  CID={CID_SIRE_Lookup(isolate(input$ID))},
                  RID={RID_SIRE_Lookup(isolate(input$ID))},
      )
      return(as.data.frame(SIRE))
      }
    return(NULL)
    
  })
    
  
  
  output$mlt_chooser<-renderUI({
    input$lookup
    idx<-chos_ini %in% c("AFA","CAU","API","HIS","NAM")
    if(!is.na(isolate(input$ID))){
      SIRE=switch(isolate(input$type),
                 DID={DID_SIRE_Lookup(isolate(input$ID))},
                 CID={CID_SIRE_Lookup(isolate(input$ID))},
                 RID={RID_SIRE_Lookup(isolate(input$ID))},
      )
      
      if(is.null(SIRE$ethnicity)){idx<-rep(TRUE,length(chos_ini))} else{
      tmp<-data.frame(race=SIRE$races,ethnicity=SIRE$ethnicity)
      choices<-data.frame()
      for (i in 1:nrow(tmp)){
      mapped<-SIRE_map(tmp$race[i],tmp$ethnicity[i])
      choices<-rbind(choices,mapped)
      }
      idx<-chos_ini %in% choices$population
    }}
    
    chooserInput("mlt_race_pairs", "Available", "Selected",
                 chos_ini[!idx],chos_ini[idx], size = 10, multiple = TRUE
    )
  })
  
  
  output$single_chooser<-renderUI({
    selectInput("single_race", "Race for Single Haplotype List",
                 choices=input$mlt_race_pairs$right, selected=NULL)
    
  })
  
  output$table_mug<-renderUI({
    matrixCustom('mug', 'MUG Typing', HLA()[["HLA"]])
    })
  
  output$num_show<-renderUI({
    numericInput('cut', 'Number of Classes To Cut Display At', 20)
  })

  
  output$naive_prior<-renderUI({
    input$update_prior
    if(input$census_prior){
      labels<-input$mlt_race_pairs$right
      i_prior<-c()
      for (i in labels){
        b<-map[chos_ini==i]
        q<-isolate(knn_call()[["call"]])
        val<-q$mean[grep(b,q$race)]
        i_prior<-c(i_prior,val)}
      start<-data.frame(labels,i_prior)
    }else{
    start<-data.frame(input$mlt_race_pairs$right,i_prior=1/length(input$mlt_race_pairs$right))}
    matrixCustom('naive_prior', 'Priors to Be Applied',start)
  })
  
  
  output$update_mug<-renderUI({
    actionButton("update_mug", "Update Mug")
  })
  

  haplotypeSingle<-reactive({
    input$update_mug
    input$lookup
    tmp<-isolate(data.frame(input$mug))
    HLA<-list()
    for (i in 1:nrow(tmp)){
      s <- list(locus=tmp[i,1], type1=tmp[i,2], type2=tmp[i,3])
      if (s$type1 != "" | s$type2 != ""){
      HLA[[i]]<-s}
    }

    if(input$single_race != c("")){
      haplotypes<-haplotypeImpute(HLA,input$single_race)} else{
        haplotypes=NULL
      }
    haplotypes
    
  })
  

  
  haplotypes_pre_Data<-reactive({
    input$update_mug
    tmp<-data.frame(input$mug)
    HLA<-list()
    for (i in 1:nrow(tmp)){
      s <- list(locus=tmp[i,1], type1=tmp[i,2], type2=tmp[i,3])
      if (s$type1 != "" | s$type2 != ""){
        HLA[[i]]<-s}
    }
    if(length(input$mlt_race_pairs$right)<=1){return(NULL)}else{
    haplotypePairs<-haplotypePairImpute(HLA,input$mlt_race_pairs$right)}
    class(haplotypePairs$haplotype_pairs[[3]])<-"numeric"
    class(haplotypePairs$haplotype_pairs[[6]])<-"numeric"
    class(haplotypePairs$haplotype_pairs[[7]])<-"numeric"
    return(haplotypePairs)
    })
    



  haplotypesData <- reactive({
    haplotypePairs<-haplotypes_pre_Data()
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
    a=paste(input$naive_prior[,1],collapse="+")
    b=paste(input$mlt_race_pairs$right,collapse="+")
    check=(a==b)
    
    if(!is.null(input$naive_prior) & check){
      prior_naive<-data.frame("Race"=input$naive_prior[,1],"Prior"=input$naive_prior[,2])
      class(prior_naive[[2]])<-"numeric"}
       else{
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

    return(list(haplotypePairs=haplotypePairs,likelihood=lik,prior=prior,call=call,idx=idx))
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
    if (is.null(haplotypes_pre_Data())){
     data.frame("You Must Pick At Least 2 Race Groups"="You Must Pick At Least 2 Race Groups") 
    }else{data.frame(haplotypes_pre_Data()$haplotype_pairs)}
  })
  
  

  
  output$haplotypes <- renderDataTable({
    haplotypeSingle()$haplotypes
  })
  
  
  
  
  
  #############################
  #######Mapping Commands######
  #############################
  
  
  output$address_input<-renderUI({
    textInput("Address","Input Search Address",value="3001 Broadway Street NE Suite 100, Minneapolis, MN 55413")
  })
  
  output$action_button<-renderUI({
    actionButton("goButton", "Retrieve Address & Map Zoom")
  })
  
      
  geo_loc<- reactive({
    input$goButton #sit till called
    loc<-isolate(geocode(input$Address, output="all"))
    
  })

  

  


  
  p<-reactive({
    input$goButton###make this sit till clicked
    geo_loc<-geo_loc()
    pt<-data.frame(x=geo_loc$results[[1]]$geometry$location$lng,y=geo_loc$results[[1]]$geometry$location$lat)
    z=isolate(input$zoom_res)
    q<-do.call("get_googlemap",list(center=as.numeric(pt),zoom=z,markers=pt))
    p<-ggmap(q)
    return(list(p=p,q=q))
  })
  
  
  
  census_dat<-reactive({
    input$goButton ###sit till called
    ####get data from the map fit
    p<-p()[["p"]]
    x_max=max(p$data$lon)
    x_min=min(p$data$lon)
    y_max=max(p$data$lat)
    y_min=min(p$dat$lat)
    
    ####create search grid for density smoothing
    seg<-isolate(input$grid)
    delta_x=(x_max-x_min)/(seg-1)
    delta_y=(y_max-y_min)/(seg-1)
    
    grid<-data.frame()
    for(i in 1:seg){
      for(j in 1:seg){
        grid<-rbind(grid,data.frame(x=x_min+delta_x*(i-1),y=y_min+delta_y*(j-1)))
      }
    }
    ###look up block group details form census API
    geo_loc<-geo_loc()
    lon=geo_loc$results[[1]]$geometry$location$lng
    lat=geo_loc$results[[1]]$geometry$location$lat
    block_grp<-Census_block(lat,lon)
    FIPS=block_grp$Block$FIPS    
    result<-data.frame(Census_pull_tract_acs(FIPS)[2])
    Individual<-data.frame(lat=lat,lon=lon,result)
    

    ###extract details for neighbors on grid###
    FIPS=character()
    for (i in 1:nrow(grid)){
      block_grp<-Census_block(grid$y[i],grid$x[i])
      FIPS=c(FIPS,block_grp$Block$FIPS)
    }
    
    result<-data.frame()
    for (i in 1:length(FIPS)){
    result<-rbind(result,data.frame(Census_pull_tract_acs(FIPS[i])[2]))
    }
    
    Neighbors<-data.frame(lat=grid$y,lon=grid$x,result)
    list(Individual=Individual,Neighbors=Neighbors,delta_x=delta_x,delta_y=delta_y)
        

  })
  
  
  
  output$census<-renderDataTable({
    c_i<-census_dat()[["Individual"]]
    c_n<-census_dat()[["Neighbors"]]
    rbind(data.frame("Source"="Target Individual",c_i),
          data.frame("Source"="Neighbor",c_n))
  })
  
  
  
  
  
  plot_dat<-reactive({
    p<-p()[["p"]]
    delta_x<-census_dat()[["delta_x"]]
    delta_y<-census_dat()[["delta_y"]]
    census<-census_dat()[["Neighbors"]]
    individual<-census_dat()[["Individual"]]
    sel<-c()
    for (i in 1:nrow(input$naive_prior)){
      map_naive<-map[chos_ini==input$naive_prior[i,1]]
      sel<-c(sel,grep(map_naive,colnames(census)))
    }
    sel<-unique(sel)
    tmp<-stack(census,select=sel)
    data<-data.frame()
    for (i in 1:length(sel)){
    data<-rbind(data,census[,c(1,2)])
    }
    
    data<-cbind(data,tmp)
    #####draw a random sample for plotting purposes using a multinomial distribution and jittering
    nsample=10000
    rs<-sample(1:nrow(data),nsample,replace=T,prob=(data$values/sum(data$values)))
    data_s<-data[rs,]
    jitter_x<-runif(nsample,min=-delta_x/2,max=delta_x/2)
    jitter_y<-runif(nsample,min=-delta_y/2,max=delta_y/2)
    data_s$lat<-data_s$lat+jitter_y
    data_s$lon<-data_s$lon+jitter_x
    
    
    #####compute 2d density estimates by race#####
    
    output<-data.frame()
    for (i in unique(data_s$ind)){
      dat<-data_s[data_s$ind %in% i,]
      dens<-kde2d(dat$lon,dat$lat,n=50)
      tmp<-data.frame(expand.grid(lon = dens$x, lat = dens$y),
                      z = as.vector(dens$z*(nrow(dat)/nrow(data_s))),ind=i)
      output<-rbind(output,tmp)
          
    }

    ###normalize density to a direct probability measure
    idx<-rep(1:nrow(tmp),length(unique(data_s$ind)))
    for(i in 1:nrow(tmp)){
      tt<-output$z[idx==i]
      tt<-tt/sum(tt)
      output$z[idx==i]<-tt
            
    }
    
    list(data_s=data_s,output=output,tmp=tmp)
    
    })
    
  
    knn_call<-reactive({###stopped here
      input$update_knn
      dat<-plot_dat()[["output"]]
      individual<-census_dat()[["Individual"]]
      data<-data.frame()
      for (i in unique(dat$ind)){
      tmp<-dat[dat$ind==i,]
      dist<-spDistsN1(as.matrix(tmp[,c(1,2)]),as.matrix(individual[,c(2,1)]),longlat=TRUE)
      idx<-order(dist)
      k=isolate(input$k_neigh)###number of nearest neighbors to make race call on
      keep<-1:length(idx) %in% idx[1:k]
      data<-rbind(data,tmp[keep,])
      }
      
      
      call<-data.frame()
      for (i in unique(data$ind)){
        call=rbind(call,data.frame(race=i,mean=mean(data$z[data$ind==i])))
                
      }
      call$mean<-call$mean/sum(call$mean)
      idx<-1:length(call$mean)
      idx<-idx[order(call$mean)]
      call$race=factor(call$race,levels=call$race[idx])###sort likelihood for display
      
      return(list(call=call,data=data))
      
    })
  
  output$class_call<-renderPlot({
      dat<-knn_call()[["call"]]
      thePlot<-ggplot(dat,aes(x=mean,y=race))+geom_segment(aes(yend=race),xend=0)+
        geom_point(size=3)+theme_bw()+xlab("Probability")+ggtitle("Prior Race Distribution Based on Census")+xlim(0,max(dat$mean))
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.y=element_line(colour="grey60",linetype="dashed"))
      print(thePlot)
      
    })
    

  
  
    output$SIRE_map_raw<-renderPlot({

      p<-p()[["p"]]
      output<-plot_dat()[["output"]]
      support<-knn_call()[["data"]]

    
    point=input$contour###set to plot type
    if (point==TRUE){
    Density_Map<-p+
      geom_point(aes(x=lon,y=lat,alpha=z,size=z,colour=z),data=output)+
      facet_grid(.~ ind)} else{
    
     Density_Map<-p+
     stat_contour(aes(x=lon,y=lat,z=z,colour = ..level..),data=output)+
      scale_colour_gradient(low = "black", high = "blue")+
       facet_grid(.~ ind)}
    
    if (input$show_support==TRUE){
      Density_Map<-Density_Map+geom_point(aes(x=lon,y=lat),data=support)
    }

     
    print(Density_Map)
        
  })
  
  output$SIRE_map_contour<-renderPlot({
    p<-p()[["p"]]
    data_s<-plot_dat()[["data_s"]]

    dens_layer<-stat_density2d(geom="point",aes(x=lon,y=lat,size=..density..,alpha=..density..,colour=..density..),contour=FALSE,data=data_s)
    
    Density_Map<-p+
      dens_layer+
      facet_grid(.~ ind)
      
    print(Density_Map)
    
  })
  
    
})

