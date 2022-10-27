rm(list=ls())

# Load simulations and call them island_replicates
load("SA1_M5_t1y2.Rdata")

island_replicates<-SA1_M5_t1y2


## Adjust values according to you data
number_species_data<-197
colonisations_data<-10
endemic_species_data<-196
non_endemic_species_data<-2
endemic_clades_data<-9
endemic_singletons_data<-5 # also known as anagenetic spp
size_largest_clade_data<-167
## If applicable:
number_type1_species_data<-30
number_type2_species_data<-167



##
replicates<-length(island_replicates)
time<-island_replicates[[1]][[1]]$island_age
if(is.null(island_replicates[[1]][[1]]$not_present_type1)==FALSE) 
{type<-"type1and2"
}else{type<-"type1only"}


###### Calculate overall species richness and 
##colonization statistics across all islands
number_colonists<-c()
number_spec<-c()
number_stac2<-c()
number_stac3<-c()
number_stac4<-c()
number_colonisations_including_stac3s<-c()
number_stac2_singletons <-c()
size_largest_clade<-c()
size_smallest_clade<-c()

if(type=="type1and2") {
  number_spec_type1<-c()
  number_spec_type2<-c()}


for(i in 1:replicates){
  
  the_island<-island_replicates[[i]]
  
  number_colonists<-append(number_colonists,
            the_island[[1]]$stt_all[,"present"][nrow(the_island[[1]]$stt_all)])
  
  unlist(the_island)->u_island
  
  if(number_colonists[[i]]==0){
    number_spec<-append(number_spec,0)
    number_stac2<-append(number_stac2,0)
    number_stac3<-append(number_stac3,0)
    number_stac4<-append(number_stac4,0)
    number_stac2_singletons <-append(number_stac2_singletons,0)
    size_largest_clade<-append(size_largest_clade,NA)
    size_smallest_clade<-append(size_smallest_clade,NA)
    number_colonisations_including_stac3s<-append(number_colonisations_including_stac3s,0)
    
    if(type=="type1and2") {
      number_spec_type1<-append(number_spec_type1,0)
      number_spec_type2<-append(number_spec_type2,0)
    }
    
  }
  
  if(number_colonists[[i]]>0){
    
    number_spec<-append(number_spec,
          sum(the_island[[1]]$stt_all[nrow(the_island[[1]]$stt_all),
                                      c(2,3,4)]))
    number_stac2<-append(number_stac2,
        length(which(u_island[which(names(u_island)=="stac")]==2)))
    number_stac3<-append(number_stac3,
    length(which(u_island[which(names(u_island)=="stac")]==3)))
    number_stac4<-append(number_stac4,
        as.numeric(the_island[[1]]$stt_all[nrow(the_island[[1]]$stt_all),2]))
    
    if(type=="type1and2") {
      number_spec_type1<-append(number_spec_type1,
    sum(the_island[[1]]$stt_type1[nrow(the_island[[1]]$stt_type1),
                                  c(2,3,4)]))
      number_spec_type2<-append(number_spec_type2,
    sum(the_island[[1]]$stt_type2[nrow(the_island[[1]]$stt_type2),
                                  c(2,3,4)]))
    }
    
    
    if(length(which(u_island[which(names(u_island)=="stac")]==2))>0)
    {
      store_stac2s_brts<-list()
      store_all_brts<-list()
      
      for (isl in 1:(length(the_island)-1))
      {
        if(the_island[[isl+1]]$stac==2) {
        store_stac2s_brts[[length(store_stac2s_brts)+1]]<-the_island[[isl+1]]$branching_times}
        store_all_brts[[length(store_all_brts)+1]]<-the_island[[isl+1]]$branching_times
      }
      number_stac2_singletons <-append(number_stac2_singletons,
                    length(which(unlist(lapply(store_stac2s_brts,length))==2)))
      size_largest_clade<-append(size_largest_clade,
                                 max(unlist(lapply(store_stac2s_brts,length)))-1)
      size_smallest_clade<-append(size_smallest_clade,
                                  min(unlist(lapply(store_all_brts,length)))-1)
      
    }else{
      number_stac2_singletons <-append(number_stac2_singletons,0)
      size_largest_clade<-append(size_largest_clade,NA)
      size_smallest_clade<-append(size_smallest_clade,NA)
      
    }
    
number_colonisations_including_stac3s<-append(number_colonisations_including_stac3s,
                                              number_colonists[[i]])
    
    if(number_stac3[i]>0)
    {
      count_stac3_cols<-0
      for (isl in 1:(length(the_island)-1))
      { if(the_island[[isl+1]]$stac==3) 
      { count_stac3_cols<-count_stac3_cols+length(the_island[[isl+1]]$all_colonisations)-1}
      }
      number_colonisations_including_stac3s[i]<-number_colonisations_including_stac3s[i]+count_stac3_cols
    }
    
  }
  
  
}

overall_results<-list(number_spec=number_spec,
                      number_colonists=number_colonists,
                      number_stac2=number_stac2,
                      number_stac3=number_stac3,
                      number_stac4=number_stac4,
                      number_stac2_singletons=number_stac2_singletons, 
                      number_colonisations_including_stac3s=number_colonisations_including_stac3s,
                      number_endemic_species=number_spec-number_stac4,
                      size_largest_clade= size_largest_clade,
                      size_smallest_clade=size_smallest_clade)

if(type=="type1and2") {
  overall_results<-list(number_spec=number_spec,
                        number_colonists=number_colonists,
                        number_stac2=number_stac2,
                        number_stac3=number_stac3,
                        number_stac4=number_stac4,
                        number_stac2_singletons=number_stac2_singletons, 
                        number_colonisations_including_stac3s=number_colonisations_including_stac3s,
                        number_endemic_species=number_spec-number_stac4,
                        size_largest_clade= size_largest_clade,
                        size_smallest_clade=size_smallest_clade,
                        number_spec_type1=number_spec_type1,
                        number_spec_type2=number_spec_type2)
}



par(mfrow=c(3,3))

####### Histogram of species richness
hist(overall_results$number_spec,xlab="Number of species",
     breaks=30,bty="n",col="cornsilk3",border="cornsilk3",main=NULL)
abline(v=median(overall_results$number_spec),col="black",lwd=2)
arrows(number_species_data, 2000,number_species_data, 0,col='blue',
       length=0.07,lwd=1.5)


### Colonizations
hist(overall_results$number_colonists,col="cornsilk3",
     border="cornsilk3",main=NULL,breaks=10,xlab="Number of colonization events")
abline(v=median(overall_results$number_colonists,na.rm=T),col="black",lwd=2)
arrows(colonisations_data, 2000,colonisations_data, 0,col='blue',
       length=0.07,lwd=1.5)


##### Size largest clade
hist(overall_results$size_largest_clade,col="cornsilk3",
     border="cornsilk3",main=NULL,breaks=30,xlab="Size largest clade")
abline(v=median(overall_results$size_largest_clade,na.rm=T),col="black",lwd=2)
arrows(size_largest_clade_data, 2000,size_largest_clade_data, 0,col='blue',
       length=0.07,lwd=1.5)


##### Number of endemic clades
hist(overall_results$number_stac2,
     col="cornsilk3",border="cornsilk3",
     main=NULL,breaks=20,
     xlab="Number of endemic clades",xlim=c(2,22))
abline(v=median(overall_results$number_stac2,na.rm=T),
       col="black",lwd=2)
arrows(endemic_clades_data, 2000,endemic_clades_data, 
       0,col='blue',length=0.07,lwd=1.5)


##### Number of endemic singletons species ("anagenetic" species)
hist(overall_results$number_stac2_singletons,col="cornsilk3",border="cornsilk3",
     main=NULL,breaks=12,xlab="Number of endemic singletons (anagenetic spp)")
abline(v=median(overall_results$number_stac2_singletons,na.rm=T),col="black",
       lwd=2)
arrows(endemic_singletons_data, 2000,endemic_singletons_data, 0,col='blue',
       length=0.07,lwd=1.5)


##### Number of non_endemic species
hist(overall_results$number_stac4,col="cornsilk3",border="cornsilk3",
     main=NULL,breaks=12,xlab="Number of non-endemic species")
abline(v=median(overall_results$number_stac4,na.rm=T),col="black",lwd=2)
arrows(non_endemic_species_data, 2000,non_endemic_species_data, 0,col='blue',
       length=0.07,lwd=1.5)



if(type=="type1and2") {
  ####### Number of type1 species
  hist(overall_results$number_spec_type1,xlab="Number of type 1 species",
       breaks=30,bty="n",col="cornsilk3",border="cornsilk3",main=NULL)
  abline(v=median(overall_results$number_spec_type1),col="black",lwd=2)
  arrows(number_type1_species_data, 2000,number_type1_species_data, 0,
         col='blue',
         length=0.07,lwd=1.5)
  
  ####### Number of type2 species
  hist(overall_results$number_spec_type2,xlab="Number of type 2 species",
       breaks=30,bty="n",col="cornsilk3",border="cornsilk3",main=NULL)
  abline(v=median(overall_results$number_spec_type2),col="black",lwd=2)
  arrows(number_type2_species_data, 2000,number_type2_species_data, 0,
         col='blue',
         length=0.07,lwd=1.5)
}



