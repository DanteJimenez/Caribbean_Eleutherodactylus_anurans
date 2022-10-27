library(DAISIE)
library(DDD)

## Example of 1 optimization with random starting values
## the_init<-1
# model<-5
# res<-200
# cond<-0
# methode<-"ode45"

set.seed(the_init) 

print(sessionInfo())

type1_data<-load("Caribbean_main_I40_M2715.Rdata")
type1_datalist<-get(type1_data)

island_age<-type1_datalist[[1]]$island_age
M<-type1_datalist[[1]]$not_present+length(type1_datalist)-1

P_type2<-NA

analysis_type<-'ARCHIPELAGO'

if(analysis_type=='ARCHIPELAGO') {
  
  type2_data<-load("Caribbean_main_I40_M2715_type2_p0.366.Rdata")
  type2_datalist<-get(type2_data)
  
  if(M==2715){P_type2<-0.366}
  if(M==2195){P_type2<-0.215}
  
}


if(model<5)  { datalist<-type1_datalist }
if(model>=5) { datalist<-type2_datalist }


r_lam<-runif(1,min=0,max=2)
r_mu<-runif(1,min=0,max=0.1)
r_K<-runif(1,min=200,max=300)
r_gam<-runif(1,min=0,max=0.0001)
r_ana<-runif(1,min=0.1,max=4)
r_lam_2<-runif(1,min=0,max=2)
r_mu_2<-runif(1,min=0,max=0.1)
r_K_2<-runif(1,min=200,max=300)
r_gam_2<-runif(1,min=0,max=0.0001)
r_ana_2<-runif(1,min=0.1,max=4)



## M1  CS - DD
if(model=="1") {
  
  ddmodel= 11
  idparsopt<-1:5
  parsfix<-NULL
  idparsfix<-NULL
  idparsnoshift = 6:10
  initparsopt<-c(r_lam,r_mu,r_K,r_gam,r_ana)
  type <-'type1'
}


## M2 CS - DI
if(model=="2") {
  ddmodel= 0
  idparsopt<-c(1,2,4,5)
  parsfix<-Inf
  idparsfix<-3
  idparsnoshift = 6:10
  initparsopt<-c(r_lam,r_mu,r_gam,r_ana)
  
  type <-'type1'
}



## M3 CS - DD no anagenesis
if(model=="3") {
  ddmodel= 11
  idparsopt<-c(1,2,3,4)
  parsfix<-0
  idparsfix<-5
  idparsnoshift = 6:10
  initparsopt<-c(r_lam,r_mu,r_K,r_gam)
  type <-'type1'
}

## M4 CS - DI no anagenesis
if(model=="4") {
  ddmodel= 0
  idparsopt<-c(1,2,4)
  parsfix<-c(Inf,0)
  idparsfix<-c(3,5)
  idparsnoshift = 6:10
  initparsopt<-c(r_lam,r_mu,r_gam)
  type <-'type1'
}

## M5 2 types
if(model=="5") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,r_ana,r_lam_2,
                  r_mu_2,r_K_2,r_gam_2,r_ana_2)
  idparsopt = 1:10
  parsfix = P_type2
  idparsfix = 11
  idparsnoshift = NULL
  ddmodel=11
  type <-'type2'
  
}


## M6 2 types different clado ext
if(model=="6") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,r_ana,r_lam_2,
                  r_mu_2,r_K_2)
  idparsopt = 1:8
  parsfix = P_type2
  idparsfix = 11
  idparsnoshift = c(9,10)
  ddmodel=11
  type <-'type2'
  
}

if(model=="7") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,r_lam_2,r_mu_2)
  idparsopt = c(1,2,4,5,6,7)
  parsfix = c(Inf,Inf,P_type2)
  idparsfix = c(3,8,11)
  idparsnoshift = c(9,10)
  ddmodel=0
  type <-'type2'
  
}


## M8 2 types no anagenesis for type 2
if(model=="8") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,r_ana,r_lam_2,
                  r_mu_2,r_K_2,r_gam_2)
  idparsopt = c(1,2,3,4,5,6,7,8,9)
  parsfix = c(0,P_type2)
  idparsfix = c(10,11)
  idparsnoshift = NULL
  ddmodel=11
  type <-'type2'
  
}


## M9 2 types like darwin's finches
if(model=="9") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,r_lam_2,r_mu_2,r_K_2)
  idparsopt = c(1,2,4,5,6,7,8)
  parsfix = c(Inf,P_type2)
  idparsfix = c(3,11)
  idparsnoshift = c(9,10)
  ddmodel=11
  type <-'type2'
  
}


## M10 2 types different all, no K
if(model=="10") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,r_lam_2,r_mu_2,
                  r_gam_2,r_ana_2)
  idparsopt = c(1,2,4,5,6,7,9,10)
  parsfix = c(Inf, Inf, P_type2)
  idparsfix = c(3,8,11)
  idparsnoshift = NULL
  ddmodel=0
  type <-'type2'
  
}

## M11 
if(model=="11") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,r_ana,
                  r_lam_2,r_mu_2,r_gam_2,r_ana_2)
  idparsopt = c(1,2,3,4,5,6,7,9,10)
  parsfix = c(Inf,P_type2)
  idparsfix = c(8,11)
  idparsnoshift = NULL
  ddmodel=11
  type <-'type2'
  
}

## M12
if(model=="12") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,r_lam_2)
  idparsopt = c(1,2,4,5,6)
  parsfix = c(Inf, Inf, P_type2)
  idparsfix = c(3,8,11)
  idparsnoshift = c(7,9,10)
  ddmodel=0
  type <-'type2'
  
}

## M13
if(model=="13") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,r_mu_2)
  idparsopt = c(1,2,4,5,7)
  parsfix = c(Inf, Inf, P_type2)
  idparsfix = c(3,8,11)
  idparsnoshift = c(6,9,10)
  ddmodel=0
  type <-'type2'
  
}


## M14
if(model=="14") {
  
  initparsopt = c(r_lam,r_mu,r_gam,r_ana,
                  r_lam_2,r_K_2)
  idparsopt = c(1,2,4,5,6,8)
  parsfix = c(Inf,P_type2)
  idparsfix = c(3,11)
  idparsnoshift = c(7,9,10)
  ddmodel=11
  type <-'type2'
  
}

## M15
if(model=="15") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,
                  r_ana,r_lam_2)
  idparsopt = 1:6
  parsfix = c(Inf,P_type2)
  idparsfix = c(8,11)
  idparsnoshift = c(7,9,10)
  ddmodel=11
  type <-'type2'
  
}

## M16
if(model=="16") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,r_ana,r_K_2)
  idparsopt = c(1:5,8)
  parsfix = P_type2
  idparsfix = 11
  idparsnoshift = c(6,7,9,10)
  ddmodel=11
  type <-'type2'
  
}


## M17
if(model=="17") {
  
  initparsopt = c(r_lam,r_mu,r_K,r_gam,
                  r_ana,r_lam_2,r_K_2)
  idparsopt = c(1:6,8)
  parsfix = P_type2
  idparsfix = 11
  idparsnoshift = c(7,9,10)
  ddmodel=11
  type <-'type2'
  
}



##### RUN1 ML Optimization
lik_res<-DAISIE_ML(datalist= datalist,
                   initparsopt=initparsopt, 
                   idparsnoshift = idparsnoshift,
                   idparsopt=idparsopt,
                   parsfix=parsfix,idparsfix=idparsfix,
                   ddmodel=ddmodel,
                   methode=methode, cond = cond, res = res)



lik_res<-as.matrix(lik_res)
to.write<-c(island_age,M,P_type2,
            the_init,type,model,res,methode,"R1",
            as.matrix(lik_res),
            initparsopt)
