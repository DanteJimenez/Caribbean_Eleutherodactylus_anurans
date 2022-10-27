rm(list=ls())
library(DAISIE)

##----- SA1-M5 (M= 2715, age= 40)
# M5 type 1 and 2
SA1_M5_t1y2<-DAISIE_sim(
  replicates=1000,time=40,
  pars = c(0.057324668, 0.007128429, 41.38616712, 0.000148218, 0.165969329,
          0.455767851, 0.007010833, 214.4799623, 3.48E-05, 75.90930665),
  prop_type2_pool = 0.366, 
  M=2715)


# save(SA1_M5_t1y2, file = "SA1_M5_t1y2.Rdata")