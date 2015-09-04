#####################################################
### Main file for running "full" simulations      ###
### (Section 4.2 of paper )                       ###
### For parameters used see "SimMain.r"           ###
### Ian Durbach, indurbach@gmail.com              ###
### last updated: 2015-09-04                      ###
#####################################################

library(hitandrun)
library(smaa)

# remember to setwd
source('ApplyErrors.r')
source('ApplyMAUT.r')
source('Generate.r')
source('OutputMeasures.r')
source('SMAA-Boot.r')
source('SMAA1.r')
source('SMAA2.r')
source('SMAA3.r')
source('SMAA4.r')
source('Weights.r')
source('SimMain.r')

###################################################################
# k is number of "scenarios" used in generation of attribute values
# runs is number of SMAA runs
# sims is number of simulations run at each combination of parameter values

K=15000
nd="T"
runs=10000
sims=100
simname=date()
identifier="run_sim_main"
thin.f = 100

runsimMain(K=K,nd=nd,runs=runs,sims=sims,simname=simname,identifier=identifier)
