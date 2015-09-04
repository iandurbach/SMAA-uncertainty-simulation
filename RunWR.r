#####################################################
### Main file for running "weight restrictions"   ###
### simulations (Section 4.1 of paper )           ###
### For parameters used see "SimWR.r"             ###
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
source('SimWR.r')

# To simulate provision of wt information in decreasing order of importance
# remove comments from two files below
#source('ApplyMAUT_incr_order.r')   
#source('Weights_incr_order.r')

###################################################################
# k is number of "scenarios" used in generation of attribute values
# runs is number of SMAA runs
# sims is number of simulations run at each combination of parameter values

K <<- 15000
nd <<- "T"
runs <<- 10000
sims <<- 500
simname <<- date()
identifier <<- "run_sim_WR_inc"
u_c <<- 0
thin.f = 100

runsimWR(K=K,nd=nd,runs=runs,sims=sims,simname=simname,identifier=identifier)
