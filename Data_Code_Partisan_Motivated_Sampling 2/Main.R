###This is the main file to reproduce the statistical analyses 
###for Partisan-Motivated-Sampling manuscript 2021

###########Remember to set the current directory as working directory before running this code##########

########Install and read in packages used for the analyses#########
source('scripts/Packages.R')

#######Script for Exclusion Criterion
source('scripts/ParticipantExclusion.R')

#######Script for Individual Differences
source('scripts/IndividualDifferences.R')

######Analyses for Experiment 1
source('scripts/Exp1a.Analyses.R')

#######Analyses for Experiment 2
source('scripts/Exp1b.Analyses.R')

#######Analyses for Experiment 3
source('scripts/Exp2.Analyses.R')

#######Analyses for Meta 1
source('scripts/Study1Meta.R')

#######Analyses for Meta 2
source('scripts/Study2Meta.R')

#######Script for Power Analyses
source('scripts/powersim.R')

#######Analyses for Meta4
source('scripts/Study4Meta.R')

#######Supplemental Models
source('scripts/SupplementalModels.R')

#######Analyses for Meta 3 
#Last in part because it takes the longest to run. 
source('scripts/Study3Meta.R')


