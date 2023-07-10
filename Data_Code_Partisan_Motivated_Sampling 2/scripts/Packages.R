# Load groundhog
library(groundhog)

# Specify the packages required
pkgs <-  c("psych", "plyr", "ggplot2", "lme4", "reshape2", "tidyverse", "ggpubr", 
           "sjPlot", "ggeffects", "MASS", "lmerTest", "blmeco", "Rmisc", "numDeriv",
           "effsize", "simr", "survival", "survminer", "lubridate", "broom", "gtsummary", 
           "knitr", "rstatix", "coxme", "car", "emmeans", "fastDummies")

# Set groundhog day
groundhog.day <- '2021-05-22'

# Load libraries with groundhog
groundhog.library(pkgs, groundhog.day)
