#----------------------------------------------------------#
#
#
#               GRAD_Exclosures PNG
#
#         artropods size change due to treatment
#
#                     Katerina Sam
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "data/lock/revn.lock")
renv::restore(lockfile = "data/lock/revn.lock")

# libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(MuMIn)
library(emmeans)
library(performance)
library(glmmTMB)

#----------------------------------------------------------#
# 3. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  22

PDF_width <-  10
PDF_height <-  6


# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
