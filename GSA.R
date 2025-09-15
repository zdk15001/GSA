# Project:   Grocery Store Access (GSA)
# Located:   Google Drive
# File Name: GSA
# Date:      2025_9_8
# Who:       Josh Sanchez and Zachary D. Kline
# What:      Script for analysis of CPS data

###############################################################################
#################            STEP 1: SETTINGS             #####################
###############################################################################
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# Set working directory (will be different for each user)
#setwd("G:/My Drive/EDU_SYNC/Research/Active/GSA/Work")


# load packages (install before if neccesary)
library(haven)

# load data from sav
cps <- read_sav("cps_00005.sav")
