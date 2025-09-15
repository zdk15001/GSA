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
#setwd("~/Library/CloudStorage/GoogleDrive-sanchej6@tcnj.edu/.shortcut-targets-by-id/1ulTYv34Kx9mqKoGgMo1TF6o4gmz3sn5f/GSA/Work ")

# load packages (install before if neccesary)
library(haven)

# load data from sav
cps <- read_sav("cps_00005.sav")



###############################################################################
#################            STEP 2: Clean             #####################
###############################################################################


##### Clean grocery store access
# Step 1: Examine
table(cps$FSSHOPMKTLW)

# Step 2:  clean by removing missing
cps$access <- ifelse(cps$FSSHOPMKTLW == 1, 0, 
                             ifelse(cps$FSSHOPMKTLW == 2, 1, NA))

# Step 3: Examine cleaned variable
table(cps$FSSHOPMKTLW, cps$access, useNA = "ifany")

##### Clean metro
# Step 1: Examine
table(cps$METRO)

# Step 2:  clean by removing missing







###############################################################################
#################            STEP 3: Create complete case dataset      #####################
###############################################################################


###############################################################################
#################            STEP 4:Descriptive Statistics      #####################
###############################################################################



###############################################################################
#################            STEP 5: Regression analysis       #####################
###############################################################################
