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

# load packages (install before if necessary)
library(haven)
#install.packages("dplyr")
library(dplyr)

# load data from sav
cps <- read_sav("cps.sav")



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
cps$msa <- ifelse(cps$METRO == 1, 0, 
                     ifelse(cps$METRO >= 2 & cps$METRO <=4, 1, NA))

# step 3: confirm
table(cps$METRO, cps$msa, useNA = "ifany")

##### Clean household food security scale; 30-day
# Step 1: Examine
table(cps$FSSTATUSM)

#Step 2: Clean by removing missing
cps$secure <- ifelse(cps$FSSTATUSM == 1, 1, 
                    ifelse(cps$FSSTATUSM == 2 | cps$FSSTATUSM ==3, 0, NA))

# Step 3: Examine cleaned variable
table(cps$FSSTATUSM, cps$secure, useNA = "ifany")

##### Clean amount spent in supermarket last week
# Step 1: Examine
table(cps$FSSPDMKTLW)

# step 2 code missing
cps$grocery_spending <- cps$FSSPDMKTLW
cps$grocery_spending <- ifelse(cps$FSSPDMKTLW > 490, NA, cps$FSSPDMKTLW)

# step 3: confirm
cps$grocery_check <- cps$FSSPDMKTLW - cps$grocery_spending
table(cps$grocery_check)

##### Clean bought food at other places online or last week
# Step 1: Examine
table(cps$FSSHOPSTRLW)

##### Clean bought food at places other than super market
# Step 1: Examine
table(cps$FSSHOPOTH)

##### Clean received emergency food
# Step 1: Examine
table(cps$FSFDBNKMO)

##### Clean how often received emergency food
# Step 1: Examine
table(cps$FSFDBNK)

##### Clean received SNAP; food stamps in the past year
# Step 1: Examine
table(cps$FSFDSTMP)






###############################################################################
#################            STEP 3: Create complete case dataset      #####################
###############################################################################


###############################################################################
#################            STEP 4:Descriptive Statistics      #####################
###############################################################################



###############################################################################
#################            STEP 5: Regression analysis       #####################
###############################################################################
