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
cps <- read_sav("cps9.22.sav")



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

# Step 2: clean by removing missing
cps$buy_other <- ifelse(cps$FSSHOPSTRLW == 1, 0,
                       ifelse(cps$FSSHOPSTRLW == 2, 1, NA))

# Step 3: confirm
table(cps$FSSHOPSTRLW, cps$buy_other, useNA = "ifany")


##### Clean received emergency food
# Step 1: Examine
table(cps$FSFDBNKMO)

# Step 2: clean
cps$emergency_food <- ifelse(cps$FSFDBNKMO == 1, 0,
                       ifelse(cps$FSFDBNKMO == 2, 1, NA))

# Step 3: confirm
table(cps$FSFDBNKMO, cps$emergency_food, useNA = "ifany")


##### Clean received SNAP; food stamps in the past year
# Step 1: Examine
table(cps$FSFDSTMP)

# Step 2: clean
cps$food_stamp <- ifelse(cps$FSFDSTMP == 1, 0,
                            ifelse(cps$FSFDSTMP == 2, 1, NA))

#Step 3: confirm
table(cps$FSFDSTMP, cps$food_stamp, useNA = "ifany")

##### Clean family income
#Step 1: examine
table(cps$FAMINC)

# Step 2: clean
cps <- mutate(cps, less_than50k = ifelse(FAMINC <= 740, 1,
                                    ifelse(FAMINC >= 995, NA, 0)))
cps <- mutate(cps, more_than50k = ifelse(FAMINC >= 800 & FAMINC <= 994, 1,
                                    ifelse(FAMINC >= 995, NA, 0)))
#cps$more_than50k <- ifelse(cps$FAMINC >= 800 & cps$FAMINC <= 994, 1,
                           #ifelse(cps$FAMINC >=995, NA, 0))
# Step 3: confirm
table(cps$FAMINC, cps$less_than50k, useNA = "ifany")
table(cps$FAMINC, cps$more_than50k, useNA = "ifany")

##### Clean age
# Step 1: examine
table(cps$AGE)

##### Clean sex
# Step 1: examine
table(cps$SEX)

# Step 2: clean
cps$male <- ifelse(cps$SEX == 1, 1, 0)
cps$female <- ifelse(cps$SEX == 2, 1, 0)

# Step 3: confirm
table(cps$SEX, cps$male, useNA = "ifany")
table(cps$SEX, cps$female, useNA = "ifany")

##### Clean race
# Step 1: examine
table(cps$RACE)

#Step 2: clean
cps$white <- ifelse(cps$RACE == 100, 1, 0)
cps$black <- ifelse(cps$RACE == 200, 1, 0)
cps$native <- ifelse(cps$RACE == 300, 1, 0)
cps$asian <- ifelse(cps$RACE == 651, 1, 0)
cps$mixed <- ifelse(cps$RACE >= 652, 1, 0)

# Step 3: confirm
table(cps$RACE, cps$white, useNA = "ifany")
table(cps$RACE, cps$black, useNA = "ifany")
table(cps$RACE, cps$native, useNA = "ifany")
table(cps$RACE, cps$asian, useNA = "ifany")
table(cps$RACE, cps$mixed, useNA = "ifany")


##### Clean marital status
# Step 1: examine
table(cps$MARST)

#Step 2: clean
cps$married <- ifelse(cps$MARST == 1 | cps$MARST == 2, 1, 0)
cps$not_married <- ifelse(cps$MARST >= 3 & cps$MARST <= 7, 1,
                          ifelse(cps$MARST == 9, NA,))

#Step 3: confirm
table(cps$MARST, cps$married, useNA = "ifany")
table(cps$MARST, cps$not_married, useNA = "ifany")


##### Clean employment status
# Step 1: examine
table(cps$EMPSTAT)

# Step 2: clean
cps$ILF <- ifelse(cps$EMPSTAT >= 01 & cps$EMPSTAT < 12, 1, 0)
cps$NILF <- ifelse(cps$EMPSTAT >= 12, 1,
                   ifelse(cps$EMPSTAT == 00, NA, 0))

# Step 3: confirm 
table(cps$EMPSTAT, cps$ILF, useNA = "ifany")
table(cps$EMPSTAT, cps$NILF, useNA = "ifany")










###############################################################################
#################            STEP 3: Create complete case dataset      #####################
###############################################################################


###############################################################################
#################            STEP 4:Descriptive Statistics      #####################
###############################################################################



###############################################################################
#################            STEP 5: Regression analysis       #####################
###############################################################################
