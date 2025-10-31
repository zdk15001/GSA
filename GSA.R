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
#install.packages("psych")
library(psych)

# load data from sav
cps <- read_sav("cps.10.26.sav")



###############################################################################
#################            STEP 2: Clean             #####################
###############################################################################


##### Clean grocery store access
# Step 1: Examine
table(cps$FSSHOPMKTLW)

# Step 2:  clean by removing missing
cps$grocery_food <- ifelse(cps$FSSHOPMKTLW == 1, 0, 
                      ifelse(cps$FSSHOPMKTLW == 2, 1, NA))

# Step 3: Examine cleaned variable
table(cps$FSSHOPMKTLW, cps$grocery_food, useNA = "ifany")



#### Clean food at res, fast food, cafe, deli, convenience, vending machine
# Step 1: examine
table(cps$FSSHOPRESTLW)

#Step 2: clean by removing missing
cps$prepared_food <- ifelse(cps$FSSHOPRESTLW == 1, 0, 
                           ifelse(cps$FSSHOPRESTLW == 2, 1, NA))

#Step 3: Examine cleaned variable
table(cps$FSSHOPRESTLW, cps$prepared_food, useNA = "ifany")



##### clean food at dollar store, pharmacy, club store, farmer's market, or online
# Step 1: Examine
table(cps$FSSHOPSTRLW)

# Step 2: clean by removing missing
cps$alt_food <- ifelse(cps$FSSHOPSTRLW == 1, 0,
                        ifelse(cps$FSSHOPSTRLW == 2, 1, NA))

# Step 3: confirm
table(cps$FSSHOPSTRLW, cps$alt_food, useNA = "ifany")



##### clean food at not listed previously
# Step 1: Examine
table(cps$FSSHOPOTHLW)

# Step 2: clean by removing missing
cps$other_food <- ifelse(cps$FSSHOPOTHLW == 1, 0,
                       ifelse(cps$FSSHOPOTHLW == 2, 1, NA))

# Step 3: confirm
table(cps$FSSHOPOTHLW, cps$other_food, useNA = "ifany")



##### Clean household food security scale; 30-day
# Step 1: Examine
table(cps$FSSTATUSM)

#Step 2: Clean by removing missing
cps$secure <- ifelse(cps$FSSTATUSM == 1, 1, 
                    ifelse(cps$FSSTATUSM >= 98, NA, 0))

cps$low_secure <- ifelse(cps$FSSTATUSM == 2, 1, 
                     ifelse(cps$FSSTATUSM >= 98, NA, 0))

cps$very_low_secure <- ifelse(cps$FSSTATUSM == 3, 1, 
                     ifelse(cps$FSSTATUSM >= 98, NA, 0))



# Step 3: Examine cleaned variable
table(cps$FSSTATUSM, cps$secure, useNA = "ifany")
table(cps$FSSTATUSM, cps$low_secure, useNA = "ifany")
table(cps$FSSTATUSM, cps$very_low_secure, useNA = "ifany")


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



#### Clean cut or skipped meal
# Step 1: examine
table(cps$FSSKIPMOCAT)

# Step 2: clean
cps$skip_cut <- ifelse(cps$FSSKIPMOCAT <= 10, 1, NA)

# Step 3: confirm
table(cps$FSSKIPMOCAT, cps$skip_cut, useNA = "ifany")



##### Clean weight loss
# Step 1: examine
table(cps$FSLOSEWTM)

# Step 2: clean
cps$lost_weight <- ifelse(cps$FSLOSEWTM == 1, 0,
                         ifelse(cps$FSLOSEWTM == 2, 1, NA))

# Step 3: confirm
table(cps$FSLOSEWTM, cps$lost_weight, useNA = "ifany")



##### Clean age
# Step 1: examine
table(cps$AGE)

# Step 2: clean  


# Step 3: confirm




##### Clean sex
# Step 1: examine
table(cps$SEX)

# Step 2: clean
cps$man <- ifelse(cps$SEX == 1, 1, 0)
cps$woman <- ifelse(cps$SEX == 2, 1, 0)

# Step 3: confirm
table(cps$SEX, cps$man, useNA = "ifany")
table(cps$SEX, cps$woman, useNA = "ifany")

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
cps$married <- ifelse(cps$MARST == 1 | cps$MARST == 2, 1, 
                      ifelse(cps$MARST == 9, NA, 0))

cps$not_married <- ifelse(cps$MARST >= 3 & cps$MARST <= 7, 1, 
                      ifelse(cps$MARST == 9, NA, 0))



#Step 3: confirm
table(cps$MARST, cps$married, useNA = "ifany")
table(cps$MARST, cps$not_married, useNA = "ifany")


##### Clean education
# Step 1: examine
table(cps$EDUC)

# Step 2: clean
cps$hs_or_less <- ifelse(cps$EDUC <= 073, 1, NA)
cps$associates <- ifelse(cps$EDUC == 091, 1, NA)
cps$bachelors <- ifelse(cps$EDUC == 111, 1, NA)
cps$grad_school <- ifelse(cps$EDUC >= 123 & cps$EDUC <= 125, 1, NA)

# Step 3: confirm
table(cps$EDUC, cps$hs_or_less, useNA = "ifany")
table(cps$EDUC, cps$associates, useNA = "ifany")
table(cps$EDUC, cps$bachelors, useNA = "ifany")
table(cps$EDUC, cps$grad_school, useNA = "ifany")


##### Clean employment status
# Step 1: examine
table(cps$EMPSTAT)

# Step 2: clean
cps$ILF <- ifelse(cps$EMPSTAT >= 01 & cps$EMPSTAT <= 22, 1,
                  ifelse(cps$EMPSTAT == 00, NA, 0))

cps$NILF <- ifelse(cps$EMPSTAT > 22, 1,
                   ifelse(cps$EMPSTAT == 00, NA, 0))

# Step 3: confirm 
table(cps$EMPSTAT, cps$ILF, useNA = "ifany")
table(cps$EMPSTAT, cps$NILF, useNA = "ifany")



##### Clean family income
#Step 1: examine
table(cps$FAMINC)

# Step 2: clean
cps$less_than_50k <- ifelse(cps$FAMINC <= 740, 1,
                            ifelse(cps$FAMINC >=995, NA, 0))

cps$from_50k_to_100k <- ifelse(cps$FAMINC >= 800 & cps$FAMINC <= 841, 1,
                               ifelse(cps$FAMINC >=995, NA, 0))

cps$more_than_100k <- ifelse(cps$FAMINC >= 842 & cps$FAMINC <= 994, 1,
                             ifelse(cps$FAMINC >=995, NA, 0))


# Step 3: confirm
table(cps$FAMINC, cps$less_than_50k, useNA = "ifany")
table(cps$FAMINC, cps$from_50k_to_100k, useNA = "ifany")
table(cps$FAMINC, cps$more_than_100k, useNA = "ifany")



##### Clean metro
# Step 1: Examine
table(cps$METRO)


# Step 2:  clean by removing missing
cps$msa <- ifelse(cps$METRO == 1, 0, 
                  ifelse(cps$METRO >= 2 & cps$METRO <=4, 1, NA))

# step 3: confirm
table(cps$METRO, cps$msa, useNA = "ifany")



##### Clean difficulty with mobility
# Step 1: examine
table(cps$DIFFMOB)

# Step 2: clean
cps$no_mob_limit <-ifelse(cps$DIFFMOB == 1, 1,
                          ifelse(cps$DIFFMOB == 0, NA, 0))
cps$mob_limit <-ifelse(cps$DIFFMOB == 2, 1,
                       ifelse(cps$DIFFMOB == 0, NA, 0))


# Step 3: confirm
table(cps$DIFFMOB, cps$no_mob_limit, useNA = "ifany")
table(cps$DIFFMOB, cps$mob_limit, useNA = "ifany")





###############################################################################
#################            STEP 3: Create complete case dataset      #####################
###############################################################################

my_full_varlist <- c("grocery_food", "prepared_food", "alt_food", "other_food", "secure", 
                "low_secure", "very_low_secure", "emergency_food", "food_stamp", "skip_cut", 
                "lost_weight", "less_than_50k", "from_50k_to_100k", "more_than_100k", "AGE",  
full_varlist <- c("grocery_food", "prepared_food", "alt_food", "other_food", "secure", 
                "low_secure", "very_low_secure", "emergency_food", "food_stamp", "skip_cut", 
                "lost_weight", "less_than_50k", "from_50k_to_100k", "more_than_100k", 
                "man", "woman", "white", "black", "native", "asian", "mixed", "married",
                "not_married", "hs_or_less", "associates", "bachelors", "grad_school", 
                "ILF", "NILF", "msa", "no_mob_limit", "mob_limit")

my_varlist <- c("grocery_food", "prepared_food", "alt_food", "other_food", "secure", 
                "low_secure", "very_low_secure", "food_stamp", "less_than_50k",
                "from_50k_to_100k", "more_than_100k", "AGE", "man", "woman", "white",
                "black", "native", "asian", "mixed", "married", "not_married",
                "ILF", "NILF", "msa", "no_mob_limit", "mob_limit")
my_varlist <- c("grocery_food", "food_stamp", "secure","low_secure", "very_low_secure",
                "prepared_food", "alt_food", "other_food", "man", "woman")


### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- cps %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


###############################################################################
#################            STEP 4:Descriptive Statistics      #####################
###############################################################################

describe(my_dataset)

###############################################################################
#################            STEP 5: Regression analysis       #####################
###############################################################################


table(my_dataset$food_stamp, my_dataset$grocery_food)