# Author:   Mihir Trivedi
# Created:  12/09/2016
#**********************************************************
# PART I----
require(data.table)

# Load MIDAS data----
system.time(load("C:/MIDAS/midas14.RData"))
gc()

# Remove unused variables
midas14[, LOCATION := NULL]
midas14[, CAUSE := NULL]
midas14[, DEATHRNUM := NULL]
midas14[, ZIP := NULL]
#midas14[, STATUS := NULL]
midas14[, SOURCE := NULL]
midas14[, TOTBIL := NULL]
midas14[, DRG := NULL]
midas14[, RECDID := NULL]
midas14[, DSHYR := NULL]
midas14[, PRDTE1 := NULL]
midas14[, PRDTE2 := NULL]
midas14[, PRDTE3 := NULL]
midas14[, PRDTE4 := NULL]
midas14[, PRDTE5 := NULL]
midas14[, PRDTE6 := NULL]
midas14[, PRDTE7 := NULL]
midas14[, PRDTE8 := NULL]
midas14[, SECOND := NULL]
midas14[, THIRD := NULL]
midas14[, PRIME := NULL]
gc()

# Number of patients
length(unique(midas14$PATIENT_ID))
# 4,623,503

# Convert dates
midas14[, NEWDTD := as.Date(NEWDTD, format = "%m/%d/%Y")]
midas14[, ADMDAT := as.Date(ADMDAT, format = "%m/%d/%Y")]
midas14[, DSCHDAT := as.Date(DSCHDAT, format = "%m/%d/%Y")]
midas14[, PATBDTE := as.Date(PATBDTE, format = "%m/%d/%Y")]

# Check the person born in 1876
subset(midas14, PATBDTE == "1876-03-08")
# Typo. Other records of this patient show 1976
subset(midas14, PATIENT_ID == "200000107751")



# Convert to factors
# Sex
midas14[, SEX := factor(SEX, levels = c("F", "M"))]

# Race
table(midas14$RACE)
midas14$RACE1 <- "Other"
midas14$RACE1[midas14$RACE == 1] <- "White"
midas14$RACE1[midas14$RACE == 2] <- "Black"
midas14[, RACE1 := factor(RACE1,
                          levels = c("White",
                                     "Black",
                                     "Other"))]
table(midas14$RACE1)
midas14[, RACE := NULL]
names(midas14)[ncol(midas14)] <- "RACE"

summary(midas14)
gc()

setkey(midas14, ADMDAT)
setkey(midas14, PATIENT_ID)
midas14[, NDX := 1:.N,
        by = PATIENT_ID]

# Number of unique ICD codes
length(unique(midas14$DX1))
length(unique(c(midas14$DX1,
                midas14$DX2,
                midas14$DX3,
                midas14$DX4,
                midas14$DX5,
                midas14$DX6,
                midas14$DX7,
                midas14$DX8,
                midas14$DX9)))

#length(unique(midas14$PROC1))
#length(unique(c(midas14$PROC1,
#                midas14$PROC2,
#                midas14$PROC3,
#                midas14$PROC4,
#                midas14$PROC5,
#                midas14$PROC6,
#                midas14$PROC7,
#                midas14$PROC8)))

# Keep only records from 01/01/1990 (admission dates)
midas14 <- subset(midas14, ADMDAT >= "1990-01-01")
gc()

#**********************************************************

# 3. Remove anybody who was younger than 18 at any admisson
midas14[, AGE := floor(as.numeric(difftime(midas14$ADMDAT, 
                                           midas14$PATBDTE,
                                           units = "days"))/365.25)]
#hist(midas14$AGE, 100)
id.rm <- midas14$PATIENT_ID[midas14$AGE < 40]
midas14 <- subset(midas14, !(PATIENT_ID %in% id.rm))
gc()
#hist(midas14$AGE, 100)

#**********************************************************




save(midas14, dx, dx.1.3, proc, file = "C:/MIDAS/tmp/midas14_clean_11232016.RData", compress = FALSE)

