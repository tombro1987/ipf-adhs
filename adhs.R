# Libraries ====
# install.packages("sjPlot")
# install.packages("ggplot2")
library(sjPlot)
library(ggplot2)


# Read in Adult Dental Health Survey 2009 ====
require(sjPlot)
adhs <- sji.SPSS("adult_dental_health_survey_2009_end_user_licence_270712.sav")
sji.viewSPSS(adhs)

# Cleansing
# adhs <- adhs[adhs$SHA == 3, ]
adhs$Treat[adhs$Treat == 2] <- 0
# remove people aged > 74
adhs <- adhs[adhs$ageband1 < 6, ]

# Use Treat - Need treatment if went to dentist tomorrow
table(adhs$Treat)
summary(adhs$Treat)
# good dist. Few NAs



# Prepare outcome variables in survey file ====
# Sex
adhs$male <- NA
adhs$male[adhs$Sex == 1] <- 1
adhs$male[adhs$Sex == 2] <- 0
adhs$female <- NA
adhs$female[adhs$Sex == 2] <- 1
adhs$female[adhs$Sex == 1] <- 0

# NSSEC8
adhs$NSSEC8[adhs$NSSEC8 == 97] <- NA
adhs$ns11 <- NA
adhs$ns12 <- NA
adhs$ns2  <- NA
adhs$ns3  <- NA
adhs$ns4  <- NA
adhs$ns5  <- NA
adhs$ns6  <- NA
adhs$ns7  <- NA
adhs$ns8  <- NA
adhs$ns11[adhs$NSSEC8 == 1.1] <- 1
adhs$ns11[adhs$NSSEC8 >  1.1] <- 0
adhs$ns12[adhs$NSSEC8 == 1.2] <- 1
adhs$ns12[adhs$NSSEC8 == 1.1 | adhs$NSSEC8 > 1.2] <- 0
adhs$ns2[adhs$NSSEC8 == 2]   <- 1
adhs$ns2[adhs$NSSEC8 < 2 | adhs$NSSEC8 > 2]   <- 0
adhs$ns3[adhs$NSSEC8 == 3]   <- 1
adhs$ns3[adhs$NSSEC8 < 3 | adhs$NSSEC8 > 3]   <- 0
adhs$ns4[adhs$NSSEC8 == 4]   <- 1
adhs$ns4[adhs$NSSEC8 < 4 | adhs$NSSEC8 > 4]   <- 0
adhs$ns5[adhs$NSSEC8 == 5]   <- 1
adhs$ns5[adhs$NSSEC8 < 5 | adhs$NSSEC8 > 5]   <- 0
adhs$ns6[adhs$NSSEC8 == 6]   <- 1
adhs$ns6[adhs$NSSEC8 < 6 | adhs$NSSEC8 > 6]   <- 0
adhs$ns7[adhs$NSSEC8 == 7]   <- 1
adhs$ns7[adhs$NSSEC8 < 7 | adhs$NSSEC8 > 7]   <- 0
adhs$ns8[adhs$NSSEC8 == 8]   <- 1
adhs$ns8[adhs$NSSEC8 < 8 | adhs$NSSEC8 > 8]   <- 0



# Read in Census files ====
cen <- read.csv("NSSEC.csv")
cen$test <- rowSums(cen[5:52])
cen <- (cen[cen$test > 0, ])
# table(duplicated(cen$geography.code)) # check for duplicates
cen <- as.data.frame(cen)

# Sex
keep <- c("geography.code",
          "Sex..Males..NS.SeC..All.categories..NS.SeC..measures..Value",
          "Sex..Females..NS.SeC..All.categories..NS.SeC..measures..Value")
cen <- cen[keep]
rm(keep)
names(cen) <- c("lsoacode", "male", "female")
cen <- as.data.frame(cen)

# NSSEC8
sec <- read.csv("NSSEC.csv")
sec$test <- rowSums(sec[5:52])
sec <- (sec[sec$test > 0, ])
# table(duplicated(sec$geography.code)) # check for duplicates
sec <- as.data.frame(sec)
keep <- c("geography.code", 
          "Sex..All.persons..NS.SeC..1.1.Large.employers.and.higher.managerial.and.administrative.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..1.2.Higher.professional.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..2..Lower.managerial..administrative.and.professional.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..3..Intermediate.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..4..Small.employers.and.own.account.workers..measures..Value",
          "Sex..All.persons..NS.SeC..5..Lower.supervisory.and.technical.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..6..Semi.routine.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..7..Routine.occupations..measures..Value",
          "Sex..All.persons..NS.SeC..8..Never.worked.and.long.term.unemployed..measures..Value")
sec <- sec[keep]
rm(keep)
names(sec) <- c("lsoacode", "ns11", "ns12", "ns2", "ns3", "ns4", 
                "ns5", "ns6", "ns7", "ns8")
cen <- merge(cen, sec, by.x = "lsoacode", by.y = "lsoacode")
rm(sec)

keep <- c("Serial", "SHA", "Treat", "male", "female",
          "ns11", "ns12", "ns2", "ns3", "ns4", "ns5", "ns6", "ns7", "ns8")
adhs <- adhs[keep]
rm(keep)

adhs <- na.omit(adhs)
adhs <- as.data.frame(adhs)
View(adhs)
View(cen)
