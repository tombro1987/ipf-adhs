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
adhs$male_s <- NA
adhs$male_s[adhs$Sex == 1] <- 1
adhs$male_s[adhs$Sex == 2] <- 0
adhs$female_s <- NA
adhs$female_s[adhs$Sex == 2] <- 1
adhs$female_s[adhs$Sex == 1] <- 0

# NSSEC8
adhs$NSSEC8[adhs$NSSEC8 == 97] <- NA
adhs$ns11_s <- NA
adhs$ns12_s <- NA
adhs$ns2_s  <- NA
adhs$ns3_s  <- NA
adhs$ns4_s  <- NA
adhs$ns5_s  <- NA
adhs$ns6_s  <- NA
adhs$ns7_s  <- NA
adhs$ns8_s  <- NA
adhs$ns11_s[adhs$NSSEC8 == 1.1] <- 1
adhs$ns11_s[adhs$NSSEC8 >  1.1] <- 0
adhs$ns12_s[adhs$NSSEC8 == 1.2] <- 1
adhs$ns12_s[adhs$NSSEC8 == 1.1 | adhs$NSSEC8 > 1.2] <- 0
adhs$ns2_s[adhs$NSSEC8 == 2]   <- 1
adhs$ns2_s[adhs$NSSEC8 < 2 | adhs$NSSEC8 > 2]   <- 0
adhs$ns3_s[adhs$NSSEC8 == 3]   <- 1
adhs$ns3_s[adhs$NSSEC8 < 3 | adhs$NSSEC8 > 3]   <- 0
adhs$ns4_s[adhs$NSSEC8 == 4]   <- 1
adhs$ns4_s[adhs$NSSEC8 < 4 | adhs$NSSEC8 > 4]   <- 0
adhs$ns5_s[adhs$NSSEC8 == 5]   <- 1
adhs$ns5_s[adhs$NSSEC8 < 5 | adhs$NSSEC8 > 5]   <- 0
adhs$ns6_s[adhs$NSSEC8 == 6]   <- 1
adhs$ns6_s[adhs$NSSEC8 < 6 | adhs$NSSEC8 > 6]   <- 0
adhs$ns7_s[adhs$NSSEC8 == 7]   <- 1
adhs$ns7_s[adhs$NSSEC8 < 7 | adhs$NSSEC8 > 7]   <- 0
adhs$ns8_s[adhs$NSSEC8 == 8]   <- 1
adhs$ns8_s[adhs$NSSEC8 < 8 | adhs$NSSEC8 > 8]   <- 0



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
names(cen) <- c("lsoacode", "male_c", "female_c")
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
names(sec) <- c("lsoacode", "ns11_c", "ns12_c", "ns2_c", "ns3_c", "ns4_c", 
                "ns5_c", "ns6_c", "ns7_c", "ns8_c")
cen <- merge(cen, sec, by.x = "lsoacode", by.y = "lsoacode")
rm(sec)
View(adhs)
keep <- c("Serial", "SHA", "Treat", "male_s", "female_s",
          "ns11_s", "ns12_s", "ns2_s", "ns3_s", "ns4_s", "ns5_s", 
          "ns6_s", "ns7_s", "ns8_s")
adhs <- adhs[keep]
rm(keep)

adhs <- na.omit(adhs)
adhs <- as.data.frame(adhs)

adhs$key <- 999
cen$key <- 999

adhs.yh <- adhs[adhs$SHA == 3, ]
cen.s <- cen[sample(1:nrow(cen), 1000, replace = F), ]
rm(adhs, cen)

apply(adhs.yh[c("Treat",  "male_s",	"female_s",	"ns11_s",	"ns12_s",	"ns2_s",
              "ns3_s",	"ns4_s",	"ns5_s",	"ns6_s",	"ns7_s",	"ns8_s")],
      MARGIN = 2,
      sum)

# lsoa1 <- merge(adhs.yh, cen.s[1, ], by.x = "key", by.y = "key")
# lsoa1$wt0 <- 1
# varlist <- c("male_s",  "female_s"	ns11	ns12	ns2	ns3	ns4	ns5	ns6	ns7	ns8)
# for (i in varlist){
#   #
# }



# apply(cen, MARGIN = 1, merge(adhs, cen, by = "key"))
