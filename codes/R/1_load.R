## ---- PREAMBLE
set.seed(42)

library(knitr)
library(nFactors)
library(polycor)
library(random.polychor.pa)
library(foreign)
library(ggplot2)
library(psych)
library(mirt)
library(cowplot)
library(tikzDevice)
library(lavaan)
library(semTools)
library(np)
library(gdata)
library(xtable)
library(cowplot)
library(tikzDevice)
library(gtools)
library(here)
library(rio)
library(Counterfactual)
library(dummies)
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)


dir_data  <- here("rdata/")
dir_syntax  <- here("codes/R/syntax/")

# define printfit function
printfit <- function(m) {
  c( fitMeasures(m, c("npar", "df", "rmsea", "mfi", "cfi.scaled")), moreFitIndices(m)["gammaHat"] )
}

## ---- LOAD_DATA

# auxiliary variables to retain
auxvars <- c("sex", "region", "ethn", "fscl", "mscl",
             "bwt", "lowbwt", "parity", "firstb", "nprevst", "caesbirth", "smkpr", "gestaw", "preterm", 
             "mothageb", "teenm", "singlem", "mheight", "mempl",
             "mysch5", "fysch5", "numch5", "ageint5", "mhied5", "mempl5", "mpsla5", "fpsla5", "fscl5", "mscl5",
             "faminc10_real", "faminc10_infl", "incq10", "scl10"
             )

# BCS -------------------------------------------
bcs5data <- read.dta(paste0(dir_data, "bcs5yeng.dta"), convert.factors = F) # all BCS data
bcs5rutb <- bcs5data[,grep("bcs5_rutb", names(bcs5data), value=TRUE)]                 # BINARY Rutter items only
bcs5rutc <- bcs5data[,grep("bcs5_rutc", names(bcs5data), value=TRUE)]                 # 3CAT Rutter items only

# BINARY VERSION
# merge Rutter items 4 and 19
bcs5_rutb419 <- bcs5rutb[,"bcs5_rutb4"]
bcs5_rutb419[which(bcs5rutb[,"bcs5_rutb19"] == 1)] <- 1
# merge extra items A and B
bcs5_rutbAB <- bcs5rutb[,"bcs5_rutbA"]
bcs5_rutbAB[which(bcs5rutb[,"bcs5_rutbB"] == 0)] <- 0
# assemble final data
bcs5rutb <- bcs5rutb[,!names(bcs5rutb) %in% c("bcs5_rutb4", "bcs5_rutb19", "bcs5_rutbA", "bcs5_rutbB")]  # remove merged
bcs5rutb$bcs5_rutb419 <- bcs5_rutb419
bcs5rutb$bcs5_rutbAB <- bcs5_rutbAB
# version with factor (binary)
bcs5rutbf <- bcs5rutb
for (i in 1:ncol(bcs5rutb)) bcs5rutbf[,i] <- as.ordered(bcs5rutb[,i])


# 3CAT version
# merge Rutter items 4 and 19 (3cat)
bcs5_rutc419 <- apply(cbind(bcs5rutc[,"bcs5_rutc4"], bcs5rutc[,"bcs5_rutc19"]), 1, min)
# assemble final data (3cat)
bcs5rutc <- bcs5rutc[,!names(bcs5rutc) %in% c("bcs5_rutc4", "bcs5_rutc19", "bcs5_rutc14", "bcs5_rutc5")]  # remove merged (4,19) and 14,5 (binary for comparability)
bcs5rutc$bcs5_rutc419 <- bcs5_rutc419
bcs5rutc$bcs5_rutbAB <- bcs5_rutbAB
bcs5rutc$bcs5_rutbC <- bcs5rutb$bcs5_rutbC   # get extra items from binary version
bcs5rutc$bcs5_rutbD <- bcs5rutb$bcs5_rutbD
bcs5rutc$bcs5_rutb14 <- bcs5rutb$bcs5_rutb14
bcs5rutc$bcs5_rutb5 <- bcs5rutb$bcs5_rutb5

# version with factor (3cat)
bcs5rutcf <- bcs5rutc
for (i in 1:ncol(bcs5rutc)) bcs5rutcf[,i] <- as.ordered(bcs5rutc[,i])


# auxiliary variables
bcs5aux <- cbind(bcs5data[,c("bcsid", 
                             auxvars)]
)
names(bcs5aux)[names(bcs5aux)=="bcsid"] <- "id"
bcs5aux$rwtd <- 1

# MCS -------------------------------------------
mcs5data <- read.dta(paste(dir_data, "mcs5yeng.dta", sep=""), convert.factors = F) # all MCS data

# items to keep (binary version)
mcskeepb <- c(2,3,5,6,7,8,10,12,13,14,15,16,18,19,22,23,24)  # excluding prosocial scale (itm 1 4 9 17 20)
# & three positively worded items (itm 11 21 25)
mcs5sdqb <- mcs5data[,paste("mcs5_sdqb", mcskeepb, sep="")]         # BINARY SDQ items only      

# version with factor (3cat)
mcs5sdqbf <- mcs5sdqb
for (i in 1:ncol(mcs5sdqb)) mcs5sdqbf[,i] <- as.ordered(mcs5sdqb[,i]) 

# items to keep (3cat version)
mcs5sdqc <- mcs5data[,c("mcs5_sdqc2", 
                        "mcs5_sdqb3",  # binary (for comparability)
                        "mcs5_sdqb5",  # binary (for comparability)
                        "mcs5_sdqc6",
                        "mcs5_sdqb7",  # binary (for comparability)
                        "mcs5_sdqc8",
                        "mcs5_sdqc10",
                        "mcs5_sdqc12",
                        "mcs5_sdqc13",
                        "mcs5_sdqb14", # binary (for comparability)
                        "mcs5_sdqc15",
                        "mcs5_sdqc16",
                        "mcs5_sdqc18",
                        "mcs5_sdqc19",
                        "mcs5_sdqc22",
                        "mcs5_sdqc23",
                        "mcs5_sdqc24")]

# version with factor (3cat)
mcs5sdqcf <- mcs5sdqc
for (i in 1:ncol(mcs5sdqc)) mcs5sdqcf[,i] <- as.ordered(mcs5sdqc[,i])                    

# add SES and cognitive data
mcs5aux <- cbind(mcs5data[,c("mcsid", 
                             auxvars, 
                             "rwtd")]
)
names(mcs5aux)[names(mcs5aux)=="mcsid"] <- "id"

# MERGE -------------------------------------------

# common items
Xtemp.bcs <- cbind( bcs5aux,      # additional variables
                    matrix("BCS", dim(bcs5rutc)[1], 1),                                                             # cohort identifier
                    bcs5rutcf[,c("bcs5_rutc1", "bcs5_rutc2", "bcs5_rutc419", "bcs5_rutc15", "bcs5_rutbD", "bcs5_rutb14")], # EXT
                    bcs5rutcf[,c("bcs5_rutc6", "bcs5_rutc16", "bcs5_rutc7", "bcs5_rutc9", "bcs5_rutbAB")]                # INT
)
Xtemp.mcs <- cbind( mcs5aux,      # additional variables
                    matrix("MCS", dim(mcs5sdqc)[1], 1),                                                              # cohort identifier
                    mcs5sdqcf[,c("mcs5_sdqc2", "mcs5_sdqc10", "mcs5_sdqc12", "mcs5_sdqc15", "mcs5_sdqb5", "mcs5_sdqb7")], # EXT
                    mcs5sdqcf[,c("mcs5_sdqc8", "mcs5_sdqc16", "mcs5_sdqc6", "mcs5_sdqc13", "mcs5_sdqb3")]               # INT
)

# append BCS and MCS
colnames(Xtemp.bcs) <- c(colnames(bcs5aux), "cohort", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
colnames(Xtemp.mcs) <- c(colnames(mcs5aux), "cohort", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
X.all <- data.frame(rbind(Xtemp.bcs, Xtemp.mcs))

# assemble final data
items.c <- X.all[,c(grep("X", names(X.all), value=T), 
                    "id", "cohort", auxvars, "rwtd")]
colnames(items.c)[colnames(items.c) == "ageint5"] <- "age"
items.c$sex <- factor(items.c$sex)
levels(items.c$sex) = c("M", "F")
items.c$cohortsex <- interaction(items.c[c("cohort","sex")]) # generate interaction
items.c$cohortsex <- factor(items.c$cohortsex,levels(items.c$cohortsex)[c(1,3,2,4)]) # reorder
levels(items.c$cohortsex) <- c("BCS.M", "BCS.F", "MCS.M", "MCS.F")
items.c <- items.c[!is.na(items.c$cohortsex),] # drop missings

# keep only complete cases in X
items.c <- items.c[complete.cases(items.c[,c(grep("X[0-9]", names(X.all), value=T),"age","sex")]),]

# add raw scores
items.c$EXT_RAW <- rowSums(apply(items.c[,paste("X", seq(1,6), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
items.c$INT_RAW <- rowSums(apply(items.c[,paste("X", seq(7,11), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
items.c$EXT_RAWr <- residuals(lm(EXT_RAW ~ age, data=items.c, na.action = na.exclude))
items.c$INT_RAWr <- residuals(lm(INT_RAW ~ age, data=items.c, na.action = na.exclude))


# FINAL CLEANING/RECODING

# social class
for (s in c("scl10", "mscl", "mscl5")) items.c[,s] <- factor(items.c[,s], labels = c("I", "II", "IIINM", "IIIM", "IV", "V", "other"))
for (s in c("fscl", "fscl5")) items.c[,s] <- factor(items.c[,s], labels = c("I", "II", "IIINM", "IIIM", "IV", "V", "other", "no fath."))

# collapsed social class (blue vs white collar)
for (s in c("scl10","mscl","mscl5","fscl", "fscl5")) {
  items.c[,paste0(s,"wb")] <- dplyr::recode(items.c[,s], 
                               "I" = "White collar",
                               "II" = "White collar",
                               "IIINM" = "White collar",
                               "IIIM" = "Blue collar",
                               "IV" = "Blue collar",
                               "V" = "Blue collar",
                               "other" = "Blue collar",
                               "no fath." = "No father fig."
  )
}

# recode social class at 10
items.c$scl10b <- NA
items.c$scl10b[items.c$scl10 %in% c("I", "II")] <- 4
items.c$scl10b[items.c$scl10 %in% c("IIINM")] <- 3
items.c$scl10b[items.c$scl10 %in% c("IIIM")] <- 2
items.c$scl10b[items.c$scl10 %in% c("IV", "V")] <- 1
items.c$scl10b[items.c$scl10 %in% c("other")] <- 5
items.c$scl10b <- factor(items.c$scl10b, labels = c("IV V","IIIM","IIINM","I II","oth"))

# recode years of schooling for plots
items.c$mysch5b <- cut(items.c$mysch5, c(-1,15,16,18,21,31))
levels(items.c$mysch5b) <- c("15", "16", "17-18", "19-21", "$>$21")

# convert to factor
facnms <- c("smkpr", "region", "lowbwt", "caesbirth", "preterm", "firstb", "teenm", "singlem", "mempl", "mempl5", "mhied5", "mpsla5", "fpsla5")
items.c[,facnms] <- lapply(items.c[,facnms] , factor)
items.c$incq10 <- ordered(items.c$incq10)
levels(items.c$mempl5) <- c("Unempl./At home", "Part time", "Full time")
items.c$mempl5b <- dplyr::recode(items.c$mempl5, "Full time" = "Employed", "Part time" = "Employed")
levels(items.c$mpsla5) <- c("Compulsory", "Post-Compulsory")
levels(items.c$singlem) <- c("Married", "Not married")
levels(items.c$smkpr) <- c("Non-smoker", "Smoker")

# log birthweight
items.c$lbwt <- log(items.c$bwt)

# missing gestational age
items.c$preterm2 <- as.character(items.c$preterm)
items.c[is.na(items.c$preterm2),"preterm2"] <- "Missing"
items.c$preterm <- factor(items.c$preterm2)
levels(items.c$preterm) <- c("Term", "Preterm", "Missing")
items.c <- items.c[ , !(names(items.c) %in% "preterm2")]

# save dataset
export(items.c[,!names(items.c) %in% c("INT_RAW", "EXT_RAW", "INT_RAWr", "EXT_RAWr")], paste0(dir_data,"cohorts_all.dta"))

# KEEP ONLY REWEIGHTED SAMPLE
items.c2 <- items.c
items.c <- items.c[items.c$rwtd==1,]

# MODEL LIST
# 1 (MAIN) : separate gender groups, no age adjustment (4 groups, BCS.M BCS.F MCS.M MCS.F)
# 2 (AGECHECK): separate gender groups, overlapping ages, no age adjustment (4 groups, BCS.M BCS.F MCS.M MCS.F)

# 3: no gender split, no age adjustment (2 groups, BCS MCS)
# 4: males only, no age adjustment (2 groups, BCS MCS)
# 5: females only, no age adjustment (2 groups, BCS MCS)
# 6: no gender split, age adjustment (2 groups, BCS MCS)
# 7: males only, age adjustment (2 groups, BCS MCS)
# 8: females only, age adjustment (2 groups, BCS MCS)
# 9: separate gender groups, age adjustment (4 groups, BCS.M BCS.F MCS.M MCS.F)
# 10: separate gender groups, age adjustment constrained to be the same across ages (4 groups, BCS.M BCS.F MCS.M MCS.F)
# 11: MIMIC model with age as regressor (4 groups, BCS.M BCS.F MCS.M MCS.F)

# list of items for different models
items <- list()
items[[1]] <- items.c
items[[2]] <- subset(items.c, age>=59 & age<=62)
items[[3]] <- items.c
items[[4]] <- subset(items.c, sex=="M")
items[[5]] <- subset(items.c, sex=="F")
items[[6]] <- items.c
items[[7]] <- subset(items.c, sex=="M")
items[[8]] <- subset(items.c, sex=="F")
items[[9]] <- items.c
items[[10]] <- items.c
items[[11]] <- items.c

# order to merge correctly with scores
for (i in 1:length(items)) items[[i]] <- items[[i]][order(items[[i]][,"cohortsex"]) , ]


## ---- MEANTABLE
# table of mean values of items

ncomm <- 11                           # number of common items
ncats <- c(3,3,3,3,2,2,3,3,3,3,2)     # number of categories per item

means <- list()
X.allg <- list()
meantab <- list()
means[[1]] <- matrix(NA, ncomm, 7)    # males
means[[2]] <- matrix(NA, ncomm, 7)    # females
X.allg[[1]] <- X.all[X.all$sex==1,]
X.allg[[2]] <- X.all[X.all$sex==2,]

for (g in c(1,2)) {                   # males/females
  colnames(means[[g]]) <- c("num", "BCS_ca", "BCS_sa", "BCS_a", "MCS_ca", "MCS_sa", "MCS_a")
  
  for (i in 1:ncomm) {                # items
    means[[g]][i,1] <- i
    t <- table(X.allg[[g]]$cohort, X.allg[[g]][,grep("X[0-9]", names(X.allg[[g]]))][,i]) # ith column of item-only matrix
    t2 <- 100*prop.table(t, 1)                           # convert to row percentages
    
    if (ncats[i] == 3) { # 3-category items
      means[[g]][i,2] <- t2[1,1]
      means[[g]][i,3] <- t2[1,2]
      means[[g]][i,5] <- t2[2,1]
      means[[g]][i,6] <- t2[2,2]
    }
    if (ncats[i] == 2) { # binary items
      means[[g]][i,4] <- t2[1,1]
      means[[g]][i,7] <- t2[2,1]
    }
  }
  
  # table with means
  meantab[[g]]      <- data.frame(row.names = paste("Item", seq(1,ncomm)))
  meantab[[g]]$num  <- seq(1,ncomm)     # item number
  meantab[[g]]$fac  <- c(rep("EXT", 6), rep("INT", 5))                                            # factor
  meantab[[g]]$cats <- as.character(ncats)                                                       # number of categories
  
  meantab[[g]]$bcs.text <- c(
    "Very restless. Often running about or jumping up and down. Hardly ever still",
    "Is squirmy or fidgety",
    "Frequently fights other children + \\newline Bullies other children",
    "Cannot settle to anything for more than a few moments",
    "Has temper tantrums",
    "Is often disobedient",
    "Often worried, worries about many things",
    "Tends to be fearful or afraid of new things or new situations",
    "Tends to do things on his/her own – rather solitary",
    "Often appears miserable, unhappy, tearful or distressed",
    "Complains of headaches + \\newline Complains of stomach-ache or has vomited"
  )
  meantab[[g]]$bcs.num <- c("1", "2", "4 \\newline 19", "15", "D", "14", "6", "16", "7", "9", "A \\newline B")   # BCS item numbers
  meantab[[g]] <- merge(meantab[[g]], means[[g]][,c("num", "BCS_ca", "BCS_sa", "BCS_a")], by="num")                  # merge matrix of means for BCS
  
  meantab[[g]]$mcs.text <- c(
    "Restless, overactive, cannot stay still for long",
    "Constantly fidgeting or squirming",
    "Often fights with other children or bullies them",
    "Easily distracted, concentration wanders",
    "Often has temper tantrums or hot tempers",
    "(+) Generally obedient, usually does what adults request",
    "Many worries, often seems worried",
    "Nervous or clingy in new situations, easily loses confidence",
    "Rather solitary, tends to play alone",
    "Often unhappy, down-hearted or tearful",
    "Often complains of head- aches, stomach-ache or sickness"
  )
  meantab[[g]]$mcs.num <- c("2", "10", "12", "15", "5", "7", "8", "16", "6", "13", "3")    # MCS item numbers
  meantab[[g]] <- merge(meantab[[g]], means[[g]][,c("num", "MCS_ca", "MCS_sa", "MCS_a")], by="num")                  # merge matrix of means for BCS
  
}

# colour coding

meantab_cc2 <- meantab
meantab_cc3 <- meantab
for (g in c(1,2)) {
  # 2 factors
  meantab_cc2[[g]][meantab_cc2[[g]]$fac=="EXT","bcs.text"] <- paste0("{\\color{OliveGreen}", meantab_cc2[[g]][meantab_cc2[[g]]$fac=="EXT","bcs.text"], "}")
  meantab_cc2[[g]][meantab_cc2[[g]]$fac=="EXT","mcs.text"] <- paste0("{\\color{OliveGreen}", meantab_cc2[[g]][meantab_cc2[[g]]$fac=="EXT","mcs.text"], "}")  
  meantab_cc2[[g]][meantab_cc2[[g]]$fac=="INT","bcs.text"] <- paste0("{\\color{Orange}", meantab_cc2[[g]][meantab_cc2[[g]]$fac=="INT","bcs.text"], "}")
  meantab_cc2[[g]][meantab_cc2[[g]]$fac=="INT","mcs.text"] <- paste0("{\\color{Orange}", meantab_cc2[[g]][meantab_cc2[[g]]$fac=="INT","mcs.text"], "}")
  
  # 3 factors
  meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(1,2,4),"bcs.text"] <- paste0("{\\color{Fuchsia}", meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(1,2,4),"bcs.text"], "}")
  meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(3,5,6),"bcs.text"] <- paste0("{\\color{OliveGreen}", meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(3,5,6),"bcs.text"], "}")
  meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(1,2,4),"mcs.text"] <- paste0("{\\color{Fuchsia}", meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(1,2,4),"mcs.text"], "}")
  meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(3,5,6),"mcs.text"] <- paste0("{\\color{OliveGreen}", meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(3,5,6),"mcs.text"], "}")
  
  meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(7,8,10),"bcs.text"] <- paste0("{\\color{Orange}", meantab_cc3[[g]][meantab_cc3[[g]]$num %in% c(7,8,10),"bcs.text"], "}")
  meantab_cc3[[g]][meantab_cc3[[g]]$fac=="INT","mcs.text"] <- paste0("{\\color{Orange}", meantab_cc3[[g]][meantab_cc3[[g]]$fac=="INT","mcs.text"], "}")
}

rm(mcskeepb, ncats, ncomm, t, t2, X.all, X.allg, means, facnms)



