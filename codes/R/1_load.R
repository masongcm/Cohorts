
## ---- PREAMBLE
rm(list=ls())         # Clear all objects from memory 
set.seed(42)

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
library(difR)
library(np)
library(gdata)
library(knitr)
library(xtable)
library(cowplot)
library(tikzDevice)
library(gtools)


dir_data  <- c("/Users/giacomomason/Documents/Projects/CohortStudies/rdata/")
dir_syntax  <- c("/Users/giacomomason/Documents/Projects/CohortStudies/codes/R/syntax/")

# define printfit function
printfit <- function(m) {
  c( fitMeasures(m, c("npar", "df", "rmsea", "mfi", "cfi.scaled")), moreFitIndices(m)["gammaHat"] )
}

## ---- LOAD_DATA

# auxiliary variables to retain
auxvars <- c("faminc_real", "faminc_infl", "incq", "ysch_moth5", "ysch_fath5", "numch5", "ageint5",
             "sex", "bwt", "smkpr", "scl10", "gestaw", "region","mothageb")

# BCS -------------------------------------------
bcs5data <- read.dta(paste(dir_data, "bcs5yeng.dta", sep=""), convert.factors = F) # all BCS data
bcs5rutb <- bcs5data[,grep("bcs5_rutb", names(bcs5data), value=TRUE)]                 # BINARY Rutter items only
bcs5rutc <- bcs5data[,grep("bcs5_rutc", names(bcs5data), value=TRUE)]                 # 3CAT Rutter items only

# social class 
bcs5data$scl10 <- factor(bcs5data$scl10, labels = c("I", "II", "IIINM", "IIIM", "IV", "V", "other"))

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
bcs5aux <- cbind(bcs5data[,c("bcsid", auxvars, "hinvq00")],
                cog1 = bcs5data$epvt_z,
                cog2 = bcs5data$hfd_z,
                cog3 = bcs5data$copy_z
                )
names(bcs5aux)[names(bcs5aux)=="bcsid"] <- "id"

# MCS -------------------------------------------
mcs5data <- read.dta(paste(dir_data, "mcs5yeng_rwt.dta", sep=""), convert.factors = F) # all MCS data
# social class 
mcs5data$scl10 <- factor(mcs5data$scl10, labels = c("I", "II", "IIINM", "IIIM", "IV", "V", "other"))

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
mcs5aux <- cbind(mcs5data[,c("mcsid", auxvars)],
                cog1 = mcs5data$nvoc_bastz,
                cog2 = mcs5data$psim_bastz,
                cog3 = mcs5data$patc_bastz
)
names(mcs5aux)[names(mcs5aux)=="mcsid"] <- "id"
# add empty column with hinvq00 to match BCS
mcs5aux$hinvq00 <- NA

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
                "id", "cohort", auxvars, "hinvq00",
                "cog1", "cog2", "cog3")]
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
items.c$EXT.RAW <- rowSums(apply(items.c[,paste("X", seq(1,6), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
items.c$INT.RAW <- rowSums(apply(items.c[,paste("X", seq(7,11), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
items.c$EXT.RAWr <- residuals(lm(EXT.RAW ~ age, data=items.c, na.action = na.exclude))
items.c$INT.RAWr <- residuals(lm(INT.RAW ~ age, data=items.c, na.action = na.exclude))


# FINAL CLEANING/RECODING
# recode social class for plots
items.c$scl10b <- NA
items.c$scl10b[items.c$scl10 %in% c("I", "II")] <- 5
items.c$scl10b[items.c$scl10 %in% c("IIINM")] <- 4
items.c$scl10b[items.c$scl10 %in% c("IIIM")] <- 3
items.c$scl10b[items.c$scl10 %in% c("IV", "V")] <- 2
items.c$scl10b[items.c$scl10 %in% c("other")] <- 1
items.c$scl10b <- factor(items.c$scl10b, labels = c("oth","IV_V","IIIM","IIINM","I_II"))

# convert to factor
items.c$smkpr <- factor(items.c$smkpr)
items.c$region <- factor(items.c$region)
items.c$incq <- ordered(items.c$incq)


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

ncomm <- 11                             # number of common items
ncats <- c(3,3,3,3,2,2,3,3,3,3,2)     # number of categories per item

means <- matrix(NA, ncomm, 7)
colnames(means) <- c("num", "BCS_ca", "BCS_sa", "BCS_a", "MCS_ca", "MCS_sa", "MCS_a")

for (i in 1:ncomm) {
  means[i,1] <- i
  t <- table(X.all$cohort, X.all[,grep("X[0-9]", names(X.all))][,i]) # ith column of item-only matrix
  t2 <- 100*prop.table(t, 1)                           # convert to row percentages
  
  if (ncats[i] == 3) { # 3-category items
    means[i,2] <- t2[1,1]
    means[i,3] <- t2[1,2]
    means[i,5] <- t2[2,1]
    means[i,6] <- t2[2,2]
  }
  
  if (ncats[i] == 2) { # binary items
    means[i,4] <- t2[1,1]
    means[i,7] <- t2[2,1]
  }

}

# table with means
meantab <- data.frame(row.names = paste("Item", seq(1,ncomm)))
meantab$num <- seq(1,ncomm)     # item number
meantab$fac <- c(rep("EXT", 6), rep("INT", 5))                                            # factor
meantab$cats <- as.character(ncats)                                                       # number of categories

meantab$bcs.text <- c(
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
meantab$bcs.num <- c("1", "2", "4 \\newline 19", "15", "D", "14", "6", "16", "7", "9", "A \\newline B")   # BCS item numbers
meantab <- merge(meantab, means[,c("num", "BCS_ca", "BCS_sa", "BCS_a")], by="num")                  # merge matrix of means for BCS

meantab$mcs.text <- c(
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
meantab$mcs.num <- c("2", "10", "12", "15", "5", "7", "8", "16", "6", "13", "3")    # MCS item numbers
meantab <- merge(meantab, means[,c("num", "MCS_ca", "MCS_sa", "MCS_a")], by="num")                  # merge matrix of means for BCS




