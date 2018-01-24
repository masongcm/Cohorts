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

# define printfit function
printfit <- function(m) {
  c( fitMeasures(m, c("npar", "df", "rmsea", "mfi", "cfi.scaled")), moreFitIndices(m)["gammaHat"] )
}

## ---- LOAD_DATA

# BCS -------------------------------------------
bcs5data <- read.dta(paste(dir_data, "bcs5yeng.dta", sep=""), convert.factors = F) # all BCS data
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
bcs5aux <- cbind(bcs5data[,c("bcsid", "faminc", "incq", "ysch_moth", "ysch_fath", "ageint5", "sex")],
                cog1 = bcs5data$epvt_z,
                cog2 = bcs5data$hfd_z,
                cog3 = bcs5data$copy_z
                )


# MCS -------------------------------------------
mcs5data <- read.dta(paste(dir_data, "mcs5yeng_rwt.dta", sep=""), convert.factors = F) # all MCS data

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
mcs5aux <- cbind(mcs5data[,c("mcsid", "faminc", "incq", "ysch_moth", "ysch_fath", "ageint5","sex")],
                cog1 = mcs5data$nvoc_bastz,
                cog2 = mcs5data$psim_bastz,
                cog3 = mcs5data$patc_bastz
)


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
nn <- c(colnames(bcs5aux), "cohort", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
nn[1] <- "ID"
colnames(Xtemp.bcs) <- nn
colnames(Xtemp.mcs) <- nn
X <- data.frame(rbind(Xtemp.bcs, Xtemp.mcs))

# assemble final data
cohort <- X[,"cohort"]
items.c <- X[,c(grep("X", names(X), value=T), "cohort")]   # noncog items only
items  <- items.c                              # numeric version of noncog items only
for (i in 1:ncol(items.c)) items[,i] <- as.numeric(items.c[,i]) - 1

# with age and sex
items.cb <- X[,c(grep("X", names(X), value=T), "cohort", "ageint5","sex")]
colnames(items.cb)[colnames(items.cb) == "ageint5"] <- "age"
items.cb$sex <- factor(items.cb$sex)

# with age, cohort+sex group
items.cc <- X[,c(grep("X", names(X), value=T), "cohort", "ageint5", "sex")]
colnames(items.cc)[colnames(items.cc) == "ageint5"] <- "age"
items.cc$cohortsex <- interaction(items.cc[c("cohort","sex")]) # generate interaction
items.cc$cohortsex <- factor(items.cc$cohortsex,levels(items.cc$cohortsex)[c(1,3,2,4)]) # reorder
levels(items.cc$cohortsex) <- c("BCS.M", "BCS.F", "MCS.M", "MCS.F")
items.cc <- items.cc[ , !(names(items.cc) %in% c("cohort","sex"))] # drop cohort and sex
items.cc <- items.cc[!is.na(items.cc$cohortsex),] # drop missings

## ---- MEANTABLE
# table of mean values of items

ncomm <- 11                             # number of common items
ncats <- c(3,3,3,3,2,2,3,3,3,3,2)     # number of categories per item

means <- matrix(NA, ncomm, 7)
colnames(means) <- c("num", "BCS_ca", "BCS_sa", "BCS_a", "MCS_ca", "MCS_sa", "MCS_a")

for (i in 1:ncomm) {
  means[i,1] <- i
  t <- table(X$cohort,X[,grep("X", names(X))][,i]) # ith column of item-only matrix
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




