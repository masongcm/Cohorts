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


dir_data  <- c("/Users/giacomomason/Documents/Projects/CohortStudies/data/")

# define printfit function
printfit <- function(m) {
  c( fitMeasures(m, c("npar", "df", "rmsea", "mfi", "cfi.scaled")), moreFitIndices(m)["gammaHat"] )
}

## ---- LOAD_DATA

# BCS -------------------------------------------
bcsdata <- read.dta(paste(dir_data, "bcsrut5yeng.dta", sep=""), convert.factors = F) # all BCS data
bcsrutb <- bcsdata[,grep("bcs_rutb", names(bcsdata), value=TRUE)]                 # BINARY Rutter items only
bcsrutc <- bcsdata[,grep("bcs_rutc", names(bcsdata), value=TRUE)]                 # 3CAT Rutter items only

# BINARY VERSION
# merge Rutter items 4 and 19
bcs_rutb419 <- bcsrutb[,"bcs_rutb4"]
bcs_rutb419[which(bcsrutb[,"bcs_rutb19"] == 1)] <- 1
# merge extra items A and B
bcs_rutbAB <- bcsrutb[,"bcs_rutbA"]
bcs_rutbAB[which(bcsrutb[,"bcs_rutbB"] == 0)] <- 0
# assemble final data
bcsrutb <- bcsrutb[,!names(bcsrutb) %in% c("bcs_rutb4", "bcs_rutb19", "bcs_rutbA", "bcs_rutbB")]  # remove merged
bcsrutb$bcs_rutb419 <- bcs_rutb419
bcsrutb$bcs_rutbAB <- bcs_rutbAB
# version with factor (binary)
bcsrutbf <- bcsrutb
for (i in 1:ncol(bcsrutb)) bcsrutbf[,i] <- as.ordered(bcsrutb[,i])


# 3CAT version
# merge Rutter items 4 and 19 (3cat)
bcs_rutc419 <- apply(cbind(bcsrutc[,"bcs_rutc4"], bcsrutc[,"bcs_rutc19"]), 1, min)
# assemble final data (3cat)
bcsrutc <- bcsrutc[,!names(bcsrutc) %in% c("bcs_rutc4", "bcs_rutc19", "bcs_rutc14", "bcs_rutc5")]  # remove merged (4,19) and 14,5 (binary for comparability)
bcsrutc$bcs_rutc419 <- bcs_rutc419
bcsrutc$bcs_rutbAB <- bcs_rutbAB
bcsrutc$bcs_rutbC <- bcsrutb$bcs_rutbC   # get extra items from binary version
bcsrutc$bcs_rutbD <- bcsrutb$bcs_rutbD
bcsrutc$bcs_rutb14 <- bcsrutb$bcs_rutb14
bcsrutc$bcs_rutb5 <- bcsrutb$bcs_rutb5

# version with factor (3cat)
bcsrutcf <- bcsrutc
for (i in 1:ncol(bcsrutc)) bcsrutcf[,i] <- as.ordered(bcsrutc[,i])


# auxiliary variables
bcsaux <- cbind(bcsdata[,c("bcsid", "faminc", "incq", "ysch_moth", "ysch_fath", "ageint5")],
                cog1 = bcsdata$epvt_z,
                cog2 = bcsdata$hfd_z,
                cog3 = bcsdata$copy_z
                )


# MCS -------------------------------------------
mcsdata <- read.dta(paste(dir_data, "mcssdq5yeng_reweighted.dta", sep=""), convert.factors = F) # all MCS data

# items to keep (binary version)
mcskeepb <- c(2,3,5,6,7,8,10,12,13,14,15,16,18,19,22,23,24)  # excluding prosocial scale (itm 1 4 9 17 20)
                                                             # & three positively worded items (itm 11 21 25)
mcssdqb <- mcsdata[,paste("mcs_sdqb", mcskeepb, sep="")]         # BINARY SDQ items only      

# version with factor (3cat)
mcssdqbf <- mcssdqb
for (i in 1:ncol(mcssdqb)) mcssdqbf[,i] <- as.ordered(mcssdqb[,i]) 

# items to keep (3cat version)
mcssdqc <- mcsdata[,c("mcs_sdqc2", 
                     "mcs_sdqb3",  # binary (for comparability)
                     "mcs_sdqb5",  # binary (for comparability)
                     "mcs_sdqc6",
                     "mcs_sdqb7",  # binary (for comparability)
                     "mcs_sdqc8",
                     "mcs_sdqc10",
                     "mcs_sdqc12",
                     "mcs_sdqc13",
                     "mcs_sdqb14", # binary (for comparability)
                     "mcs_sdqc15",
                     "mcs_sdqc16",
                     "mcs_sdqc18",
                     "mcs_sdqc19",
                     "mcs_sdqc22",
                     "mcs_sdqc23",
                     "mcs_sdqc24")]

# version with factor (3cat)
mcssdqcf <- mcssdqc
for (i in 1:ncol(mcssdqc)) mcssdqcf[,i] <- as.ordered(mcssdqc[,i])                    

# add SES and cognitive data
mcsaux <- cbind(mcsdata[,c("mcsid", "faminc", "incq", "ysch_moth", "ysch_fath", "ageint5")],
                cog1 = mcsdata$nvoc_bastz,
                cog2 = mcsdata$psim_bastz,
                cog3 = mcsdata$patc_bastz
)


# common items
Xtemp.bcs <- cbind( bcsaux,      # additional variables
                    matrix("BCS", dim(bcsrutc)[1], 1),                                                             # cohort identifier
                    bcsrutcf[,c("bcs_rutc1", "bcs_rutc2", "bcs_rutc419", "bcs_rutc15", "bcs_rutbD", "bcs_rutb14")], # EXT
                    bcsrutcf[,c("bcs_rutc6", "bcs_rutc16", "bcs_rutc7", "bcs_rutc9", "bcs_rutbAB")]                # INT
)
Xtemp.mcs <- cbind( mcsaux,      # additional variables
                    matrix("MCS", dim(mcssdqc)[1], 1),                                                              # cohort identifier
                    mcssdqcf[,c("mcs_sdqc2", "mcs_sdqc10", "mcs_sdqc12", "mcs_sdqc15", "mcs_sdqb5", "mcs_sdqb7")], # EXT
                    mcssdqcf[,c("mcs_sdqc8", "mcs_sdqc16", "mcs_sdqc6", "mcs_sdqc13", "mcs_sdqb3")]               # INT
)

# append BCS and MCS
nn <- c(colnames(bcsaux), "cohort", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
nn[1] <- "ID"
colnames(Xtemp.bcs) <- nn
colnames(Xtemp.mcs) <- nn
X <- data.frame(rbind(Xtemp.bcs, Xtemp.mcs))

# assemble final data
cohort <- X[,"cohort"]
items.c <- X[,c(grep("X", names(X), value=T), "cohort")]   # noncog items only
items  <- items.c                              # numeric version of noncog items only
for (i in 1:ncol(items.c)) items[,i] <- as.numeric(items.c[,i]) - 1


# ADD cognitive measures
itemsa.c <- X[,c(grep("X", names(X), value=T), grep("cog", names(X), value=T), "cohort")]   # include cognitive measures


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




