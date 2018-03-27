# INEQUALITY DECOMPOSITION

#######################################################################################################
## ---- IQRDEC_FUN
# functions

# function to get centiles
getcent <- function(var){
  pc <- c()
  for (p in seq(1:99)/100) {
    pc <- c(pc,quantile(var,p, na.rm=T))
  }
  return(pc)
}

# function for quantile differences
qdiff <- function(data, q2=.75, q1=.25) {
  pc1 <- quantile(data, q1)
  pc2 <- quantile(data, q2)
  iqr <- pc2-pc1 
  return(iqr)
}

# function for quantile differences table
iqtab <- function(var, by) {
  tab <- aggregate(var, by=by, 
                   FUN = function(x) c(
                     iq9010 = round(qdiff(x, .9, .1) ,3),
                     iq7525 = round(qdiff(x, .75, .25) ,3),
                     iq9050 = round(qdiff(x, .9, .5) ,3),
                     iq5010 = round(qdiff(x, .5, .1) ,3)
                   ))
  rownames(tab) <- paste(as.character(tab[,1]),as.character(tab[,2]), sep = ".")
  tab[,c(1,2)] <- NULL
  return(tab)
}

#######################################################################################################
## ---- IQRDEC

# data
df.final2 <- scores2plot

# ADD RESIDUALISED SCORES
df.final$EXTres <- NA
df.final$INTres <- NA

df.final <- df.final2[FALSE,]
for (g in c("M","F")) {
  for (c in c("BCS", "MCS")) {
    subs <- df.final2[df.final2$cohort==c & df.final2$sex==g,]
    subs$EXTres <- residuals(lm(EXT ~ as.factor(incq) + ysch_moth + ysch_fath, data=subs, na.action=na.exclude))
    subs$INTres <- residuals(lm(INT ~ as.factor(incq) + ysch_moth + ysch_fath, data=subs, na.action=na.exclude))
    df.final <- rbind(df.final,subs)
  }
}
# KEEP ONLY OBS WITH COMPLETE DATA
df.final <- df.final[complete.cases(df.final[,c('EXT', 'INT', 'incq', 'ysch_moth', 'ysch_fath')]),]
rm(df.final2)

# assemble tables
iqs.orig <- rbind(
  cbind(
    t(iqtab(df.final$EXT, by=list(df.final$cohort, df.final$sex))), 
    t(iqtab(df.final$INT, by=list(df.final$cohort, df.final$sex)))
  ),
  c(aggregate(df.final$EXT, by=list(df.final$cohort, df.final$sex), FUN=function(x) round(sd(x),3))[,3],
    aggregate(df.final$INT, by=list(df.final$cohort, df.final$sex), FUN=function(x) round(sd(x),3))[,3]
  )
)
rownames(iqs.orig) <- c("90-10", "75-25", "90-50", "50-10", "Std. dev.")

iqs.resid <- rbind(
  cbind(
    t(iqtab(df.final$EXTres, by=list(df.final$cohort, df.final$sex))), 
    t(iqtab(df.final$INTres, by=list(df.final$cohort, df.final$sex)))
  ),
  c(aggregate(df.final$EXTres, by=list(df.final$cohort, df.final$sex), FUN=function(x) round(sd(x),3))[,3],
    aggregate(df.final$INTres, by=list(df.final$cohort, df.final$sex), FUN=function(x) round(sd(x),3))[,3]
  )
)
rownames(iqs.resid) <- c("90-10", "75-25", "90-50", "50-10", "Std. dev.")

#######################################################################################################
## ---- PCGRAPHS
# percentile graphs


centiles <- as.matrix(seq(1:99)/100)
colnames(centiles) <- 'cent'
for (g in c("M","F")) {
  for (c in c("BCS", "MCS")) {
    for (f in c('EXT', 'INT')) {
      col <- as.matrix(getcent(df.final[df.final$cohort==c & df.final$sex==g,f]))
      colnames(col) <- paste(c,g,f, sep = ".")
      centiles <- data.frame(cbind(centiles, col))
    }
  }
}

plot(centiles$cent, centiles$MCS.M.EXT - centiles$BCS.M.EXT, type='o')
plot(centiles$cent, centiles$MCS.F.EXT - centiles$BCS.F.EXT, type='o')
plot(centiles$cent, centiles$MCS.F.INT - centiles$BCS.F.INT, type='o')


#######################################################################################################
## ---- JMPDEC
df.final$incq <- factor(df.final$incq)
covars <- c("incq","ysch_moth","ysch_fath")
# formulas
form.ext <- as.formula(paste("EXT ~ ", paste0(covars, collapse = '+')))
form.int <- as.formula(paste("INT ~ ", paste0(covars, collapse = '+')))

# males EXT
m.ext.bcs <- lm(form.ext, data = subset(df.final, sex=='M' & cohort=='BCS'))
m.ext.mcs <- lm(form.ext, data = subset(df.final, sex=='M' & cohort=='MCS'))
m.ext.j <- lm(form.ext, data = subset(df.final, sex=='M'))
bbar <- m.ext.j$coefficients # average beta

# inverse average CDF of percentile residuals
# (quantile in the dist of average residuals for each percentile of group residual)
finvbar.bcs <- quantile(residuals(m.ext.j), as.vector(ecdf(residuals(m.ext.bcs))(residuals(m.ext.bcs))))
finvbar.mcs <- quantile(residuals(m.ext.j), as.vector(ecdf(residuals(m.ext.mcs))(residuals(m.ext.mcs))))

y1.bcs <- model.matrix(m.ext.bcs)%*%bbar + finvbar.bcs
y1.mcs <- model.matrix(m.ext.mcs)%*%bbar + finvbar.mcs
y2.bcs <- model.matrix(m.ext.bcs)%*%m.ext.bcs$coefficients + finvbar.bcs
y2.mcs <- model.matrix(m.ext.mcs)%*%m.ext.mcs$coefficients + finvbar.mcs
y3.bcs <- model.matrix(m.ext.bcs)%*%m.ext.bcs$coefficients + residuals(m.ext.bcs)
y3.mcs <- model.matrix(m.ext.mcs)%*%m.ext.mcs$coefficients + residuals(m.ext.mcs)

plot(density(y1.bcs))
lines(density(y1.mcs))

plot(as.matrix(seq(1:99)/100), getcent(y1.mcs) - getcent(y1.bcs), type='o')

T <- mean(y3.mcs)-mean(y3.bcs)
Q <- mean(y1.mcs)-mean(y1.bcs)
P <- mean(y2.mcs)-mean(y2.bcs) - Q
U <- mean(y3.mcs)-mean(y3.bcs) - mean(y2.mcs)-mean(y2.bcs)