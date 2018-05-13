
############################################################################################
## ---- AGEHIST
# histogram of age (months) by cohort

# make data
aged <- aggregate(scores2plot$cohortsex, list(age = scores2plot$age, cohort = scores2plot$cohort), FUN = length)
# convert to densities
aged$dens[aged$cohort == "BCS"] <-  aged[aged$cohort == "BCS", "x"]/ sum(aged[aged$cohort == "BCS", "x"])
aged$dens[aged$cohort == "MCS"] <-  aged[aged$cohort == "MCS", "x"]/ sum(aged[aged$cohort == "MCS", "x"])
aged[aged$cohort=="MCS", "dens"] <- - aged[aged$cohort=="MCS", "dens"] # flip MCS

ggplot(aged, aes(x=age, y=dens, fill=cohort)) + 
  ylab("Density") + scale_y_continuous(limits = c(-.5,.75)) +
  xlab("Age (months)") +
  geom_bar(stat="identity", position="identity") +
  theme(legend.position = c(0.9, 0.2)) +
  geom_hline(aes(yintercept=0))


############################################################################################
## ---- RAWHIST
# histograms for raw scores

# common options
addopts.raw <- function(x) {
  x <- x +
    theme(
      axis.title.y=element_blank(),
      legend.position="none"
    ) +
    scale_x_continuous(name = "Raw score", breaks = seq(0,10,1)) +
    scale_fill_discrete("") + # remove fill guide title
    scale_colour_discrete(guide=FALSE) +  # remove colour legend
    geom_bar(position="dodge", aes(y = ..prop..)) # PLOTS
  return(x)
}

rawext.m <- ggplot(subset(scores2plot, sex=="M"),aes(x=EXT_RAW, fill=cohort)) + ggtitle("EXT Scores (Males)") + coord_cartesian(ylim = c(0,.25))
rawint.m <- ggplot(subset(scores2plot, sex=="M"),aes(x=INT_RAW, fill=cohort)) + ggtitle("INT Scores (Males)") + coord_cartesian(ylim = c(0,.4))
rawext.f <- ggplot(subset(scores2plot, sex=="F"),aes(x=EXT_RAW, fill=cohort)) + ggtitle("EXT Scores (Females)") + coord_cartesian(ylim = c(0,.25))
rawint.f <- ggplot(subset(scores2plot, sex=="F"),aes(x=INT_RAW, fill=cohort)) + ggtitle("INT Scores (Females)") + coord_cartesian(ylim = c(0,.4))
rawlist <- list(rawext.m, rawint.m, rawext.f, rawint.f) 
rawlist <- lapply(rawlist, addopts.raw) # apply options to all graphs

pcol <- plot_grid( rawlist[[1]], rawlist[[2]], rawlist[[3]], rawlist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)

# add legend
legend_b <- get_legend(rawlist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p


############################################################################################
## ---- FACDENS
# densities of scored factors

# Kolmogorov-Smirnov test
ksp <- list()
for (g in c("M", "F")) {
  for (f in c("EXT","INT")) {
    cs1 <- paste0("BCS.",g)
    cs2 <- paste0("MCS.",g)
    pval <- round(ks.test(scores2plot[scores2plot$cohortsex==cs1,f],
                                  scores2plot[scores2plot$cohortsex==cs2,f]
    )$p.value,3)
    if (pval>.0001) ksp[[paste0(f,".",g)]] <- toString(pval)
    else ksp[[paste0(f,".",g)]] <- "<0.0001"
  }
}
# graph x axis boundaries
maxx <- max(scores2plot[names(scores2plot) %in% c("EXT","INT")]) +.05
minx <- min(scores2plot[names(scores2plot) %in% c("EXT","INT")]) -.05

# common options
addopts.dens <- function(x) {
  x <- x +
    theme(
      axis.title.y=element_blank(),
      legend.position="none"
    ) +
    scale_x_continuous(name = "Quantile", breaks = seq(-3.5,1.5,.5)) +
    coord_cartesian(xlim = c(minx, maxx), ylim = c(0,.62)) +
    scale_fill_discrete("") + # remove fill guide title
    scale_colour_discrete(guide=FALSE) +  # remove colour legend
    geom_density(alpha = 0.1) # PLOTS
  return(x)
}

# densities of factor scores
pdext.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("EXT Scores (Males)")
pdint.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("INT Scores (Males)")
pdext.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("EXT Scores (Females)")
pdint.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("INT Scores (Females)")
denslist <- list(pdext.ebm.m, pdint.ebm.m, pdext.ebm.f, pdint.ebm.f) 
denslist <- lapply(denslist, addopts.dens) # apply options to all graphs
# add KS pvalue
for (p in 1:4) {
  denslist[[p]] <- denslist[[p]] + 
    annotate("text", x = minx+.5, y = .5, label = "KS p-value") + 
    annotate("text", x = minx+.5, y = .45, label = ksp[[p]])
}
pcol <- plot_grid( denslist[[1]], denslist[[2]], denslist[[3]], denslist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add legend
legend_b <- get_legend(denslist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

############################################################################################
## ---- INCINEQ
# MEAN/CI plot of income by income quintile

# aggregate mean income
semean <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
meaninc <- data.frame(
  aggregate(scores2plot$faminc10_real, 
            by = list(scores2plot$cohort, scores2plot$incq), 
            FUN=function(x) c(m = mean(x, na.rm=T), se = semean(x) )))
meaninc <- cbind(meaninc[1:2], as.matrix(meaninc$x))
colnames(meaninc) <- c("cohort", "quant", "mean", "semean")

# restandardise
q1.bcs <- meaninc[meaninc$cohort=="BCS" & meaninc$quant=="1", "mean"]
q1.mcs <- meaninc[meaninc$cohort=="MCS" & meaninc$quant=="1", "mean"]
meaninc[meaninc$cohort=="BCS", "mean"] <- (meaninc[meaninc$cohort=="BCS", "mean"] - q1.bcs)/q1.bcs
meaninc[meaninc$cohort=="MCS", "mean"] <- (meaninc[meaninc$cohort=="MCS", "mean"] - q1.mcs)/q1.mcs
meaninc$ciu <- meaninc$mean + 1.96*meaninc$semean
meaninc$cil <- meaninc$mean - 1.96*meaninc$semean

ineq.inc <- ggplot(data=meaninc, aes(x=as.factor(quant), y=mean, colour=cohort)) +
  geom_point(size=3) + 
  scale_y_continuous(name = "Mean Family Income", breaks = seq(0,11,2)) +
  xlab("Family Income Quintile at 10") +
  theme(legend.justification=c(0,0), legend.position=c(0,.8), legend.title = element_blank())

ineq.inc


############################################################################################
## ---- FACINEQ
# MEAN/CI plot of scores by income quintile

#common options
addopts <- function(x) {
  x <- x + xlab("Family Income Quintile at 10") + ylab("Factor score") +
    coord_cartesian(ylim = c(-.4, .5)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=EXT, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=EXT, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=INT, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=INT, colour=cohort)) + ggtitle("Females Internalising")

ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f) 
ineqlist <- lapply(ineqlist, addopts) # apply options to all graphs

# arrange the plots in a single column
pcol <- plot_grid( ineqlist[[1]],ineqlist[[2]],ineqlist[[3]],ineqlist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
legend_b <- get_legend(ineqlist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

############################################################################################
## ---- FACINEQ_DEMEAN
# function to demean variables in data to have mean zero in a given level
# data = dataset
# refvar = reference variable (factor)
# reflev = level of refvar at which to demean
# suffix = string to append to demeaned variable
demean <- function(data, refvar, reflev, suffix) {
  
  # get mean by cohortsex to rescale
  tempdata <- data[data[,refvar]==reflev,]
  meansfac <- aggregate(tempdata[,c("EXT","INT")], by=list(tempdata$cohortsex), FUN = function(x) mean(x, na.rm=T))
  rownames(meansfac) <- meansfac$Group.1
  meansfac <- meansfac[,-1]
  # rescale scores so that mean in lowest level is 0
  for (f in c("EXT","INT")) {
    fd <- paste0(f,suffix)
    data[,fd] <- NA
    for (c in c("BCS","MCS")) {
      for (g in c("M","F")) {
        cs <- paste0(c,".",g)
        data[data$cohortsex==cs,fd] <- data[data$cohortsex==cs,f]-meansfac[cs,f]
      }
    }
  }
  return(data)
}

############################################################################################
## ---- FACINEQ_SC
# MEAN/CI plot of scores by social class
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

# reorder social class to have "other" on the LHS (scl10c)
scores2plot$scl10c <- factor(scores2plot$scl10b, levels(scores2plot$scl10b)[c(5,1:4)])

# demean scores for lowest class
scores2plot <- demean(scores2plot, "scl10b", "IV V", "dsc")

# DOTCI
# common options for dotCI
addopts_main <- function(x) {
  x <- x + xlab("Family social class at 10") +
    scale_y_continuous(name = "Factor score (IV V = 0)", breaks = seq(-.4,.4,.2)) +
    coord_cartesian(ylim = c(-.4, .4)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}
ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10c)), aes(x=scl10c, y=EXTdsc, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10c)), aes(x=scl10c, y=EXTdsc, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10c)), aes(x=scl10c, y=INTdsc, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10c)), aes(x=scl10c, y=INTdsc, colour=cohort)) + ggtitle("Females Internalising")
ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object


# HISTOGRAM OF X
# get histogram data
tabb <- scores2plot %>%
  filter(!is.na(scl10c)) %>%
  count(cohort, sex, scl10c) %>%
  group_by(cohort,sex) %>% 
  mutate(prop = n / sum(n))
tabb <- as.data.frame(tabb)

# common options for histogram
addopts_hist <- function(x) {
  x <- x + theme(axis.title.x=element_blank(),
                 axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                 axis.ticks=element_blank(),
                 legend.position = "none") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
}
hist.m <- ggplot(data = tabb[tabb$sex=="M",], aes(scl10c, prop, fill = cohort)) + ggtitle("Distribution of categories") 
hist.f <- ggplot(data = tabb[tabb$sex=="F",], aes(scl10c, prop, fill = cohort)) + ggtitle("Distribution of categories")
histlist <- list(hist.m, hist.f, hist.m, hist.f)
histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs

# assemble and arrange
for (i in length(ineqlist)) {
  maxWidth = unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
  ineqlist[[i]]$widths[2:3] <- maxWidth
  histlist[[i]]$widths[2:3] <- maxWidth
}
# plot
grid.arrange(ineqlist[[1]],ineqlist[[2]], 
             ineqlist[[3]],ineqlist[[4]],
             histlist[[1]],histlist[[2]], legend_b,
             ncol=2, nrow=4, heights = c(4,4,2,0.5))

############################################################################################
## ---- FACINEQ_YS
# MEAN/CI plot of scores by maternal years of schooling
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

# demean scores for lowest level
scores2plot <- demean(scores2plot, "mysch5b", "15", "dys")

# DOTCI
# common options for dotCI
addopts_main <- function(x) {
  x <- x + xlab("Age mother left FTE") +
    scale_y_continuous(name = "Factor score (Left at 15 = 0)", breaks = seq(-.2,.8,.2)) +
    coord_cartesian(ylim = c(-.2, .8)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}
ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mysch5b)), aes(x=mysch5b, y=EXTdys, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mysch5b)), aes(x=mysch5b, y=EXTdys, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mysch5b)), aes(x=mysch5b, y=INTdys, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mysch5b)), aes(x=mysch5b, y=INTdys, colour=cohort)) + ggtitle("Females Internalising")
ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object

# HISTOGRAM OF X
# get histogram data
tabb <- scores2plot %>%
  filter(!is.na(mysch5b)) %>%
  count(cohort, sex, mysch5b) %>%
  group_by(cohort,sex) %>% 
  mutate(prop = n / sum(n))
tabb <- as.data.frame(tabb)

# common options for histogram
addopts_hist <- function(x) {
  x <- x + theme(axis.title.x=element_blank(),
                 axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                 axis.ticks=element_blank(),
                 legend.position = "none") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
}
hist.m <- ggplot(data = tabb[tabb$sex=="M",], aes(mysch5b, prop, fill = cohort)) + ggtitle("Distribution of categories") 
hist.f <- ggplot(data = tabb[tabb$sex=="F",], aes(mysch5b, prop, fill = cohort)) + ggtitle("Distribution of categories")
histlist <- list(hist.m, hist.f, hist.m, hist.f)
histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs

# assemble and arrange
for (i in length(ineqlist)) {
  maxWidth = unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
  ineqlist[[i]]$widths[2:3] <- maxWidth
  histlist[[i]]$widths[2:3] <- maxWidth
}
# plot
grid.arrange(ineqlist[[1]],ineqlist[[2]], 
             ineqlist[[3]],ineqlist[[4]],
             histlist[[1]],histlist[[2]], legend_b,
             ncol=2, nrow=4, heights = c(4,4,2,0.5))


############################################################################################
## ---- FACINEQ_CS
# MEAN/CI plot of scores by compulsory schooling
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

scores2plot <- demean(scores2plot, "mpsla5", "Compulsory", "dcs")

#common options
addopts_main <- function(x) {
  x <- x + xlab("Maternal schooling at 5") +
    scale_y_continuous(name = "Factor score (Compulsory = 0)", breaks = seq(-.4,.4,.2)) +
    coord_cartesian(ylim = c(-.4, .4)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mpsla5)), aes(x=mpsla5, y=EXTdcs, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mpsla5)), aes(x=mpsla5, y=EXTdcs, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mpsla5)), aes(x=mpsla5, y=INTdcs, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mpsla5)), aes(x=mpsla5, y=INTdcs, colour=cohort)) + ggtitle("Females Internalising")
ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object

# HISTOGRAM OF X
# get histogram data
tabb <- scores2plot %>%
  filter(!is.na(mpsla5)) %>%
  count(cohort, sex, mpsla5) %>%
  group_by(cohort,sex) %>% 
  mutate(prop = n / sum(n))
tabb <- as.data.frame(tabb)

# common options for histogram
addopts_hist <- function(x) {
  x <- x + theme(axis.title.x=element_blank(),
                 axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                 axis.ticks=element_blank(),
                 legend.position = "none") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
}
hist.m <- ggplot(data = tabb[tabb$sex=="M",], aes(mpsla5, prop, fill = cohort)) + ggtitle("Distribution of categories") 
hist.f <- ggplot(data = tabb[tabb$sex=="F",], aes(mpsla5, prop, fill = cohort)) + ggtitle("Distribution of categories")
histlist <- list(hist.m, hist.f, hist.m, hist.f)
histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs

# assemble and arrange
for (i in length(ineqlist)) {
  maxWidth = unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
  ineqlist[[i]]$widths[2:3] <- maxWidth
  histlist[[i]]$widths[2:3] <- maxWidth
}
# plot
grid.arrange(ineqlist[[1]],ineqlist[[2]], 
             ineqlist[[3]],ineqlist[[4]],
             histlist[[1]],histlist[[2]], legend_b,
             ncol=2, nrow=4, heights = c(4,4,2,0.5))


############################################################################################
## ---- FACINEQ_ME
# MEAN/CI plot of scores by maternal employment
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

scores2plot <- demean(scores2plot, "mempl5", "Unempl./At home", "dme")

#common options
addopts_main <- function(x) {
  x <- x + xlab("Maternal employment at 5") +
    scale_y_continuous(name = "Factor score (Unemployed = 0)", breaks = seq(-.4,.4,.2)) +
    coord_cartesian(ylim = c(-.4, .4)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mempl5)), aes(x=mempl5, y=EXTdme, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mempl5)), aes(x=mempl5, y=EXTdme, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mempl5)), aes(x=mempl5, y=INTdme, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mempl5)), aes(x=mempl5, y=INTdme, colour=cohort)) + ggtitle("Females Internalising")
ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object

# HISTOGRAM OF X
# get histogram data
tabb <- scores2plot %>%
  filter(!is.na(mempl5)) %>%
  count(cohort, sex, mempl5) %>%
  group_by(cohort,sex) %>% 
  mutate(prop = n / sum(n))
tabb <- as.data.frame(tabb)

# common options for histogram
addopts_hist <- function(x) {
  x <- x + theme(axis.title.x=element_blank(),
                 axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                 axis.ticks=element_blank(),
                 legend.position = "none") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
}
hist.m <- ggplot(data = tabb[tabb$sex=="M",], aes(mempl5, prop, fill = cohort)) + ggtitle("Distribution of categories") 
hist.f <- ggplot(data = tabb[tabb$sex=="F",], aes(mempl5, prop, fill = cohort)) + ggtitle("Distribution of categories")
histlist <- list(hist.m, hist.f, hist.m, hist.f)
histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs

# assemble and arrange
for (i in length(ineqlist)) {
  maxWidth = unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
  ineqlist[[i]]$widths[2:3] <- maxWidth
  histlist[[i]]$widths[2:3] <- maxWidth
}
# plot
grid.arrange(ineqlist[[1]],ineqlist[[2]], 
             ineqlist[[3]],ineqlist[[4]],
             histlist[[1]],histlist[[2]], legend_b,
             ncol=2, nrow=4, heights = c(4,4,2,0.5))


############################################################################################
## ---- FACINEQ_SM
# MEAN/CI plot of scores by maternal marital status
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

scores2plot <- demean(scores2plot, "singlem", "Married", "dsm")

#common options
addopts_main <- function(x) {
  x <- x + xlab("Maternal marital status at birth") +
    scale_y_continuous(name = "Factor score (Married = 0)", breaks = seq(-.4,.4,.2)) +
    coord_cartesian(ylim = c(-.4, .4)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$singlem)), aes(x=singlem, y=EXTdsm, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$singlem)), aes(x=singlem, y=EXTdsm, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$singlem)), aes(x=singlem, y=INTdsm, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$singlem)), aes(x=singlem, y=INTdsm, colour=cohort)) + ggtitle("Females Internalising")
ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object

# HISTOGRAM OF X
# get histogram data
tabb <- scores2plot %>%
  filter(!is.na(singlem)) %>%
  count(cohort, sex, singlem) %>%
  group_by(cohort,sex) %>% 
  mutate(prop = n / sum(n))
tabb <- as.data.frame(tabb)

# common options for histogram
addopts_hist <- function(x) {
  x <- x + theme(axis.title.x=element_blank(),
                 axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                 axis.ticks=element_blank(),
                 legend.position = "none") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
}
hist.m <- ggplot(data = tabb[tabb$sex=="M",], aes(singlem, prop, fill = cohort)) + ggtitle("Distribution of categories") 
hist.f <- ggplot(data = tabb[tabb$sex=="F",], aes(singlem, prop, fill = cohort)) + ggtitle("Distribution of categories")
histlist <- list(hist.m, hist.f, hist.m, hist.f)
histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs

# assemble and arrange
for (i in length(ineqlist)) {
  maxWidth = unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
  ineqlist[[i]]$widths[2:3] <- maxWidth
  histlist[[i]]$widths[2:3] <- maxWidth
}
# plot
grid.arrange(ineqlist[[1]],ineqlist[[2]], 
             ineqlist[[3]],ineqlist[[4]],
             histlist[[1]],histlist[[2]], legend_b,
             ncol=2, nrow=4, heights = c(4,4,2,0.5))


