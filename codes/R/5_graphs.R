
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
legend_b <- get_legend(rawlist[[1]] + theme(legend.position="bottom", legend.justification="center", legend.box.just = "bottom"))
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
    else ksp[[paste0(f,".",g)]] <- "$<0.0001$"
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
    scale_x_continuous(name = "Score", breaks = seq(-3.5,1.5,.5)) +
    coord_cartesian(xlim = c(minx, maxx), ylim = c(0,.62)) +
    scale_fill_discrete("") + # remove fill guide title
    scale_colour_discrete(guide=FALSE) +  # remove colour legend
    geom_density(alpha = 0.1, bw = "nrd", adjust=2, kernel = "epanechnikov")
  return(x)
}

# densities of factor scores
pdext.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("Males Externalising")
pdint.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("Males Internalising")
pdext.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("Females Externalising")
pdint.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("Females Internalising")
denslist <- list(pdext.ebm.m, pdint.ebm.m, pdext.ebm.f, pdint.ebm.f) 
denslist <- lapply(denslist, addopts.dens) # apply options to all graphs
# add KS pvalue
for (p in 1:4) {
  denslist[[p]] <- denslist[[p]] + 
    annotate("text", x = minx+1, y = .5, label = "Kolmog.-Smirnov") + 
    annotate("text", x = minx+1, y = .45, label = paste0("$p$",ksp[[p]]))
}
pcol <- plot_grid( denslist[[1]], denslist[[3]], denslist[[2]], denslist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add legend
legend_b <- get_legend(denslist[[1]] + theme(legend.position="bottom", legend.justification="center", legend.box.just = "bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

############################################################################################
## ---- FACRAW
# see how scored factors compare with raw scores
box.ext.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(EXT_RAW), y=EXT)) + geom_boxplot() + ggtitle("BCS Externalising") + xlab("Sum score") + ylab("Factor score")
box.int.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(INT_RAW), y=INT)) + geom_boxplot() + ggtitle("BCS Internalising") + xlab("Sum score") + ylab("Factor score")
box.ext.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(EXT_RAW), y=EXT)) + geom_boxplot() + ggtitle("MCS Externalising") + xlab("Sum score") + ylab("Factor score")
box.int.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(INT_RAW), y=INT)) + geom_boxplot() + ggtitle("MCS Internalising") + xlab("Sum score") + ylab("Factor score")
plot_grid(box.ext.bcs, box.int.bcs, box.ext.mcs, box.int.mcs, ncol=2, align="h")
