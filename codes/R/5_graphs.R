
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
    geom_density(alpha = 0.1, bw = "nrd", adjust=1.5, kernel = "epanechnikov")
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
    annotate("text", x = minx+1, y = .5, label = "Kolmog.-Smirnov") + 
    annotate("text", x = minx+1, y = .45, label = paste0("$p$",ksp[[p]]))
}
pcol <- plot_grid( denslist[[1]], denslist[[3]], denslist[[2]], denslist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add legend
legend_b <- get_legend(denslist[[1]] + theme(legend.position="bottom"))
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

############################################################################################
## ---- FACINEQ_MAKE

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

# function to make inequality plots
# data = dataset
# categ = categorical variable (factor)
# suffix = string appended to demeaned variable
plotineq <- function(data, categ, suffix, 
                     ylab = "Y axis", 
                     xlab = "X axis", 
                     ylim = c(-.4, .4),
                     vline = NULL
) {
  
  # extract relevant variables
  datatemp <- data[, c("cohort", "sex", categ, paste0("EXT", suffix), paste0("INT", suffix))]
  colnames(datatemp) <- c("cohort", "sex", "categ", "EXT", "INT")
  datatemp <- datatemp[complete.cases(datatemp),] # just complete cases
  
  
  #common options
  addopts_main <- function(x) {
    x <- x + xlab(xlab) +
      scale_y_continuous(name = ylab, breaks = round(seq(-.8,.8,.2),1)) +
      coord_cartesian(ylim = ylim) +
      stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
      stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
      labs(list(colour="")) + theme(legend.position="none")
    if (!is.null(vline)) x <- x + geom_vline(xintercept = vline, linetype="dashed")
    return(x)
  }
  
  # make dotCI
  ineq.ext.m <- ggplot(data=subset(datatemp, sex=="M"), aes(x=categ, y=EXT, colour=cohort)) + ggtitle("Males Externalising")
  ineq.ext.f <- ggplot(data=subset(datatemp, sex=="F"), aes(x=categ, y=EXT, colour=cohort)) + ggtitle("Females Externalising")
  ineq.int.m <- ggplot(data=subset(datatemp, sex=="M"), aes(x=categ, y=INT, colour=cohort)) + ggtitle("Males Internalising")
  ineq.int.f <- ggplot(data=subset(datatemp, sex=="F"), aes(x=categ, y=INT, colour=cohort)) + ggtitle("Females Internalising")
  ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f)
  ineqlist <- lapply(ineqlist, function(x) addopts_main(x)) # apply options to all graphs
  legend_b <- cowplot::get_legend(ineqlist[[1]] + theme(legend.position="bottom")) # get legend
  ineqlist <- lapply(ineqlist, function(x) ggplot_gtable(ggplot_build((x)))) # make into gtable object
  
  # HISTOGRAM OF X
  # get histogram data
  histdata <- datatemp %>%
    filter(!is.na(categ)) %>%
    count(cohort, sex, categ) %>%
    group_by(cohort,sex) %>% 
    mutate(prop = n / sum(n))
  histdata <- as.data.frame(histdata)
  
  
  # common options for histogram
  addopts_hist <- function(x) {
    x <- x + theme(axis.title.x=element_blank(),
                   axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                   axis.ticks=element_blank(),
                   legend.position = "none") +
      scale_y_continuous(breaks = round(seq(.0,.2,.2),1)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) # histogram
    return(x)
  }
  
  # make histograms
  hist.m <- ggplot(data = histdata[histdata$sex=="M",], aes(categ, prop, fill = cohort)) + ggtitle("Distribution of categories") 
  hist.f <- ggplot(data = histdata[histdata$sex=="F",], aes(categ, prop, fill = cohort)) + ggtitle("Distribution of categories")
  histlist <- list(hist.m, hist.f, hist.m, hist.f)
  histlist <- lapply(histlist, function(x) ggplot_gtable(ggplot_build(addopts_hist(x)))) # apply options to all graphs
  
  # assemble and arrange
  for (i in length(ineqlist)) {
    maxWidth = grid::unit.pmax(ineqlist[[i]]$widths[2:3], histlist[[i]]$widths[2:3])
    ineqlist[[i]]$widths[2:3] <- maxWidth
    histlist[[i]]$widths[2:3] <- maxWidth
  }
  
  # return everything
  out <- list()
  out$ineqlist <- ineqlist
  out$histlist <- histlist
  out$legend <- legend_b
  
  return(out)
}

# SOCIAL CLASS at 10 -------------------------
# reorder social class to have "other" on the LHS (scl10c)
scores2plot$scl10c <- factor(scores2plot$scl10b, levels(scores2plot$scl10b)[c(5,1:4)])
# demean scores for lowest class
scores2plot <- demean(scores2plot, "scl10b", "IIINM", "dsc")
# make plots
fi_sc <- plotineq(scores2plot, "scl10b", "dsc", ylab = "Score (IIINM = 0)", xlab = "Family Social Class (10)",
                  ylim = c(-.6,.4), vline=4.5)

# FATHER SOCIAL CLASS at 5 --------------------
# demean scores for lowest class
scores2plot <- demean(scores2plot, "fscl5wb", "Blue collar", "dfsc")
# make plots
fi_fsc <- plotineq(scores2plot, "fscl5wb", "dfsc", ylab = "Score (Blue c. = 0)", xlab = "Father Occupation (5)",
                  ylim = c(-.4,.4))


# MATERNAL SCHOOLING -------------------------
# demean scores for lowest level
scores2plot <- demean(scores2plot, "mysch5b", "17-18", "dys")
fi_ys <- plotineq(scores2plot, "mysch5b", "dys", ylab = "Score (17-18 = 0)", xlab = "Age mother left FTE (5)",
                  ylim = c(-.6,.4))

# MATERNAL POSTCOMPULSORY  -------------------------
scores2plot <- demean(scores2plot, "mpsla5", "Compulsory", "dps")
fi_ps <- plotineq(scores2plot, "mpsla5", "dps", ylab = "Score (Compuls. = 0)", xlab = "Maternal schooling (5)",
                  ylim = c(-.1,.4))

# MATERNAL EMPLOYMENT  -------------------------
scores2plot <- demean(scores2plot, "mempl5", "Unempl./At home", "dme")
fi_me <- plotineq(scores2plot, "mempl5", "dme", ylab = "Score (Unempl./At home = 0)", xlab = "Maternal employment (5)")

# EMPLOYMENT*EDUCATION  -------------------------
# generate interaction
scores2plot$mscem5 <- interaction(scores2plot[c("mempl5b","mpsla5")]) # generate interaction
levels(scores2plot$mscem5) <- c("Compuls. \n Unempl./Home",
                                "Compuls. \n Employed",
                                "Post-comp. \n Unempl./Home",
                                "Post-comp. \n Employed"
)
scores2plot <- demean(scores2plot, "mscem5", "Compuls. \n Unempl./Home", "dmecs")
fi_mecs <- plotineq(scores2plot, "mscem5", "dmecs", ylab = "Score (Compuls. + Unempl. = 0)", xlab = "Maternal employment/education at 5",
                    ylim = c(-.2,.6), vline=2.5)

# SMOKING IN PREGNANCY
scores2plot <- demean(scores2plot, "smkpr", "Non-smoker", "dsm")
fi_sm <- plotineq(scores2plot, "smkpr", "dsm", ylab = "Score (Non-smoker = 0)", xlab = "Maternal smoking (pregn.)",
                  ylim = c(-.6,.1))
