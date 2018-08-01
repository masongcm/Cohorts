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
plotineq <- function(data, categ, 
                     suffix = NULL, 
                     ylab = "Y axis", 
                     xlab = "X axis", 
                     ylim = c(-.4, .4),
                     vline = NULL
) {
  
  # extract relevant variables
  if (!is.null(suffix)) datatemp <- data[, c("cohort", "sex", categ, paste0("EXT", suffix), paste0("INT", suffix))]
  if (is.null(suffix)) datatemp <- data[, c("cohort", "sex", categ, "EXT", "INT")]
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

# # non-normalised version
# fi_fsc2 <- plotineq(scores2plot, "fscl5wb", ylab = "Score ", xlab = "Father Occupation (5)",
#                     ylim = c(-.4,1.0))

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
