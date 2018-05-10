## ---- COUNTERF_SEQDEC_ESTIM
# ESTIMATE COUNTERFACTUALS (without inference)

library(Counterfactual)

# numeric cohort
finaldata$cohn <- as.numeric(finaldata$cohort)-1
# decide how many quantiles
quants <- seq(.04,.96,.02)

# allocate
lcf.ext.m <- list()
lcf.ext.f <- list()
lcf.int.m <- list()
lcf.int.f <- list()

for (j in 1:length(decseq)) {
  lcf.ext.m[[j]] <- counterfactual(as.formula(paste("EXT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(finaldata, sex=='M'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.ext.f[[j]] <- counterfactual(as.formula(paste("EXT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(finaldata, sex=='F'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.int.m[[j]] <- counterfactual(as.formula(paste("INT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(finaldata, sex=='M'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.int.f[[j]] <- counterfactual(as.formula(paste("INT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(finaldata, sex=='F'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
}

## ---- COUNTERF_SEQDEC_PREP

# function to make sequential counterfactual decomposition plots
# arguments: 
# 1) list of CF estimates with increasing num of covariates
# 2) list of composition plot titles (defaults to X1-XK)
# 3) bandwidth for kernel smoothing
makecfdecplots <- function(flst, titles = NA, kbwt = 0.1) {
  
  numdec <- length(flst) # number of decompositions
  
  if (is.na(titles)) {
    titles <- paste0("X",seq(1:numdec))
  } 
  else {
    if (length(titles) != numdec) stop("List of titles must be same as number of decompositions ")
  }
  
  # assemble decomposition data
  cfdb <- data.frame(cbind(flst[[1]]$quantiles, flst[[1]]$total_effect)) # get quantiles and total effect
  colnames(cfdb) <- c("quant","tot")
  cfdb$quant <- round(cfdb$quant, 3)
  for (i in 1:numdec) cfdb <- cbind(cfdb, flst[[i]]$composition_effect)     # get composition effects
  colnames(cfdb)[3:(3+numdec-1)] <- paste0("com", seq(1:numdec))
  cfdb <- cbind(cfdb, flst[[numdec]]$structral_effect)                    # get last structure effect
  colnames(cfdb)[3+numdec] <- "str"
  cfdb$comd1 <- cfdb$com1                                   # compute differences in composition effects
  for (i in 2:numdec) cfdb[,paste0("comd",i)] <- cfdb[,paste0("com",i)] - cfdb[,paste0("com",i-1)]
  
  # smooth quantile differences
  nqsm <- length(flst[[1]]$quantiles)*4-3                # number of smoothed quantiles
  for (i in 1:numdec) {
    smooth <- ksmooth(flst[[1]]$quantiles, cfdb[,paste0("comd",i)], kernel="normal", n.points = nqsm, bandwidth=kbwt)
    smooth2 <- data.frame(cbind(round(smooth$x,3), smooth$y))
    colnames(smooth2) <- c("quant", paste0("comds",i))
    cfdb <- merge(cfdb, smooth2, by="quant", all = F) # merge and keep only original quantiles
  }
  
  # smooth total and structure
  smooth_tot <- ksmooth(flst[[1]]$quantiles, cfdb[,"tot"], kernel="normal", n.points = nqsm, bandwidth=kbwt)
  smooth_tot2 <- data.frame(cbind(round(smooth_tot$x,3), smooth_tot$y))
  colnames(smooth_tot2) <- c("quant", "tots")
  cfdb <- merge(cfdb, smooth_tot2, by="quant", all = F)
  smooth_str <- ksmooth(flst[[1]]$quantiles, cfdb[,"str"], kernel="normal", n.points = nqsm, bandwidth=kbwt)
  smooth_str2 <- data.frame(cbind(round(smooth_str$x,3), smooth_str$y))
  colnames(smooth_str2) <- c("quant", "strs")
  cfdb <- merge(cfdb, smooth_str2, by="quant", all = F)
  
  # get axis boundaries
  maxy <- max(cfdb[! names(cfdb) %in% "quant"]) +.05
  miny <- min(cfdb[! names(cfdb) %in% "quant"]) -.05
  
  # common options
  addopts.all <- function(x) {
    x <- x +
      theme(
        axis.title.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        plot.title = element_text(margin=margin(t=15), face = "italic"), # italic title inside graph
        legend.position="none"
      ) +
      scale_x_continuous(name = "Quantile", breaks = seq(.1,.9,.2)) +
      coord_cartesian(ylim = c(miny, maxy)) +
      labs(list(colour=""))
    return(x)
  }
  
  # plots
  cfplist <- list()
  cfplist[["p.tot"]] <- ggplot(cfdb) + 
    geom_line(aes(quant, tot), linetype = "dashed") + geom_line(aes(quant, tots), color="blue", size=1) + ggtitle("Total difference")
  for (i in 1:numdec) {
    cfdb.temp <- cfdb[,c("quant", paste0(c("comd","comds"),i))]
    colnames(cfdb.temp) <- c("quant", "comd", "comds")
    cfplist[[paste0("p.comd",i)]]<- ggplot(cfdb.temp) + # plot
      geom_line(aes(quant,comd), linetype = "dashed") + geom_line(aes(quant,comds), color="blue", size=1) + 
      ggtitle(paste0("Comp. ", titles[i]))
  }
  cfplist[["p.str"]] <- ggplot(cfdb) + geom_line(aes(quant, str), linetype = "dashed") + 
    geom_line(aes(quant, strs), color="blue", size=1) + ggtitle("Structure")
  cfplist <- lapply(cfplist, addopts.all) # apply options to all graphs
  
  # return both plots and data
  cfpall <- list(plots = cfplist, data = cfdb)
  return(cfpall)
}

# compute results
cfres.ext.m <- makecfdecplots(lcf.ext.m, titles = decvarsgroups, kbwt = .1)
cfres.ext.f <- makecfdecplots(lcf.ext.f, titles = decvarsgroups, kbwt = .1)
cfres.int.m <- makecfdecplots(lcf.int.m, titles = decvarsgroups, kbwt = .1)
cfres.int.f <- makecfdecplots(lcf.int.f, titles = decvarsgroups, kbwt = .1)

## ---- COUNTERF_SEQDEC_PLOT

# extract PLOTS
plist.ext.m <- cfres.ext.m$plots
plist.ext.f <- cfres.ext.f$plots
plist.int.m <- cfres.int.m$plots
plist.int.f <- cfres.int.f$plots

# EXT PLOTS
# arrange the plots in a single column
cfp.seqdec.ext <- plot_grid( plist.ext.m[[1]],plist.ext.m[[2]],plist.ext.m[[3]],plist.ext.m[[4]],plist.ext.m[[5]],plist.ext.m[[6]],plist.ext.m[[7]],plist.ext.m[[8]],
                             plist.ext.f[[1]],plist.ext.f[[2]],plist.ext.f[[3]],plist.ext.f[[4]],plist.ext.f[[5]],plist.ext.f[[6]],plist.ext.f[[7]],plist.ext.f[[8]],
                             align = 'vh',
                             hjust = -1,
                             nrow = 2,
                             labels = c("A) Males", rep("",1+length(decseq)),"B) Females", rep("",1+length(decseq))), label_size = 17
)
# INT PLOTS
# arrange the plots in a single column
cfp.seqdec.int <- plot_grid( plist.int.m[[1]],plist.int.m[[2]],plist.int.m[[3]],plist.int.m[[4]],plist.int.m[[5]],plist.int.m[[6]],plist.int.m[[7]],plist.int.m[[8]],
                             plist.int.f[[1]],plist.int.f[[2]],plist.int.f[[3]],plist.int.f[[4]],plist.int.f[[5]],plist.int.f[[6]],plist.int.f[[7]],plist.int.f[[8]],
                             align = 'vh',
                             hjust = -1,
                             nrow = 2,
                             labels = c("A) Males", rep("",1+length(decseq)),"B) Females", rep("",1+length(decseq))), label_size = 17
)