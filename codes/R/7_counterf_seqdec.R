## ---- COUNTERF_SEQDEC_PREP

# function to make sequential counterfactual decomposition plots
# arguments: 
# 1) list of CF estimates with increasing num of covariates
# 2) list of composition plot titles (defaults to X1-XK)
makecfdecplots <- function(flst, titles = NA) {
  
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
  for (i in 1:numdec) cfdb <- cbind(cfdb, flst[[i]]$composition_effect)     # get composition effects
  colnames(cfdb)[3:(3+numdec-1)] <- paste0("com", seq(1:numdec))
  cfdb <- cbind(cfdb, flst[[numdec]]$structral_effect)                    # get last structure effect
  colnames(cfdb)[3+numdec] <- "str"
  cfdb$comd1 <- cfdb$com1                                   # compute differences in composition effects
  for (i in 2:numdec) cfdb[,paste0("comd",i)] <- cfdb[,paste0("com",i)] - cfdb[,paste0("com",i-1)]
  
  # get axis boundaries
  maxy <- max(cfdb[! names(cfdb) %in% "quant"]) +.05
  miny <- min(cfdb[! names(cfdb) %in% "quant"]) -.05
  
  # common options
  addopts.all <- function(x) {
    x <- x +
      theme(
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 1, face = "italic"),
        legend.position="none"
      ) +
      scale_x_continuous(name = "Quantile", breaks = seq(.1,.9,.1)) +
      coord_cartesian(ylim = c(miny, maxy)) +
      labs(list(colour="")) +
      geom_smooth(se=F) + geom_line() # PLOTS
    return(x)
  }
  
  # plots
  cfplist <- list()
  cfplist[["p.tot"]] <- ggplot(cfdb, aes(quant, tot)) + ggtitle("Total difference")
  for (i in 1:numdec) {
    cfdb.temp <- data.frame(cbind(quant = cfdb$quant, com = cfdb[,paste0("comd",i)]))
    cfplist[[paste0("p.comd",i)]]<- ggplot(cfdb.temp, aes(quant, com)) + ggtitle(paste0("Comp. ", titles[i]))
  }
  cfplist[["p.str"]] <- ggplot(cfdb, aes(quant, str)) + ggtitle("Structure")
  
  cfplist <- lapply(cfplist, addopts.all) # apply options to all graphs

  return(cfplist)
}




# MAKE COUNTERFACTUALS (without inference)

# sequence of decomposition variables
decseq <- list("faminc_real", "scl10b", "ysch_moth5", paste0(c("mothageb", "numch5"), collapse = "+"))

# allocate
lcf.ext.m <- list()
lcf.ext.f <- list()
lcf.int.m <- list()
lcf.int.f <- list()

for (j in 1:length(decseq)) {
  lcf.ext.m[[j]] <- counterfactual(as.formula(paste("EXT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(scores2plot, sex=='M'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.ext.f[[j]] <- counterfactual(as.formula(paste("EXT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(scores2plot, sex=='F'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.int.m[[j]] <- counterfactual(as.formula(paste("INT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(scores2plot, sex=='M'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
  lcf.int.f[[j]] <- counterfactual(as.formula(paste("INT ~ ", paste0(decseq[1:j], collapse = '+')))
                                   , data = subset(scores2plot, sex=='F'),
                                   group = cohn, treatment=TRUE, decomposition=TRUE, 
                                   method = "logit", quantiles = quants,
                                   nreg=100, noboot = T)
}

## ---- COUNTERF_SEQDEC_PLOT
# MAKE PLOTS
plist.ext.m <- makecfdecplots(lcf.ext.m, titles = c("Real income", "Social class", "Maternal education", "Demographics"))
plist.ext.f <- makecfdecplots(lcf.ext.f, titles = c("Real income", "Social class", "Maternal education", "Demographics"))
plist.int.m <- makecfdecplots(lcf.int.m, titles = c("Real income", "Social class", "Maternal education", "Demographics"))
plist.int.f <- makecfdecplots(lcf.int.f, titles = c("Real income", "Social class", "Maternal education", "Demographics"))

# EXT PLOTS
# arrange the plots in a single column
cfp.seqdec.ext <- plot_grid( plist.ext.m[[1]],plist.ext.m[[2]],plist.ext.m[[3]],plist.ext.m[[4]],plist.ext.m[[5]],plist.ext.m[[6]],
                             plist.ext.f[[1]],plist.ext.f[[2]],plist.ext.f[[3]],plist.ext.f[[4]],plist.ext.f[[5]],plist.ext.f[[6]],
                             align = 'vh',
                             hjust = -1,
                             nrow = 2,
                             labels = c("A) Males", rep("",5),"B) Females", rep("",5)), label_size = 17
)
# INT PLOTS
# arrange the plots in a single column
cfp.seqdec.int <- plot_grid( plist.int.m[[1]],plist.int.m[[2]],plist.int.m[[3]],plist.int.m[[4]],plist.int.m[[5]],plist.int.m[[6]],
                             plist.int.f[[1]],plist.int.f[[2]],plist.int.f[[3]],plist.int.f[[4]],plist.int.f[[5]],plist.int.f[[6]],
                             align = 'vh',
                             hjust = -1,
                             nrow = 2,
                             labels = c("A) Males", rep("",5),"B) Females", rep("",5)), label_size = 17
)