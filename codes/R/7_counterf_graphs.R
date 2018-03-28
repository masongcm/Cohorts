#######################################################################################################
## ---- COUNTERF_PREP_PLOTS

# function to get data table out of counterfactual result
getcfdat <- function(cffit) {
  plotdata <- data.frame(cbind(
    quant = cffit$quantiles,
    str = cffit$resSE[,1],
    str.l = cffit$resSE[,3],
    str.u = cffit$resSE[,4],
    com = cffit$resCE[,1],
    com.l = cffit$resCE[,3],
    com.u = cffit$resCE[,4],
    tot = cffit$resTE[,1],
    tot.l = cffit$resTE[,3],
    tot.u = cffit$resTE[,4],
    qf.cf = cffit$model_quantile_counter[,1],
    qf.ref1 = cffit$model_quantile_ref1[,1],
    qf.ref0 = cffit$model_quantile_ref0[,1]
  ))
  return(plotdata)
}

# function to make plots
makecfplots <- function(cffit) {
  
  # make data
  cfdata <- getcfdat(cffit)
  
  # common options
  addopts.all <- function(x) {
    x <- x +
      theme(
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 1, face = "italic")
        ) +
      scale_x_continuous(name = "Quantile", breaks = seq(.1,.9,.1))
    return(x)
  }
  # options for effects only
  addopts.ce <- function(x) {
    x <- x +
      coord_cartesian(ylim = c(-.4, .5)) +
      labs(list(colour="")) + theme(legend.position="none")
    return(x)
  }
  
  # plots
  p.qfs <- ggplot(cfdata, aes(quant))+ ggtitle("Quantile functions") +
    geom_line(aes(y=qf.ref0, colour = "BCS")) + 
    geom_line(aes(y=qf.ref1, colour = "MCS")) +
    geom_line(aes(y=qf.cf, colour = "MCS (counterfactual)", linetype = "MCS (counterfactual)")) +
    scale_linetype_manual(values=c("dashed"), guide=FALSE) + 
    scale_colour_manual(values = c("#F8766D", "#00BFC4", "#00BFC4")) + #override colours
    theme(legend.justification=c(0,0), legend.position=c(0,.8), legend.title = element_blank())
  p.tot <- ggplot(cfdata, aes(quant, tot))+
    geom_line() + ggtitle("Total difference") +
    geom_ribbon(aes(ymin=tot.l,ymax=tot.u),alpha=0.3)
  p.str <- ggplot(cfdata, aes(quant, str))+
    geom_line() + ggtitle("Structure") +
    geom_ribbon(aes(ymin=str.l,ymax=str.u),alpha=0.3)
  p.com <- ggplot(cfdata, aes(quant, com))+
    geom_line() + ggtitle("Composition") +
    geom_ribbon(aes(ymin=com.l,ymax=com.u),alpha=0.3)
  
  cfplist <- list(p.qfs, p.tot, p.str, p.com) 
  cfplist <- lapply(cfplist, addopts.all) # apply options to all graphs
  cfplist[2:4] <- lapply(cfplist[2:4], addopts.ce) # apply options to all graphs
  
  return(cfplist)
}

# MAKE PLOTS
cfp.ext.m <- makecfplots(logitcf.ext.m)
cfp.ext.f <- makecfplots(logitcf.ext.f)
cfp.int.m <- makecfplots(logitcf.int.m)
cfp.int.f <- makecfplots(logitcf.int.f)


# EXT PLOTS
# arrange the plots in a single column
cfp.ext <- plot_grid( cfp.ext.m[[1]],cfp.ext.m[[2]],cfp.ext.m[[3]],cfp.ext.m[[4]],
                      cfp.ext.f[[1]],cfp.ext.f[[2]],cfp.ext.f[[3]],cfp.ext.f[[4]],
                  align = 'vh',
                  hjust = -1,
                  nrow = 2,
                  labels = c("A) Males", "", "", "","B) Females", "", "", ""), label_size = 17
)

# INT PLOTS
# arrange the plots in a single column
cfp.int <- plot_grid( cfp.int.m[[1]],cfp.int.m[[2]],cfp.int.m[[3]],cfp.int.m[[4]],
                      cfp.int.f[[1]],cfp.int.f[[2]],cfp.int.f[[3]],cfp.int.f[[4]],
                      align = 'vh',
                      hjust = -1,
                      nrow = 2,
                      labels = c("A) Males", "", "", "","B) Females", "", "", ""), label_size = 17
)



