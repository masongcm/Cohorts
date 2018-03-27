#######################################################################################################
## ---- COUNTERF_PLOT

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
    tot.u = cffit$resTE[,4]
  ))
  return(plotdata)
}

cfpdata.ext.m <- getcfdat(logitcf.ext.m)
cfpdata.ext.f <- getcfdat(logitcf.ext.f)

# common options
addopts <- function(x) {
  x <- x + ylab("Quantile Effect") +
    scale_x_continuous(name = "", breaks = seq(.1,.9,.1)) +
    coord_cartesian(ylim = c(-.4, .5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

p.tot.m <- ggplot(cfpdata.ext.m, aes(quant, tot))+
  geom_line() + ggtitle("EXT Males, Total") +
  geom_ribbon(aes(ymin=tot.l,ymax=tot.u),alpha=0.3)
p.str.m <- ggplot(cfpdata.ext.m, aes(quant, str))+
  geom_line() + ggtitle("EXT Males, Structure") +
  geom_ribbon(aes(ymin=str.l,ymax=str.u),alpha=0.3)
p.com.m <- ggplot(cfpdata.ext.m, aes(quant, com))+
  geom_line() + ggtitle("EXT Males, Composition") +
  geom_ribbon(aes(ymin=com.l,ymax=com.u),alpha=0.3)
p.tot.f <- ggplot(cfpdata.ext.f, aes(quant, tot))+
  geom_line() + ggtitle("EXT Females, Total difference") +
  geom_ribbon(aes(ymin=tot.l,ymax=tot.u),alpha=0.3)
p.str.f <- ggplot(cfpdata.ext.f, aes(quant, str))+
  geom_line() + ggtitle("EXT Females, Structure") +
  geom_ribbon(aes(ymin=str.l,ymax=str.u),alpha=0.3)
p.com.f <- ggplot(cfpdata.ext.f, aes(quant, com))+
  geom_line() + ggtitle("EXT Females, Composition") +
  geom_ribbon(aes(ymin=com.l,ymax=com.u),alpha=0.3)

cfplist <- list(p.tot.m, p.str.m, p.com.m, p.tot.f, p.str.f, p.com.f) 
cfplist <- lapply(cfplist, addopts) # apply options to all graphs

# arrange the plots in a single column
cfp <- plot_grid( cfplist[[1]],cfplist[[2]],cfplist[[3]],cfplist[[4]],cfplist[[5]],cfplist[[6]],
                  align = 'vh',
                  hjust = -1,
                  nrow = 2
)
cfp

