###################################################################
## ---- GELB_FIG

# read data from stata
gelbdata <- read.dta(paste0(dir_data, "gelbres.dta"), convert.factors = F) # all BCS data

# keep only conditional and explained 
gelbdata <- gelbdata[gelbdata$varname %in% c("fulleff","med1","med2","med3","med4"),]
gelbdata$varname[gelbdata$varname=="fulleff"] <- "med0"

# extract estimates
gelbdata$MBCS_v <- as.numeric(substr(gelbdata$coef_1_1,2,7))
gelbdata$MMCS_v <- as.numeric(substr(gelbdata$coef_2_1,2,7))
gelbdata$FBCS_v <- as.numeric(substr(gelbdata$coef_1_2,2,7))
gelbdata$FMCS_v <- as.numeric(substr(gelbdata$coef_2_2,2,7))

# keep relevant variables
gelbdata <- gelbdata[,c("outcome", "varname", "MBCS_v", "MMCS_v", "FBCS_v", "FMCS_v")]

# get percentages by outcome
varLabel <- paste0(c("MBCS", "MMCS", "FBCS", "FMCS"),"_v")
gelbdata <- gelbdata %>% dplyr::group_by(outcome) %>% dplyr::mutate_at(vars(varLabel), funs(p = round(((./sum(.))*100),1))) %>%
  setNames(gsub("_v_p", "_p", names(.)))



# reshape data
gelbdata <- reshape2::melt(gelbdata, id=c("outcome", "varname"))
gelbdata <- cbind(gelbdata$outcome, gelbdata$varname, 
                      reshape2::colsplit(gelbdata$variable, "_", c("cohortsex", "type")),
                  gelbdata$value)
colnames(gelbdata) <- c("outcome", "vgroup", "cohortsex", "type", "value")
gelbdata <- reshape2::dcast(gelbdata, formula = outcome+vgroup+cohortsex ~ type)
colnames(gelbdata) <- c("outcome", "vgroup", "cohortsex", "perc", "value")

# to show percentages
# https://stackoverflow.com/questions/34903368/how-to-center-stacked-percent-barchart-labels

# reverse ordering
gelbdata$cohortsex <- factor(gelbdata$cohortsex, levels = c("FMCS","FBCS","MMCS","MBCS"))
levels(gelbdata$cohortsex) <- c("Females\nMCS","Females\nBCS","Males\nMCS","Males\nBCS")

# name levels of groups
gelbdata$vgroup <- factor(gelbdata$vgroup)
levels(gelbdata$vgroup) <- c("Unexplained", decvarsgroups[2:length(decvarsgroups)])
gcols <- c("grey80", "orangered4","darkorange2","goldenrod4","goldenrod2")
names(gcols) <- levels(gelbdata$vgroup) # colors


# common graph syntax
addopts_gelb <- function(x) {
  x <- x +
    geom_bar(stat = "identity", alpha=.7, width = 0.5) + 
    geom_vline(xintercept = 2.5) +
    coord_flip(ylim = c(0.0,0.3)) +
    scale_y_continuous(name = "Difference (high vs low educated mothers)") + xlab("") +
    scale_fill_manual(values = gcols, name = "") +
    geom_text(aes(label=ifelse(value >= .02, paste0(sprintf("%.1f", perc),"\\%"),"")), position=position_stack(vjust=0.5)) +
    guides(fill=guide_legend(ncol=1,byrow=TRUE)) +
    theme(legend.position = "right")
  return(x)
}

# make plots
gelbplot_ext <- addopts_gelb(ggplot(data = subset(gelbdata, outcome=="EXT"), aes(x = cohortsex, y= value, fill=vgroup)))
gelbplot_int <- addopts_gelb(ggplot(data = subset(gelbdata, outcome=="INT"), aes(x = cohortsex, y= value, fill=vgroup)))

