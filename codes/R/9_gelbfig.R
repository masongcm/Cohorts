

gelbdata <- read.dta(paste0(dir_data, "gelbres.dta"), convert.factors = F) # all BCS data

# keep only conditional and explained 
gelbdata <- gelbdata[gelbdata$varname %in% c("fulleff","med1","med2","med3","med4"),]
gelbdata$varname[gelbdata$varname=="fulleff"] <- "med0"

gelbdata$M_BCS <- as.numeric(substr(gelbdata$coef_1_1,2,7))
gelbdata$M_MCS <- as.numeric(substr(gelbdata$coef_2_1,2,7))
gelbdata$F_BCS <- as.numeric(substr(gelbdata$coef_1_2,2,7))
gelbdata$F_MCS <- as.numeric(substr(gelbdata$coef_2_2,2,7))

gelbdata_ext <- gelbdata[gelbdata$outcome=="EXT",names(gelbdata) %in% c("varname", "M_BCS", "M_MCS", "F_BCS", "F_MCS")]

gelbdata_ext <- reshape2::melt(gelbdata_ext, id=c("varname"))
colnames(gelbdata_ext) <- c("vgroup", "cohortsex", "value")
ggplot(data = gelbdata_ext, aes(x = cohortsex, y= value, fill=vgroup)) + geom_bar(stat = "identity")