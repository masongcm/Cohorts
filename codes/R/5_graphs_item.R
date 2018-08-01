
############################################################################################
## ---- ITEMINEQ_MAKE

# make items binary and keep items + decomposition
s2pb <- finaldata %>%
  mutate_at(paste0("X", c(1,2,3,4,7,8,9,10)), funs(b = recode(., "1"="1", "2"="0", "0"="1"))) %>% # 3 cat
  mutate_at(paste0("X", c(5,6,11)), funs(b = recode(., "1"="0", "0"="1"))) %>% # 2 cat
  select(id, cohort, sex, mpsla5, smkpr, fscl5wb, X1_b, X2_b, X3_b, X4_b, X5_b, X6_b, X7_b, X8_b, X9_b, X10_b, X11_b) %>%
  mutate_at(paste0("X", seq(1,11), "_b"), funs(as.numeric(as.character(.)))) %>%
  rename_at(paste0("X", seq(1,11), "_b"), ~ paste0("Xb", seq(1,11)))

# make father's social class binary, and reverse order (lowest first)
s2pb <- s2pb %>% 
  mutate(fscl5wb = na_if(fscl5wb, "No father fig.")) %>%
  mutate(fscl5wb = fct_rev(fscl5wb),
         smkpr = fct_rev(smkpr))


# function to return plot of items
itemineq <- function(data, categvar,
                     ylab = "Prevalence Ratio",
                     ylim = NULL
) {
  
  categ_enq <- enquo(categvar) # tidy eval

  # compute within cohort differences in item prevalence by mother education
  plotdata <- data %>%
    group_by(cohort, sex, !! categ_enq) %>%
    filter(!is.na(!! categ_enq)) %>% # nonmissing education
    summarise_at(vars(starts_with('Xb')), funs(mean(.))) %>% # prevalence by item 
    gather(key = item, value = prevalence, paste0("Xb", seq(1,11))) %>% # reshape to items in row
    group_by(cohort, sex, item) %>% 
    mutate(diff = lag(prevalence, default = NA) - prevalence) %>% # get differences between levels of mpsla5
    mutate(prevratio = lag(prevalence, default = NA) / prevalence) %>% # get differences between levels of mpsla5
    filter(!is.na(diff)) %>% # only non-null differences
    mutate(itemn = readr::parse_number(item)) %>%
    select(cohort, sex, item, itemn, prevratio, diff) %>%
    mutate(fac = ifelse(itemn > 6, "INT", "EXT"),
           diff = diff*100)
  
  # item labels
  plotdata$iteml <- factor(plotdata$itemn)
  levels(plotdata$iteml) <- c("(1)\nRestless",
                              "(2)\nSquirmy",
                              "(3)\nFights",
                              "(4)\nCan't settle",
                              "(5)\nTantrums",
                              "(6)\nDisobedient",
                              "(7)\nWorried",
                              "(8)\nAfraid of new",
                              "(9)\nSolitary",
                              "(10)\nUnhappy",
                              "(11)\nAches")
  
  # get maximum ratio for graph limit
  if (is.null(ylim)) {
    maxy <- max(plotdata$prevratio, na.rm = TRUE)+0.05
    miny <- min(plotdata$prevratio, na.rm = TRUE)-0.05
    ylim <- c(maxy,miny)
  }

  # common options for bar graphs
  addopts_prev <- function(x) {
    x <- x + theme(axis.title.x=element_blank(),
                   legend.position = "none") +
      scale_y_continuous(name = ylab) +
      coord_cartesian(ylim = ylim) +
      geom_hline(yintercept = 1, linetype="dashed") +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = .75) +
      labs(list(fill=""))
    return(x)
  }
  
  prev.ext.m <- ggplot(subset(plotdata, sex == "M" & fac == "EXT"), aes(iteml, prevratio, fill = cohort)) + ggtitle("Males Externalising")
  prev.int.m <- ggplot(subset(plotdata, sex == "M" & fac == "INT"), aes(iteml, prevratio, fill = cohort)) + ggtitle("Males Internalising")
  prev.ext.f <- ggplot(subset(plotdata, sex == "F" & fac == "EXT"), aes(iteml, prevratio, fill = cohort)) + ggtitle("Females Externalising")
  prev.int.f <- ggplot(subset(plotdata, sex == "F" & fac == "INT"), aes(iteml, prevratio, fill = cohort)) + ggtitle("Females Internalising")
  prevlist <- list(prev.ext.m, prev.int.m, prev.ext.f, prev.int.f)
  prevlist <- lapply(prevlist, function(x) addopts_prev(x)) # apply options to all graphs
  pcol <- plot_grid( prevlist[[1]], prevlist[[3]], prevlist[[2]], prevlist[[4]],
                     align = 'vh',
                     hjust = -1,
                     nrow = 2
  )
  # add legend
  legend_b <- get_legend(prevlist[[1]] + theme(legend.position="bottom", legend.justification="center", legend.box.just = "bottom"))
  plotout <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
  
  # return data and plot
  out <- list(data = plotdata,
              plot = plotout)
  return(out)
}

itempl_fsc <- itemineq(s2pb, fscl5wb, ylab = "Prevalence Ratio (Blue vs. White Collar)")$plot
itempl_ps <- itemineq(s2pb, mpsla5, ylab = "Prevalence Ratio (Low vs. High Education)")$plot
itempl_sm <- itemineq(s2pb, smkpr, ylab = "Prevalence Ratio (Smoker vs. Non-smoker)")$plot





