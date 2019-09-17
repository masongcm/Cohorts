## ---- RIF_PLOTS_PREP

iqs <- list()
iqs[["2575"]] <- "ΔQD 75-25"
iqs[["5090"]] <- "ΔQD 90-50"
iqs[["1050"]] <- "ΔQD 50-10"

skills <- c("EXT", "INT")
gends <- c("M", "F")
rif_beta <- list()
for (iq in names(iqs)) {
  for (s in skills) {
    for (g in gends) {
      # read file
      rif_file <- paste("rif", s, g, iq, sep = "_")
      raw_file <- read.csv(paste0(dir_stata_estimates, rif_file, ".csv"), header=TRUE) %>%
        mutate(b = -b) # flip sign of difference to make MCS-BCS
      # extract total difference
      tdiff <- raw_file %>% 
        filter((varname == "Tdifference" & eqname == "Overall:Tdifference")) %>%
        pull(b)
      
      rif_beta[[rif_file]] <- raw_file %>%
        filter(
          (varname == "Tdifference" & eqname == "Overall:Tdifference") |
          (varname == "Pure_explained" & eqname == "Explained:Pure_explained") |
            (varname == "Specif_err" & eqname == "Explained:Specif_err") |
            (varname == "Pure_Unexplained" & eqname == "Unexplained:Pure_Unexplained") |
            (varname == "Reweight_err" & eqname == "Unexplained:Reweight_err")
        ) %>%
        rename(component = varname) %>%
        select(component, b) %>%
        mutate(
          b_pc = b/tdiff*100,
          iq = iqs[[iq]],
          skill = s,
          g = g,
          gend = ifelse(g == "M", "Males", "Females")
        )
    }
  }
}

# append
rif_beta_all <- bind_rows(rif_beta)

# reorder factors
rif_beta_all$skill <- factor(rif_beta_all$skill, levels = c("EXT", "INT"))
rif_beta_all$gend <- factor(rif_beta_all$gend, levels = c("Males", "Females"))
rif_beta_all$iq <- factor(rif_beta_all$iq, levels = c("ΔQD 75-25", "ΔQD 90-50", "ΔQD 50-10"))
rif_beta_all$component <- factor(
  rif_beta_all$component,
  levels = c("Tdifference", "Pure_explained", "Specif_err", "Pure_Unexplained", "Reweight_err")
  )
levels(rif_beta_all$component) <- c("Total Diff.", "Composition", "Specif. Error", "Coefficient", "Reweight. Error")

# common graph options
bptheme <- theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(), axis.line.x = element_blank(),
                 axis.ticks=element_blank(),
                 strip.background=element_rect(fill="white"),
                 panel.spacing = unit(1, "lines"))

# Normal BAR PLOT, by gender
palette_5 <- c("#d95f02", "#7570b3", "#b5b1e6", "#1b9e77", "#99c9bc")
rif_plots <- list()
for (g in c("Males", "Females")) {
  print(g)
  rif_beta_gend <- rif_beta_all %>% filter(gend == !!g) %>% 
    mutate(xaxis = 1)
  rif_plots[[g]] <- ggplot(rif_beta_gend, aes(xaxis, b, fill = component)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(g) +
    geom_hline(yintercept = 0) +
    ylab("Difference") + ylim(-.2, .4) +
    facet_grid(~ skill + iq) +
    scale_fill_manual("Component", values = palette_5) + 
    bptheme
}

## ---- RIF_PLOTS_PC
# Normal BAR PLOT, by gender, percentages
palette_4 <- c("#7570b3", "#b5b1e6", "#1b9e77", "#99c9bc")
rif_plots_pc <- list()
for (g in c("Males", "Females")) {
  print(g)
  rif_beta_gend <- rif_beta_all %>% filter(gend == !!g) %>% 
    filter(component != "Total Diff.") %>%
    mutate(xaxis = 1)
  rif_plots_pc[[g]] <- ggplot(rif_beta_gend, aes(xaxis, b_pc, fill = component)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(g) +
    geom_hline(yintercept = 0) +
    scale_fill_manual("Component", values = palette_4) + 
    ylab("% of Total Difference") +
    facet_grid(~ skill + iq) + 
    bptheme
}

## ---- RIF_PLOTS_STACKED
# stacked version
rif_beta_stacked <- rif_beta_all %>% filter(component != "Tdifference")

rif_plots_stacked <- ggplot(rif_beta_stacked, aes(skill, b, fill = component)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0) +
  facet_grid(~ iq + gend) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_rect(fill="white"))

