## RIF_PLOTS
iqs <- list()
iqs[["2575"]] <- "ΔQD 75-25"
iqs[["5090"]] <- "ΔQD 90-50"
iqs[["1050"]] <- "ΔQD 50-10"

skills <- c("EXT", "INT")
gends <- c("M", "F")
rif_beta <- list()
rif_plots <- list()
for (iq in names(iqs)) {
  for (s in skills) {
    for (g in gends) {
      file <- paste("rif", s, g, iq, sep = "_")
      rif_beta[[file]] <- read.csv(paste0(dir_stata_estimates, file, ".csv"), header=TRUE) %>%
        filter(
          (varname == "Tdifference" & eqname == "Overall:Tdifference") |
          (varname == "Pure_explained" & eqname == "Explained:Pure_explained") |
            (varname == "Specif_err" & eqname == "Explained:Specif_err") |
            (varname == "Pure_Unexplained" & eqname == "Unexplained:Pure_Unexplained") |
            (varname == "Reweight_err" & eqname == "Unexplained:Reweight_err")
        ) %>%
        rename(component = varname) %>%
        mutate(
          iq = iqs[[iq]],
          skill = s,
          g = g,
          gend = ifelse(g == "M", "Males", "Females")
        ) %>%
        select(component, b, skill, gend, iq)
    }
  }
  # append
  
}
rif_beta_all <- bind_rows(rif_beta)
# reorder factors
rif_beta_all$skill <- factor(rif_beta_all$skill, levels = c("EXT", "INT"))
rif_beta_all$gend <- factor(rif_beta_all$gend, levels = c("Males", "Females"))
rif_beta_all$iq <- factor(rif_beta_all$iq, levels = c("ΔQD 75-25", "ΔQD 90-50", "ΔQD 50-10"))


# STACKED BAR PLOT
ggplot(rif_beta_all, aes(skill, b, fill = component)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0) +
  facet_grid(~ iq + gend) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_rect(fill="white"))

