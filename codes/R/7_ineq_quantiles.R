##---- QDTABLE

# get quantile difference by group
getQdiffs <- function(data) {
  qt <- c(.1, .25, .5, .75, .9)
  # select quantiles
  # nest data (subsets by cohort, sex, and skill)
  data %>%
    mutate_at(c("sex", "cohort"),  as.character) %>%
    select(id, cohort, sex, EXT, INT) %>%
    gather(key = skill, value = value, c("EXT", "INT")) %>% # make EXT/INT long (skill)
    group_by(cohort, sex, skill) %>%
    nest() %>% # nest (one dataframe for each cohort/sex/skill combination)
    mutate(
      quantiles = map_chr(data, ~quantile(.$value, qt, na.rm=TRUE) %>% stringr::str_c(collapse = '_'))
    ) %>%
    separate(quantiles, paste0('q', 100*qt), sep = '_') %>%
    mutate_at(vars(starts_with('q')), as.numeric) %>%
    mutate(
      d9010 = q90-q10,
      d7525 = q75-q25,
      d9050 = q90-q50,
      d5010 = q50-q10
    ) %>%
    select(cohort, sex, skill, starts_with("d"), -data)
}

# compute quantile differences for bootstrap samples
bdatlist <- list()
for (b in 1:length(bootscores)) {
  bdatlist[[b]] <- finaldata %>%
    select(id, sex, cohort) %>%
    right_join(bootscores[[b]], by = "id") %>% # join with BOOT scores
    getQdiffs() %>%
    mutate(bootrep = b) # record bootstrap rep
}

qdtable <- bind_rows(bdatlist) %>%
  gather(key = pcdiff, value = value, starts_with("d")) %>%
  group_by(cohort, sex, skill, pcdiff) %>% # summarise QDs across boot samples
  summarise(
    med = median(value),
    ciu = quantile(value, .975),
    cil = quantile(value, .025),
    medci = sprintf("%s\\n[%s,%s]", prn(med), prn(cil), prn(ciu))
  ) %>%
  select(-med, -ciu, -cil) %>%
  # unite cohortsex and reorder
  unite(cohortsex, c("cohort", "sex"), sep=".") %>%
  mutate(cohortsex = fct_relevel(factor(cohortsex), c("BCS.M", "MCS.M", "BCS.F", "MCS.F"))) %>%
  arrange(cohortsex) %>%
  spread(cohortsex, medci) %>%
  mutate(
    pcdiff = paste0(str_sub(pcdiff,2,3), " - ",  str_sub(pcdiff,4,5))
    )

# qdtable[,-1] %>% # drop first column
#   kable("latex", escape = F, booktabs = T, linesep = "", align=c('l','c','c','c','c'),
#         col.names = c("Quantile diff.", "BCS (1970)", "MCS (2000/1)", "BCS (1970)", "MCS (2000/1)")) %>%
#   add_header_above(c(" " = 1,  "Males" = 2, "Females" = 2)) %>%
#   column_spec(2:5, width = "3em") %>%
#   mutate_all(linebreak)
#   
  

  
