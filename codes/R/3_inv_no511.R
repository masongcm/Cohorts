############################################################################################
## ---- FA_INV_AUX_NO511
# robustness checks without items 5-11

# WE CONFIGURAL - THETA PARAMETERISATION (Condition 8)
c.th.4.no511 <- glue::glue_collapse(readLines(paste0(dir_syntax, "c_th_4_no511.lav")), sep=" \n  ")

# WE THRESHOLD+LOADINGS invariance, theta par (Condition 19)
tl.th.4.no511 <- glue::glue_collapse(readLines(paste0(dir_syntax, "tl_th_4_no511.lav")), sep=" \n  ")

# WE THRESHOLD+LOADINGS+INTERCEPT invariance, theta par (Condition 27)
tli.th.4.no511 <- glue::glue_collapse(readLines(paste0(dir_syntax, "tli_th_4_no511.lav")), sep=" \n  ")

# MODEL 6: separate gender groups, not age adjusted, no items 5-11
fa.c[[6]]   <- cfa(c.th.4.no511, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[6]]  <- cfa(tl.th.4.no511, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[6]] <- cfa(tli.th.4.no511, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

############################################################################################
## ---- FA_SCORES_NO511
# score model selected as final
fascores_no511 <- cbind(fadata[[1]], do.call(rbind, lavPredict(fa.tl[[6]], newdata = fadata[[1]])))


############################################################################################
## ---- SCATTER_NO511

fss <- fascores %>%
  select(id, EXT, INT) %>%
  inner_join(select(fascores_no511, id, EXT, INT), by = "id") %>%
  gather(key = skill, value = value, EXT.x, EXT.y, INT.x, INT.y) %>%
  separate(skill, c("skill", "itm")) %>%
  mutate(itm = ifelse(itm == "x", "full", "no511")) %>%
  mutate(skill = ifelse(skill == "EXT", "Externalising", "Internalising")) %>%
  spread(itm, value)

fscorrs <- fss %>% # compute correlations
  group_by(skill) %>%
  summarise(label = paste0("r = ", sprintf("%.3f", cor(full, no511))))
  
ggplot(fss, aes(x = full, y = no511)) + 
  geom_point(alpha = .15) + 
  coord_fixed() +
  facet_grid(cols = vars(skill)) + 
  xlab("Score (all items)") + ylab("Score (excl. items 5 and 11)") + 
  geom_text(
    data    = fscorrs,
    mapping = aes(x = -3, y = 1, label = label)
  ) +
  theme(
    text=element_text(family="Times"),
    strip.background =element_rect(fill="white"),
    strip.text.x = element_text(face = "bold", margin = margin(2,0,2,0, "pt"))
  )
  
rm(fss, fscorrs)