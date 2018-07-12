##---- FA_INV_PARTIAL
# partial intercept invariance analysis

dif_comm <- "
EXT =~ c(NA, NA, NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA, NA, NA)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2, t1_2, t1_2)*t1 + c(t2_2, t2_2, t2_2, t2_2)*t2
X1 | c(t1_1, t1_1, t1_1, t1_1)*t1 + c(t2_1, t2_1, t2_1, t2_1)*t2
X3 | c(t1_3, t1_3, t1_3, t1_3)*t1 + c(t2_3, t2_3, t2_3, t2_3)*t2
X4 | c(t1_4, t1_4, t1_4, t1_4)*t1 + c(t2_4, t2_4, t2_4, t2_4)*t2
X5 | c(t1_5, t1_5, t1_5, t1_5)*t1
X6 | c(t1_6, t1_6, t1_6, t1_6)*t1
X8 | c(t1_8, t1_8, t1_8, t1_8)*t1 + c(t2_8, t2_8, t2_8, t2_8)*t2
X7 | c(t1_7, t1_7, t1_7, t1_7)*t1 + c(t2_7, t2_7, t2_7, t2_7)*t2
X9 | c(t1_9, t1_9, t1_9, t1_9)*t1 + c(t2_9, t2_9, t2_9, t2_9)*t2
X10 | c(t1_10, t1_10, t1_10, t1_10)*t1 + c(t2_10, t2_10, t2_10, t2_10)*t2
X11 | c(t1_11, t1_11, t1_11, t1_11)*t1
X1 ~~ c(1,NA,NA,NA)*X1
X2 ~~ c(1,NA,NA,NA)*X2
X3 ~~ c(1,NA,NA,NA)*X3
X4 ~~ c(1,NA,NA,NA)*X4
X5 ~~ c(1,1,1,1)*X5
X6 ~~ c(1,1,1,1)*X6
X7 ~~ c(1,NA,NA,NA)*X7
X8 ~~ c(1,NA,NA,NA)*X8
X9 ~~ c(1,NA,NA,NA)*X9
X10 ~~ c(1,NA,NA,NA)*X10
X11 ~~ c(1,1,1,1)*X11
EXT ~~ c(1,NA,NA,NA)*EXT
INT ~~ c(1,NA,NA,NA)*INT
EXT ~~ NA*INT
EXT ~ c(0,NA,NA,NA)*1
INT ~ c(0,NA,NA,NA)*1
"

itm <- seq(1,11)
itmEXT <- seq(1,6)
itmINT <- seq(7,11)

# unique 4-way combinations of items
grid <- expand.grid(itmEXT,itmEXT,itmINT,itmINT)
grid <- grid[grid[,1]!=grid[,2],]
grid <- grid[grid[,3]!=grid[,4],]
grid <- unique(grid)
grid.sort <- t(apply(grid, 1, sort))
grid <- grid[!duplicated(grid.sort),]
grid <- cbind(data.frame(seq(1,nrow(grid))), grid)
colnames(grid) <- c("modnum", "EXT1", "EXT2", "INT1", "INT2")

# initialise
dif <- list()
dif_syn <- list()
dif_add <- list()
dif_fit <- list()
dif_means <- list()

for (i in 1:nrow(grid)) {
  # separate fixed and free items
  fixed <- as.vector(grid[i,c("EXT1", "EXT2", "INT1", "INT2")])
  free  <- itm[! itm %in% fixed]
  dif_add[[i]] <- paste0(
    paste0("X", free, " ~ c(0, NA, NA, NA)*1", collapse = "\n"),
    "\n",
    paste0("X", fixed, " ~ c(0, 0, 0, 0)*1", collapse = "\n")
  )
  # estimate model
  dif_syn[[i]] <- paste0(dif_comm, dif_add[[i]]) 
  dif[[i]] <- cfa(dif_syn[[i]], data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
  dif_means[[i]] <- lavInspect(dif[[i]], "mean.lv")
  dif_fit[[i]] <- printfit(dif[[i]])
}

# collapse means to matrix
allmeans <- matrix(unlist(dif_means), ncol = length(unlist(dif_means[[1]])), byrow=TRUE)
colnames(allmeans) <- names(unlist(dif_means[[1]]))
# collapse fit to matrix
allfit <- matrix(unlist(dif_fit), ncol = length(dif_fit[[1]]), byrow=TRUE)
colnames(allfit) <- names(unlist(dif_fit[[1]]))
# bind
partialfit <- cbind(grid, allfit, allmeans)
rm(allfit, allmeans)

# show models with best RMSEA
head(res[order(res$rmsea),], 15)

# pick best fitting model
fa.tlip <- list()
fa.tlip[[1]] <- dif[[110]]


##---- FA_INV_PARTIAL_SCORES
# partial intercept invariance analysis

# examine scores
scores_dif1 <- lavPredict(dif[[110]], newdata = items[[1]])
scores_dif1 <- cbind(items[[1]], do.call(rbind, scores_dif1))
scores_dif1 <- scores_dif1 %>% 
  dplyr::select(id, EXT, INT) %>%
  dplyr::rename(EXTd1 = EXT, INTd1 = INT)

scores_dif2 <- lavPredict(dif[[95]], newdata = items[[1]])
scores_dif2 <- cbind(items[[1]], do.call(rbind, scores_dif2))
scores_dif2 <- scores_dif2 %>% 
  dplyr::select(id, EXT, INT) %>%
  dplyr::rename(EXTd2 = EXT, INTd2 = INT)

scores_dif3 <- lavPredict(dif[[117]], newdata = items[[1]])
scores_dif3 <- cbind(items[[1]], do.call(rbind, scores_dif3))
scores_dif3 <- scores_dif3 %>% 
  dplyr::select(id, EXT, INT) %>%
  dplyr::rename(EXTd3 = EXT, INTd3 = INT)

mergedscores <- merge(scores2plot, scores_dif1, by = 'id')
mergedscores <- merge(mergedscores, scores_dif2, by = 'id')
mergedscores <- merge(mergedscores, scores_dif3, by = 'id')
plot(mergedscores$INTd1, mergedscores$INTd3)
