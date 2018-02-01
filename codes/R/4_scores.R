# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES
scores <- list()
scoresdf <- list()
items.scored <- list()
for (i in 4:5) { # only specifications with separate gender groups

  scores[[i]] <- predict(finalmod[[i]], newdata = items[[i]])
  scoresdf[[i]] <- do.call(rbind, scores[[i]])
  items.scored[[i]] <- cbind(items[[i]], scoresdf[[i]])
  
  # residualise scores in age
  items.scored[[i]]$EXTr <- residuals(lm(EXT ~ age, data=items.scored[[i]], na.action = na.exclude))
  items.scored[[i]]$INTr <- residuals(lm(INT ~ age, data=items.scored[[i]], na.action = na.exclude))
  
  # raw scores
  items.scored[[i]]$EXT.RAW <- rowSums(apply(items.scored[[i]][,paste("X", seq(1,6), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
  items.scored[[i]]$INT.RAW <- rowSums(apply(items.scored[[i]][,paste("X", seq(7,11), sep="")], 2, function(x) as.numeric(x)), na.rm = T)
  
}

# mean by sex and cohort
aggregate(items.scored[[4]][, c("EXT", "EXT.RAW", "INT", "INT.RAW")], list(items.scored[[4]]$cohortsex), mean, na.rm=T)

# select scores to plot
scores2plot <- items.scored[[4]]

