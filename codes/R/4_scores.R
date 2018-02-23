# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES
scores <- list()
scoresdf <- list()
items.scored <- list()
for (i in 7:11) { # only specifications with separate gender groups

  scores[[i]] <- predict(finalmod[[i]], newdata = items[[i]])
  scoresdf[[i]] <- do.call(rbind, scores[[i]])
  items.scored[[i]] <- cbind(items[[i]], scoresdf[[i]])
  
  # residualise scores in age
  items.scored[[i]]$EXTr <- residuals(lm(EXT ~ age, data=items.scored[[i]], na.action = na.exclude))
  items.scored[[i]]$INTr <- residuals(lm(INT ~ age, data=items.scored[[i]], na.action = na.exclude))
  
}


