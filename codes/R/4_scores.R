# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES_MAIN
# score model selected as final
scores.final <- predict(finalmod, newdata = items[[1]])
scores2plot <- cbind(items[[1]], do.call(rbind, scores.final))

