# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES_MAIN
# score model selected as final
scores.final <- lavPredict(finalmod, newdata = items[[1]])

############################################################################################
## ---- FA_SCORES_BIND
# bind scores with data
scores2plot <- cbind(items[[1]], do.call(rbind, scores.final))
