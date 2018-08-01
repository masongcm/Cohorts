# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES_MAIN
# score model selected as final
scores.final <- lavPredict(finalmod, newdata = items[[1]])

############################################################################################
## ---- FA_SCORES_BIND
# bind scores with data
finaldata <- cbind(items[[1]], do.call(rbind, scores.final))

############################################################################################
## ---- FA_SCORES_ALT
# score alternative models
scores.final_alt <- lavPredict(dif[[102]], newdata = items[[1]])
scores2plot_alt <- cbind(items[[1]], do.call(rbind, scores.final_alt))

scores.final_alt2 <- lavPredict(dif[[117]], newdata = items[[1]])
scores2plot_alt2 <- cbind(items[[1]], do.call(rbind, scores.final_alt2))