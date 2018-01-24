# FUNCTION FOR SCORES

############################################################################################
## ---- FA_SCORES
indices <- inspect(finalmod,"case.idx")   # indices for observations used
scores <- lavPredict(finalmod)
items.scored <- cbind(X[,c("ID", "faminc","incq" )], items.cb, matrix(NA, nrow(items.cb), 2))
colnames(items.scored) <- c("ID", "faminc","incq", colnames(items.cb), "EXT", "INT")
items.scored[indices[[1]],"EXT"] <- scores[[1]][,"EXT"]
items.scored[indices[[2]],"EXT"] <- scores[[2]][,"EXT"]
items.scored[indices[[1]],"INT"] <- scores[[1]][,"INT"]
items.scored[indices[[2]],"INT"] <- scores[[2]][,"INT"]

# residualise scores in age
items.scored$EXTr <- residuals(lm(EXT ~ age, data=items.scored, na.action = na.exclude))
items.scored$INTr <- residuals(lm(INT ~ age, data=items.scored, na.action = na.exclude))

# mean by sex and cohort
aggregate(items.scored[, c("EXT", "EXTr", "INT", "INTr")], list(items.scored$sex, items.scored$cohort), mean, na.rm=T)

# add raw scores
items.scored$EXT.RAW <- rowSums(items[,paste("X", seq(1,6), sep="")], na.rm = T)
items.scored$INT.RAW <- rowSums(items[,paste("X", seq(7,11), sep="")], na.rm = T)


