# SCRIPT TO COMPARE SCORED FACTORS FROM DIFFERENT MODELS

############################################################################################
## ---- FA_SCORES_COMPARE

# 4 group with no intercept invariance
scores.tl <- predict(fa.tl[[7]], newdata = items[[7]]) # new scores prediction
items.scored.tl <- cbind(items[[7]][,c("id", "cohortsex", "faminc_real", "incq")], do.call(rbind, scores.tl))
names(items.scored.tl)[names(items.scored.tl) %in% c("EXT","INT")] <- c("EXTtl", "INTtl")

# 4 group with no intercept invariance, overlapping ages (model 9)
items.scored.tlir <- cbind(items[[9]][,c("id", "cohortsex")], do.call(rbind, scores[[9]]))
names(items.scored.tlir)[names(items.scored.tlir) %in% c("EXT","INT")] <- c("EXTtlir", "INTtlir")

# 4 group with intercept invariance, no age adjustment (model 7)
items.scored.tli <- cbind(items[[7]][,c("id", "cohortsex")], do.call(rbind, scores[[7]]))
names(items.scored.tli)[names(items.scored.tli) %in% c("EXT","INT")] <- c("EXTtli", "INTtli")

# 4 group with intercept invariance, age adjustment (model 8)
items.scored.tlia <- cbind(items[[8]][,c("id", "cohortsex")], do.call(rbind, scores[[8]]))
names(items.scored.tlia)[names(items.scored.tlia) %in% c("EXT","INT")] <- c("EXTtlia", "INTtlia")

# 4 group with intercept invariance, age adjustment (constrained) (model 10)
items.scored.tliac <- cbind(items[[10]][,c("id", "cohortsex")], do.call(rbind, scores[[10]]))
names(items.scored.tliac)[names(items.scored.tliac) %in% c("EXT","INT")] <- c("EXTtliac", "INTtliac")

# 4 group MIMIC
items.scored.mim <- cbind(items[[10]][,c("id", "cohortsex")], do.call(rbind, scores[[11]]))
names(items.scored.mim)[names(items.scored.mim) %in% c("EXT","INT")] <- c("EXTmim", "INTmim")

# merge scores
compare.scores <- merge(items.scored.tl, items.scored.tli, by=c("id", "cohortsex"))
compare.scores <- merge(compare.scores, items.scored.tlir, by=c("id", "cohortsex"))
compare.scores <- merge(compare.scores, items.scored.tlia, by=c("id", "cohortsex"))
compare.scores <- merge(compare.scores, items.scored.tliac, by=c("id", "cohortsex"))
compare.scores <- merge(compare.scores, items.scored.mim, by=c("id", "cohortsex"))

# assemble means
means.ext <- aggregate(compare.scores[, c(grep("EXT", names(compare.scores), value=T))], list(compare.scores$cohortsex), mean)
means.ext <- cbind(group = factor(c(1,3,2,4)), means.ext)
levels(means.ext$group) <- c("BCS Males", "MCS Males", "BCS Females", "MCS Females")
means.ext <- means.ext[order(means.ext$group),]
means.ext <- as.matrix(means.ext[, !names(means.ext) %in% "Group.1"],3)

means.int <- aggregate(compare.scores[, c(grep("INT", names(compare.scores), value=T))], list(compare.scores$cohortsex), mean)
means.int <- cbind(group = factor(c(1,3,2,4)), means.int)
levels(means.int$group) <- c("BCS Males", "MCS Males", "BCS Females", "MCS Females")
means.int <- means.int[order(means.int$group),]
means.int <- as.matrix(means.int[, !names(means.int) %in% "Group.1"],3)


# ggplot(compare.scores, aes(x=EXTtl, y=EXTtli, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtl, y=EXTtlir)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtl2, y=EXTtli)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtli, y=EXTtlia, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtli, y=EXTtliac, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtli, y=EXTmim, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=INTtli, y=INTtliac, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=INTtli, y=INTmim, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()
# ggplot(compare.scores, aes(x=EXTtliac, y=EXTmim, color=cohortsex)) + geom_point(size=2, alpha=.2) + coord_fixed()

