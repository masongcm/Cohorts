###################################################################
## ---- REGS_DEP

# function to extract relevant pvalues from interacted model
# outstring: whether to output as string or as numeric (so can be averaged for bootstrap)
extractp <- function(mod, outstring = FALSE) {
  coefs <- summary(mod)$coefficients
  coefs <- data.frame(tail(coefs, dim(coefs)[1]/2)) # select interactions (second half of table)
  colnames(coefs) <- c("est", "se", "t", "pval")
  
  coefs2 <- coefs %>% 
    tibble::rownames_to_column(var = "var") %>%
    mutate(var2 = gsub(":cohortMCS", "", var)) %>%
    filter(var2!="cohortMCS") %>% # eliminate string "cohortMCS"
    filter(!grepl(omitcoefs, var2)) # remove coefficients to omit
  
  if (outstring==TRUE) pvalvec <- prnpval(as.vector(coefs2$pval))
  if (outstring==FALSE) pvalvec <- as.vector(coefs2$pval)
  names(pvalvec) <- coefs2$var2
  
  # insert blanks corresponding to coefficient groups
  pvalvec2 <- NULL
  if (outstring==TRUE) for (i in 1:length(grouplabs)) pvalvec2 <- c(pvalvec2, "", rep(NA, length(grouplabs[[i]])))
  if (outstring==FALSE) for (i in 1:length(grouplabs)) pvalvec2 <- c(pvalvec2, 999, rep(NA, length(grouplabs[[i]])))
  pvalvec2[is.na(pvalvec2)] <- pvalvec
  if (outstring==FALSE) pvalvec2[pvalvec2 == 999] <- NA
  
  return(pvalvec2)
}


# REGRESSIONS OF EXT/INT ON CHILDHOOD VARIABLES
# add region to vector of regressors
decvars_reg <- c(decvars, c("region"))
decvars_int_reg <- c(decvars_int, c("region"))

# labels for groups (texreg)
grouplabs <- list("Maternal education (5)" = 1, 
                  "Maternal employment (5)" = 2,
                  "Father occ. (5) - White collar = 0" = 3:4, 
                  "Maternal background (0)" = 5:7,
                  "Pregnancy" = 8:10)
# labels for variables (texreg)
varlabs <- c("Post-compulsory",
             "Employed",
             "Blue collar", "No father figure",
             "Age", "Unmarried", "Nonwhite child",
             "Firstborn", "Mother smoked in pregnancy", "(log) Birthweight"
)

# formulas
form_ext <- as.formula(paste("EXT ~ ", pplus(decvars_reg)))
form_int <- as.formula(paste("INT ~ ", pplus(decvars_reg)))

# formulas with full cohort interactions
cohort_interactions <- paste0("cohort:", decvars_reg)
form_exti <- as.formula(paste("EXT ~ ", pplus(decvars_reg), "+ cohort +", pplus(cohort_interactions)))
form_inti <- as.formula(paste("INT ~ ", pplus(decvars_reg), "+ cohort +", pplus(cohort_interactions)))

# variables to omit from output
toomit <- c("region", "(Intercept)", "numch5", "mheight", "nprevst", "preterm")
omitcoefs <- paste0(toomit, collapse = "|") # regexp

# regressions
r_ext <- list()
r_int <- list()
for (s in c("M", "F")) {
  for (c in c("BCS", "MCS")) {
    cs <- paste0(c,".",s)
    
    r_ext[[cs]] <- list()
    r_int[[cs]] <- list()
    
    # OLS
    r_ext[[cs]][["OLS"]]  <- lm(form_ext,  data=subset(regdata, cohortsex==cs))
    r_int[[cs]][["OLS"]]  <- lm(form_int,  data=subset(regdata, cohortsex==cs))
    
    # OLS Bootstrap
    #bsamp <- merge
    # r_ext[[cs]][["OLSbootse"]]
    
    # Tobit
    r_ext[[cs]][["Tobit"]]  <- VGAM::vglm(form_ext, VGAM::tobit(Upper = max(regdata$EXT)), data=subset(regdata, cohortsex==cs))
    r_int[[cs]][["Tobit"]]  <- VGAM::vglm(form_int, VGAM::tobit(Upper = max(regdata$INT)), data=subset(regdata, cohortsex==cs))
    
  }
  
  # extract pvalues of interactions
  r_ext[[paste0(s,"p")]] <- extractp(lm(form_exti,  data=subset(regdata, sex==s)), outstring = TRUE)
  r_int[[paste0(s,"p")]] <- extractp(lm(form_inti,  data=subset(regdata, sex==s)), outstring = TRUE)
  
}

# bootstrap SEs
nboot <- length(bootscores)

for (s in c("M", "F")) {
  for (c in c("BCS", "MCS")) {
    
    bootcoefs_ext <- NULL
    bootcoefs_int <- NULL
    
    cs <- paste0(c,".",s)
    
    cat("\nInference for", cs, "\n")
    for (b in 1:nboot) {
      
      cat("\r Bootstrap sample", b, "of", nboot)
      
      # fetch scores from bootstrap samples
      bsamp <- regdata %>%
        select(-EXT, -INT) %>%
        right_join(bootscores[[b]], by = "id")
      
      # compute coefficients
      bootcoefs_ext <- cbind(bootcoefs_ext, as.matrix(lm(form_ext,  data=subset(bsamp, cohortsex==cs))$coefficients))
      bootcoefs_int <- cbind(bootcoefs_int, as.matrix(lm(form_int,  data=subset(bsamp, cohortsex==cs))$coefficients))
      
    }
    
    # compute standard errors
    r_ext[[cs]][["OLSbootse"]] <- apply(bootcoefs_ext, MARGIN = 1, FUN = sd)
    r_int[[cs]][["OLSbootse"]] <- apply(bootcoefs_int, MARGIN = 1, FUN = sd)
    
  }
  # 
  # # pvalues of interactions
  # bootpi_ext <- NULL
  # bootpi_int <- NULL
  # cat("\nInteraction pvalues for gender", s, "\n")
  # for (b in 1:nboot) {
  #   # fetch scores from bootstrap samples
  #   bsamp <- regdata %>%
  #     select(-EXT, -INT) %>%
  #     right_join(bootscores[[b]], by = "id")
  #   # extract pvalues
  #   cat("\r Bootstrap sample", b, "of", nboot)
  #   bootpi_ext <- cbind(bootpi_ext, extractp(lm(form_exti,  data=subset(bsamp, sex==s))))
  #   bootpi_int <- cbind(bootpi_int, extractp(lm(form_inti,  data=subset(bsamp, sex==s))))
  # }
  # r_ext[[paste0(s,"pboot")]] <- apply(bootpi_ext, MARGIN = 1, FUN = mean)
  # r_int[[paste0(s,"pboot")]] <- apply(bootpi_int, MARGIN = 1, FUN = mean)
  
}
