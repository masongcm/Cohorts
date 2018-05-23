###################################################################
## ---- REGS_DEP

# function to collapse regressors in formula
pplus <- function(x) paste0(x, collapse="+")

# REGRESSIONS OF EXT/INT ON CHILDHOOD VARIABLES
# add region to vector of regressors
decvars_reg <- c(decvars, c("region"))
decvars_int_reg <- c(decvars_int, c("region"))

# labels for groups (texreg)
grouplabs <- list("Father occupation (5)" = 1:2, 
                  "Maternal education (5)" = 3, 
                  "Maternal employment (5)" = 4,
                  "Maternal background (birth)" = 5:8,
                  "Pregnancy" = 9:14)
# labels for variables (texreg)
varlabs <- c("Blue collar", "No father",
             "Post-compulsory",
             "Employed",
             "Age", "Height", "Unmarried", "Nonwhite child",
             "Parity", "Firstborn", "Num prev. stillbirths",
             "Mother smoked in pregnancy", "Preterm birth",
             "(log) Birthweight"
)

# formulas
form_ext <- as.formula(paste("EXT ~ ", pplus(decvars_reg)))
form_int <- as.formula(paste("INT ~ ", pplus(decvars_reg)))

# regressions
r_ext <- list()
r_int <- list()
for (cs in c("BCS.M", "MCS.M", "BCS.F", "MCS.F")) {
  r_ext[[cs]] <- list()
  r_int[[cs]] <- list()
  
  # OLS
  r_ext[[cs]][["OLS"]]  <- lm(form_ext,  data=subset(finaldata, cohortsex==cs))
  r_int[[cs]][["OLS"]]  <- lm(form_int,  data=subset(finaldata, cohortsex==cs))
  
  # Tobit
  r_ext[[cs]][["Tobit"]]  <- VGAM::vglm(form_ext, VGAM::tobit(Upper = max(finaldata$EXT)), data=subset(finaldata, cohortsex==cs))
  r_int[[cs]][["Tobit"]]  <- VGAM::vglm(form_int, VGAM::tobit(Upper = max(finaldata$INT)), data=subset(finaldata, cohortsex==cs))
  
}
