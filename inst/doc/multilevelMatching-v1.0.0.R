## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(multilevelMatching)
simulated_data <- multilevelMatching::simulated_data
knitr::kable(head(simulated_data), digits = 2)

## ----boxplot, fig.height = 4, fig.width= 5, fig.align="center"-----------
boxplot(
  outcome ~ treatment, 
  data = simulated_data,
  xlab = "treatment level",
  ylab = "outcome",
  main = "Outcomes by treatment level"
)

## ------------------------------------------------------------------------
outcome <- simulated_data$outcome
treatment <- simulated_data$treatment
covar_matrix <- as.matrix(
  simulated_data[ ,names(simulated_data) %in% paste0("covar", 1:6)]
)

## ------------------------------------------------------------------------
identifying_names <- paste0(
  rep(letters[1:25],each = 12), 
  rep(letters[1:25], 12)
)
(
  length(identifying_names) == 
    length(unique(identifying_names)) 
) && 
  (
    length(identifying_names) == 
      NROW(simulated_data)
  )



## ------------------------------------------------------------------------
# names(outcome) <- identifying_names
names(treatment) <- identifying_names
# rownames(covar_matrix) <- identifying_names

## ------------------------------------------------------------------------
set.seed(123)
fit1 <- multilevelMatchX(
  Y = outcome,
  W = treatment,
  X = covar_matrix
)

## ------------------------------------------------------------------------
fit1

## ------------------------------------------------------------------------
set.seed(123)
fit2 <- multiMatch(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  match_on = "covariates"
)

## ------------------------------------------------------------------------
class(fit2)

## ------------------------------------------------------------------------
print(fit2)

## ------------------------------------------------------------------------
summary(fit2)

## ------------------------------------------------------------------------
names(fit2)

## ------------------------------------------------------------------------
knitr::kable(head(fit2$impute_mat_sorted), digits = 2)

## ------------------------------------------------------------------------
# GPSM <- "multinomiallogisticReg" 
GPSM <- "ordinallogisticReg"
# GPSM <- "existing" 

set.seed(123)
fit1 <- multilevelGPSMatch(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  GPSM = GPSM,
  Trimming = FALSE
)

rbind(Estimate = fit1$tauestimate, Variance = fit1$varestimate)

## ------------------------------------------------------------------------
set.seed(123)
fit2 <- multiMatch(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  match_on = "polr",
  trimming = FALSE
)

summary(fit2)

## ------------------------------------------------------------------------
set.seed(123)
fit3a <- multiMatch(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  match_on = "multinom",
  J_var_matches = 2,
  trimming = TRUE
)

set.seed(123)
fit3b <- multiMatch(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  match_on = "multinom",
  M_matches = 3,
  J_var_matches = 2,
  trimming = TRUE
)

## ------------------------------------------------------------------------
fit3a

## ------------------------------------------------------------------------
fit3b

## ------------------------------------------------------------------------
set.seed(123)
pr_w1 <- sample(x=c(0.3,0.5), replace=TRUE, size=length(treatment))
pr_w2 <- (1-pr_w1)/3
pr_w3 <- 1-(pr_w1+pr_w2)
existing_GPS_matrix <- cbind(pr_w1, pr_w2,pr_w3)

## ------------------------------------------------------------------------
#the following checks are also carried out under the hood
nrow(existing_GPS_matrix)==length(treatment)
ncol(existing_GPS_matrix)==length(unique(treatment))
all(rowSums(existing_GPS_matrix)==1)

## ------------------------------------------------------------------------
# set.seed(123)
# fit1 <- multilevelGPSMatch(
#   Y = outcome,
#   W = treatment,
#   X = existing_GPS_matrix,
#   Trimming = 0,
#   GPSM = "existing"
# )
set.seed(123)
fit2 <- multiMatch(
  Y = outcome,
  W = treatment,
  X = existing_GPS_matrix,
  trimming = 0,
  match_on = "existing"
)
fit2

## ------------------------------------------------------------------------
NS <- 5 ## The number of strata to divide into
linearp <- FALSE ## Use subclassification, not linear prediction
nboot <- 10 ## Number of bootstrap samples for variance estimation

## ------------------------------------------------------------------------
set.seed(123)
multilevelGPSStratification(
  Y = outcome,
  W = treatment,
  X = covar_matrix,
  GPSM = "multinomiallogisticReg",
  NS = NS,
  linearp = linearp,
  nboot = nboot
)

