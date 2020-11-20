rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014")
library(magrittr)

ds <- datasets::mtcars # dataset for demonstrations
ds
dplyr::glimpse(ds)

# mdl <- glm(formula = mpg ~ 1 + cyl + wt + hp, data=ds)
mdl <- glm(formula = am ~ 1 + cyl + wt + hp  , data=ds, family = binomial(link="logit") )
mdl

# useful functions working with GLM model objects
summary(mdl) # model summary
coefficients(mdl) # point estimates of model parameters (aka "model solution")
vcov(mdl) # covariance matrix of model parameters (inspect for colliniarity)
confint(mdl, level=0.95) # confidence intervals for the estimated parameters

predict(mdl); fitted(mdl) # generate prediction of the full model (all effects)
residuals(mdl) # difference b/w observed and modeled values
anova(mdl) # put results into a familiary ANOVA table
influence(mdl) # regression diagnostics

# create a model summary object to query
summod <- summary(mdl)

# parameter estimates
summod$coefficients

# degrees of freedom of the models
(dfF <- mdl$df.residual)              # FULL            ( df ERROR)
(dfR <- mdl$df.null)                  # RESTRICTED      ( df TOTAL)
(dfD <-dfR - dfF)                     # DIFFERENCE      ( df RESIDUAL) - gain in complexity / loss of simplicity

# misfit of the models
(SSE <- mdl$deviance);EF <- SSE       # FULL            (SS Error) - (EF)
(SST <- mdl$null.deviance); ER <- SST # RESTRICTED      (SS Total) - (ER)
(SSR <- SST - SSE)                    # DIFFERENCE      (SS Resisudal) - gain in accurcy / loss of misfit

# accuracy vs parsimony
(MSE <- EF / dfF)                     # FULL            (Mean Square Error)
(MST <- ER / dfR)                     # RESTRICTED      (Mean Square Total)
(MSR <- (ER - EF) / (dfR - dfF))      # DIFFERENCE      (Mean Square Residual); (MSR <-SSR/dfR)


(SS1 <- stats::anova(mdl))             # (Type I SS): ignor below, control above
(SS3 <- car::Anova(mdl,type="III"))  # Type III SS, control for everything


# to graph the F test we can use a custom graphing:
base::source("./scripts/graphing/graph-areaF.R")
areaF(misfit_full = SSE, df_full = dfF, misfit_reduced = SSR, df_reduced = dfR, scalemax = 3 )
