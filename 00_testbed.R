## Testbed for function
set.seed(42)

rerf <- function(X, y, re, data,
                 initre = 0, max_iter = 100, tol = 1e-5) {
  
  library(lme4)
  library(randomForest)
  
  ## Initialize
  y_adj <- y - initre 
  error_tol <- 1e-1
  iter <- 1
  
  ## Model 1
  while(iter < max_iter & error_tol > tol) {
    
    ## E-step
    rf_mod <- randomForest(X, y_adj)
    
    yhat <- predict(rf_mod, X)
    rf_resid <- y - yhat
    rf_resid <- y - rf_mod$predicted
    
    re_formula <- as.formula(paste('rf_resid ~ -1 +', re))
    re_mod <- lmer(re_formula, data = data)
    print(paste(iter, logLik(re_mod)))
    
    ## M-step
    re_hat <- predict(re_mod)
    
    #  y-Zb
    y_adj <- y - re_hat
    
    ## Iterations
    iter <- iter + 1
  }
  
  return(list(rf = rf_mod, lmer = re_mod, re = ranef(re_mod)))
  
}

sleep <- read.csv("data/sleepstudy.csv")

X = data.frame(Days = sleep$Days)
y = sleep$Reaction

re = "(Days | Subject)"

fit <- rerf(X, y, re, data = sleep, max_iter = 100)
