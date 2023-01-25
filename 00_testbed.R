## Testbed for function

rerf <- function(X, y, re, data, 
                 initre = 0, max_iter = 100, tol = 1e-5) {
  
  library(lme4)
  library(randomForest)
  
  ## Initialize
  y_adj <- y - initre 
  error_tol <- 1e6
  
  ## Model 1
  rf <- randomForest(X, y_adj)
  
  while(iter < max_iter & error_tol > tol) {
    print(re)
  }
  
  
}
