## Testbed for function

rerf <- function(X, y, re, data,
                 initre = 0, max_iter = 100, tol = 1e-5) {
  
  library(lme4)
  library(randomForest)
  
  ## Initialize
  y_adj <- y - initre 
  error_tol <- 1e6
  iter <- 1
  
  ## Model 1
  rf <- randomForest(X, y_adj)
  
  while(iter < max_iter & error_tol > tol) {
    print(paste(iter, re))
    iter <- iter + 1
  }
  
  
}

sleep <- read.csv("data/sleepstudy.csv")

X = data.frame(days = sleep$Days)
y = sleep$Reaction

re = "(1 | Subject)"

rerf(X, y, re)
