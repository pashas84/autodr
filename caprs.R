install.packages("formula.tools")
library(formula.tools)

caprs <- function(x){
  sum <- summary(x)
  num <- which.min(sum$bic)
  y <- sum$which[num,]
  z <- y[y == TRUE]
  rh <- paste(names(z[-1]), collapse = "+")
  lh <- lhs(x$call[[2]])
  form <- paste(lh, rh, sep = "~")
  as.formula(form)
}