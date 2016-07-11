install.packages("leaps", "MASS", "forecast", "formula.tools")
library(leaps); library(MASS); library(forecast); library(formula.tools)

autodr <- function(formula, data, method){
  if (method == "bestsubsets") {
    fit <- regsubsets(formula, data = data)
    sum <- summary(fit)
    num <- which.max(sum$adjr2)
    xy <- sum$which[num,]
    x <- xy[xy == TRUE]
    rh <- paste(names(x[-1]), collapse = "+")
    lh <- lhs(formula)
    form <- paste(lh, rh, sep = "~")
    frmla <- as.formula(form)
    auto.arima(model.frame(frmla, data = data)[1], xreg = model.frame(frmla, data = data)[-1])
  }
  else if (method == "stepwise") {
    fit <- lm(formula, data = data)
    step <- stepAIC(fit, trace = 0)
    y <- lhs(as.formula(fit))
    auto.arima(model.frame(step)[1], xreg = model.frame(step)[-1])
  } 
}