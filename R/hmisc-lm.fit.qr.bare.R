lm.fit.qr.bare <- function(x, y, 
                           tolerance = NULL,
                           intercept=TRUE, xpxi=FALSE,
                           singzero=FALSE)
{
  if(!length(tolerance)) tolerance <- 1e-7
  if(intercept)
    x <- cbind(Intercept=1, x)
  else x <- as.matrix(x)
  z    <- lm.fit(x, y, tol=tolerance)
  coef <- z$coefficients
  if(singzero && any(isna <- is.na(coef))) coef[isna] <- 0.
    
  res <- z$residuals
  sse <- sum(res^2)
  sst <- sum((y - mean(y))^2)

  res <- list(coefficients = coef,    residuals = res, 
              rsquared     = 1 - sse / sst,
              fitted.values = z$fitted.values)
  if(xpxi) {
    p <- 1L : z$rank
    res$xpxi <- chol2inv(z$qr$qr[p, p, drop=FALSE])
  }
  res
}

