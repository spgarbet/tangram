# Hmisc::lm.fit.qr.bare copied for internal use in tangram
# Copyright (C) 2017 Frank Harrell
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

