# tangram a general purpose table toolkit for R
# Copyright (C) 2017-2018 Shawn Garbett
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


# Internal with no argument validation
smd_categorical  <- function(x, group, weight)
{
  # Total weights per category
  ct      <- tapply(weight, list(group, x), FUN=sum, default=0)
  ct      <- apply(ct, 1, function(i) i/sum(i))      # Normalize to probability
  nrow    <- dim(ct)[1]
  Tm      <- as.matrix(ct[2:nrow,1], ncol=1)
  Cm      <- as.matrix(ct[2:nrow,2], ncol=1)
  T_C     <- Tm - Cm
  S       <- -(tcrossprod(Tm, Tm) + tcrossprod(Cm, Cm)) / 2
  diag(S) <- diag((tcrossprod(Tm, 1-Tm) + tcrossprod(Cm, 1-Cm)) / 2 )

  sqrt(t(T_C) %*% solve(S) %*% T_C)[1,1]
}

# Internal with no argument validation
smd_continuous         <- function(x, g1, g2, weight)
{
  x1 <- weighted.mean(x[g1], weight[g1])
  x2 <- weighted.mean(x[g2], weight[g2])
  v1 <- sum((x[g1] - x1)^2) / (sum(g1) - 1)
  v2 <- sum((x[g2] - x2)^2) / (sum(g2) - 1)

  abs(x1 - x2) / sqrt( (v1+v2) / 2)
}

#' Compute the standardized mean distance between 2 groups for numerical
#' or categorical information. Using method described in
#' 'A unifed approach to measuring the effect size between two groups
#' using SAS' by Dongsheng Yand and Jarrod E. Dalton, 2012. SAS Global
#' Forum 2012
#'
#' @param x vector; data to estimate effect size for groups
#' @param group vector; the grouping variable.
#' @param weight vector; weighting information for x
#'
#' @importFrom stats qnorm weighted.mean
#' @export
standard_difference <- function(x, group, weight=NULL)
{
  # Force Factor
  if(!inherits(group,"factor"))   group <- as.factor(group)
  if(length(x) != length(group))  stop("smd() call x and group lengths differ")
  if(length(levels(group)) != 2)  stop("smd() call must have only 2 groups")

  # Remove NA
  valid  <- !is.na(x) & !is.na(group)
  x      <- x[valid]
  group  <- group[valid]

  n1 <- sum(group == levels(group)[1])
  n2 <- sum(group == levels(group)[2])

  # Just treat everything weighted, simpler downstream
  if(is.null(weight)) weight <- rep(1, length(x))

  g1 <- group == levels(group)[1]
  g2 <- group == levels(group)[2]

  # Validate weights
  if(length(x) != length(weight)) stop("smd() call x and weight lengths differ")

  # Is it possible to compute anything?
  if(length(x) < 4 || length(x[g1]) < 2 || length(x[g2]) < 2) return(NA)

  # Dispatch to relevant
  result <-
    if(inherits(x,"factor"))   smd_categorical(x, group, weight)                  else
    if(length(unique(x)) == 2) smd_categorical(as.factor(x==x[1]), group, weight) else
                               smd_continuous(x, g1, g2, weight)

  # Compute some 95% CI
  sigmad <- sqrt((n1+n2)/(n1*n2) + result^2/2/(n1+n2)) # Hedges and Olkin 1995

  attr(result, "hedges")  <- c(n1=n1, n2=n2,
                               l95=result - qnorm(0.975)*sigmad,
                               u95=result + qnorm(0.975)*sigmad)
  result
}
