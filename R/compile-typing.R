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

#' Determine if a vector is categorical or not
#'
#' @param x Vector to determine type of
#' @param threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return A Boolean: TRUE / FALSE
#' @export
#'
#' @examples
#'
#' is.categorical(c(1,2,3))
#' is.categorical(c(rep(1,20), rep(2, 20), rep(3, 20)), threshold=5)
#' is.categorical(c("A","B","B"))
#' is.categorical(factor(c("A","B","C")))
#' is.categorical(factor(c("A","B","B","A")))
#' is.categorical(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
is.categorical <- function(x, threshold=NA)
{
  is.factor(x)    ||
  is.character(x) ||
  is.logical(x)   ||
  (!is.na(threshold) && length(unique(x[! is.na(x)])) < threshold)
}

#' Determine if a vector is binomial or not
#'
#' @param x Vector to determine type of
#' @param threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return a Boolean: TRUE / FALSE
#' @export
#'
#' @examples
#'
#' is.binomial(c(1,2,3))
#' is.binomial(factor(c("A","B","C")))
#' is.binomial(factor(c("A","B","B","A")))
#' is.binomial(factor(c(TRUE, FALSE, TRUE, FALSE)))
#' is.binomial(c('M', 'F', 'M', 'F'), 10)
is.binomial <- function(x, threshold=NA)
{
  (is.factor(x) && length(levels(x)) == 2) ||
  (!is.na(threshold) && length(unique(x[! is.na(x)])) == 2)
}

#' Convert data type to a factor if it's not already
#'
#' @param x Data to convert to factor
#'
#' @return Data as a factor
#' @export
#'
#' @examples
#'
#' as.categorical(1:3)
#'
as.categorical <- function(x)
{
  if(!inherits(x, "factor"))
  {
    stuff  <- attributes(x)
    x <- factor(x, levels=if(inherits(x, "logical")) c("FALSE", "TRUE") else sort(unique(x[!is.na(x)])))
    attr(x, "label") <- stuff[["label"]]
    attr(x, "units") <- stuff[["units"]]
  }
  x
}
