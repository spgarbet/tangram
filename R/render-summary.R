# tangram a general purpose table toolkit for R
# Copyright (C) 2017 Shawn Garbett
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

textsub_table <-list(
  c("\u00A0",                                "  "),       # Special spaces
  c("(^|[^\\\\])~((\\\\.|[^~\\\\])*)~",      "\\1_\\2"),  # Subscript
  c("(^|[^\\\\])\\^((\\\\.|[^~\\\\])*)\\^",  "\\1^\\2"),  # Superscript
  c("\\\\(.)",                               "\\1")       # convert escaped characters
)

#' @importFrom stringi stri_trans_nfc
#' @importFrom stringi stri_trans_nfd
#' @importFrom utils capture.output
textify <- function(x)
{
  y <- as.character(x)          # Make sure a character string was passed
  if(nchar(y) == 0) return("")  # Abort early for zero characters

  ## Kludge for converting from "byte" to the current encoding
  ## in a way which preserves the hex notation.
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) y[encBytes] <- capture.output(cat(y[encBytes], sep = "\n"))

  ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
  ## processing of accented characters. Doing this early to
  ## circumvent pecularities in gsub() (and nchar()) when working in
  ## the C locale.
  y <- stri_trans_nfd(y)

  # Convert strikethrough
  y <- "~~one~~ other \\~~ ~~two~~"
  pieces <- strsplit(y, "(?<!\\\\)~~", perl=TRUE)[[1]] # Strikethrough
  if(length(pieces) > 1)
  {
    subs <- seq(2, length(pieces), by=2)
    pieces[subs] <- sapply(pieces[subs], function(x) {
      gsub("(\\\\.|.)", "\\1\u0336", x, perl=TRUE)
    })
  }
  y <- paste0(pieces, collapse="")

  ## Run all conversions as appropriate not inside "$"
  pieces  <- strsplit(y, "(?<!\\\\)\\$", perl=TRUE)[[1]]
  if(length(pieces) > 0)
  {
    subs <- seq(1, length(pieces), by=2)
    pieces[subs] <- sapply(pieces[subs], function(x) {
      Reduce(function(u, v) gsub(v[1], v[2], u, perl=TRUE), textsub_table, x)
    })
  }

  for(i in 1:length(pieces))
  {
    if((i %% 2) != 0)
    {
      for (subst in textsub_table) pieces[i] <- gsub(subst[1], subst[2], pieces[i], perl = TRUE)
    }
  }
  y <- paste0(pieces, collapse="")



  ## Convert result to UTF-8 NFC encoding, although most non-ASCII
  ## content has probably been converted to LaTeX commands.
  stri_trans_nfc(y)
}



#######
#' The default method for rendering tangram objects
#' A tangram is a summary, so it returns itself. Otherwise convert to a text representation.
#'
#' @param object object; the item to render
#' @param ... additional arguments passed to summary
#' @return the text summary
#' @examples
#' summary(cell_label("123"))
#' summary(cell_iqr(rnorm(20)))
#' summary(cell_estimate(2.1,0.8, 3.3))
#' summary(cell_fraction(45, 137))
#' summary(table_builder()   %>%
#'         row_header("row") %>%
#'         col_header(1,2,3) %>%
#'         add_col("A","B","C"))
#' summary(tangram(drug~bili, pbc))
#' @include compile-cell.R
#' @include compile.R
#' @rdname summary
#' @export
summary.tangram <- function(object, ...) object

#' @rdname summary
#' @export
summary.table_builder <- function(object,...) summary(table_flatten(object$table))

#' @rdname summary
#' @export
summary.cell <- function(object, ...)
{
  sep  <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")

  if(is.null(names(object)))
  {
    paste(object, collapse=sep)
  } else {
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    paste(paste0(name, as.character(object)), collapse=sep)
  }
}

#' @rdname summary
#' @export
summary.cell_label <- function(object, ...)
{
  units <- attr(object, "units")
  if(is.null(units)) object else paste0(object, " (", units, ")")
}

#' @rdname summary
#' @export
summary.cell_spearman <- function(object, ...)
{
  paste0("S=",object[1],", ","P=",object[3])
}

#' @rdname summary
#' @export
summary.cell_iqr <- function(object, ...)
{
  mid <- floor(length(object)/2) + 1
  y <- as.character(object)
  x <- y[mid] <- paste0("*", y[mid], "*")

  result <- paste0(y, collapse=' ')

  z <- attr(object, "msd")
  if(is.null(z)) result else
  {
    paste0(result, ' ', z[1], '\u00b1', z[2])
  }
}

#' @rdname summary
#' @export
summary.cell_range <- function(object, ...)
{
  sep <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")
  paste0("(", object[1], sep, object[2], ")")
}

#' @rdname summary
#' @export
summary.cell_estimate <- function(object,...)
{
  paste0(c(summary(object[[1]]), summary(object[[2]])), collapse=' ')
}

#' @rdname summary
#' @export
summary.cell_fraction <- function(object,...)
{
  den <- as.character(object['denominator'])
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object['numerator'])
  paste0(object['ratio'], "  ", num, "/", den)
}

#' @rdname summary
#' @export
summary.cell_fstat <- function(object, ...)
{
  paste0("F_{", object[2], ",", object[3], "}=", object[1], ", P=", object[4])
}

#' @rdname summary
#' @export
summary.cell_chi2 <- function(object, ...)
{
  paste0("X^2_", object[2], "=", object[1], ", P=", object[3])
}

#' @rdname summary
#' @export
summary.cell_studentt <- function(object, ...)
{
  paste0("t_", object[2], "=", object[1], ", P=", object[3])
}

#######
#' Print methods for tangram objects
#'
#' @param x object; the item to render
#' @param ... additional arguments passed to summary
#' @return the text summary
#' @examples
#' print(cell_label("123"))
#' print(cell_iqr(rnorm(20)))
#' print(cell_estimate(2.1,0.8, 3.3))
#' print(cell_fraction(45, 137))
#' print(table_builder()   %>%
#'         row_header("row") %>%
#'         col_header(1,2,3) %>%
#'         add_col("A","B","C"))
#' print(tangram(drug~bili, pbc))
#' @rdname print
#' @export
print.cell <- function(x, ...)
{
  cat(summary(x, ...))
}

#' @rdname print
#' @export
print.tangram <- function(x,...)
{
  nrows <- rows(x)
  ncols <- cols(x)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(x[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- summary(x[[row]][[col]])
    })
  })

  maxwidths <- apply(text, 2, FUN=function(x) max(nchar(x), na.rm=TRUE))

  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(col == 1)
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "right")
      }
      else
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "both")
      }
    })
  })

  pasty <- apply(text, 1, function(x) paste(x, collapse="  "))

  result <- paste0(paste0(rep("=",nchar(pasty[1])), collapse=''), '\n')

  for(row in pasty)
  {
    result <- paste0(result, row, '\n')
    if(last_header_row > 0              &&
       length(pasty) >= last_header_row &&
       row == pasty[last_header_row])
    {
      result <- paste0(result, paste0(rep("-",nchar(pasty[1])),collapse=''),'\n')
    }
  }
  result <- paste0(result, paste0(rep("=",nchar(pasty[1])),collapse=''), '\n')

  cat(result)
  invisible(result)
}

#' Print a text summary of a given table_builder
#'
#' @param x The table_builder to render to text
#' @param ... additional arguments, unused at present
#' @return A text string rendering of the given table
#' @export
print.table_builder <- function(x,...) print(summary(x$table,...))

### Notes on making text rendering of histograms
# map <- c(" ", "\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2587")
# h <- hist(rexp(200))
# paste0(map[floor(h$density*8/max(h$density))+1], collapse='')
