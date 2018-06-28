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
  c("\u00A0",                                  "  "),        # Special spaces
  c("\\*\\*",                                 "\\*"),        # Double asterisks converted to single
  c("(^|[^\\\\])~([^~\\\\])~",                "\\1_\\2"),    # Single character subscript
  c("(^|[^\\\\])~((\\\\.|[^~\\\\])+)~",       "\\1_{\\2}"),  # Subscript
  c("(^|[^\\\\])\\^((\\\\.|[^\\^\\\\])+)\\^", "\\1^\\2 "),   # Superscript
  c("(^|[^\\\\])\\^((\\\\.|[^\\^\\\\])+) _",  "\\1^\\2_"),   # Superscript followed by subscript correction
  c("(^|[^\\\\])\\\\frac{([^}]*)}{([^}]*)}",  "\\1 \\2/\\3"),# Make fractions
  c("\\\\(.)",                                "\\1")         # convert escaped characters
)

#' @include iify.R
textify <- Vectorize(function(x)
{
  x <- iify(x, textsub_table)

  ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
  ## processing of accented characters. Doing this early to
  ## circumvent pecularities in gsub() (and nchar()) when working in
  ## the C locale.
  y <- stri_trans_nfd(x)

  # Convert strikethrough
  #y <- "~~one~~ other \\~~ ~~two~~"
  pieces <- strsplit(y, "(?<!\\\\)~~", perl=TRUE)[[1]] # Strikethrough
  if(length(pieces) > 1)
  {
    subs <- seq(2, length(pieces), by=2)
    pieces[subs] <- sapply(pieces[subs], function(x) {
      gsub("(\\\\.|.)", "\\1\u0336", x, perl=TRUE)
    })
  }

  y <- paste0(pieces, collapse="")

  ## Convert result to UTF-8 NFC encoding, although most non-ASCII
  ## content has probably been converted to LaTeX commands.
  stri_trans_nfc(y)
})


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
summary.tangram <- function(object, ...) internal_print(object, ...)

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
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(textify(n),"=") else "", "character")
    paste(paste0(name, as.character(object)), collapse=sep)
  }
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
print.tangram <- function(x, ...)
{
  renderer <- render_route_tangram()
  renderer(x, ...)
}

#' Router for rendering method
#'
#' This functions detects if knitr is loaded, and does it's best to determine the output
#' format from knitr and returns the appropriate rendering function.
#'
#' @importFrom knitr is_html_output
#' @importFrom knitr is_latex_output
#' @importFrom knitr opts_knit
#' @return A rendering function to use
render_route_tangram <- function()
{
  if(! "knitr" %in% .packages()) return(internal_print)

  if(knitr::is_html_output()) return(html5.tangram)

  if(knitr::is_latex_output()) return(latex.tangram)

  if(is.null(knitr::opts_knit$get("out.format"))) return(internal_print)

  rmd.tangram
}

#' @importFrom stringr str_pad
internal_print <- function(x, ...)
{
  nrows <- rows(x)
  ncols <- cols(x)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(x[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- textify(summary(x[[row]][[col]]))
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

  if(!is.null(attr(x, "caption")))
  {
    result <- paste0(textify(attr(x, "caption")), '\n', result)
  }


  if(!is.null(attr(x, "footnote")))
  {
    result <- paste0(result, textify(paste0(attr(x, "footnote"), collapse="\n")), collapse='\n' )
  }

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


