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

# From: https://github.com/rstudio/gt/issues/227
utf8Tortf <- function(x){

  stopifnot(length(x) == 1 & "character" %in% class(x) )

  x_char <- unlist(strsplit(x, ""))
  x_int  <- utf8ToInt(x)
  x_rtf  <- ifelse(x_int <= 255,
                   ifelse(x_int <= 127, x_char, paste0("\\'",as.hexmode(x_int))),
                   ifelse(x_int <= 32768, paste0("\\uc1\\u", x_int,"?"),
                         paste0("\\uc1\\u-", x_int - 65536, "?") ) )

  paste0(x_rtf, collapse = "")
}

rtfsub_table <- list(
  c("\\*\\*([^\\*]+)\\*\\*",  "{\\\\b \\1}"), # Bold
  c("__([^_]+)__",            "{\\\\b \\1}"),
  c("\\*([^\\*]+)\\*",        "{\\\\i \\1}"), # Italic
  c("_([^_]+)_",              "{\\\\i \\1}"),
  c("`([^`]+)`",              "{\\\\f1 \\1}"),  # Inline Code
  c("~~([^~]+)~~",            "{\\\\strike \\1}"),  # Strikethrough
  c("~([^~]+)~",              "{\\\\sub \\1}"),           # Subscript
  c("\u00A0",                 "\\\\~"),             # no-break space (NBSP) must be handled after subscripting
  c("\\^([^\\^]+)\\^",        "{\\\\super \\1}"),

  # And deal with pesky fraction
  c("\\\\frac\u007B(.*)\u007D\u007B(.*)\u007D", " \\1/\\2")

)
rtfify <- function(x) utf8Tortf(iify(x, rtfsub_table))


#' Default conversion to RTF for an abstract table element
#'
#' Gives a warning and produces an empty cell
#'
#' @param object The cell to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return A RTF string rendering of the given cell
#' @export
#'
rtf.default <- function(object, id, ...)
{
  warning(paste("rtf unhandled class : ", base::class(object), collapse=', '))
  ""
}

#' @export
rtf.character <- function(object, id, ...) rtfify(object)

est_column_widths <- function(object)
{
  nrows <- rows(object)
  ncols <- cols(object)

  lens <- sapply(1:ncols, FUN=function(col) {
    max(sapply(1:nrows, FUN=function(row) {
      nchar(summary(object[[row]][[col]]))
    }))
  })

  c(0, pmax(lens/16, rep(0.4, ncols)))
}

#' S3 rtf Method function for use on abstract table class
#'
#' @param object The cell to render to RTF
#' @param id A unique identifier for the table (strongly recommended). If not provided, caption will be used.
#' @param ... additional arguments to renderer. Unused at present.
#' @return A text string rendering of the given table
#' @export
#'
rtf <- function(object, id, ...)
{
  UseMethod("rtf", object)
}

#' Convert a tangram into an RTF string or file
#'
#' Given a tangram class, a series of conversion creates an rtf
#' representation of the table.
#'
#' @param object The cell table to render to RTF
#' @param caption A string caption for the table
#' @param fragment A boolean flag that determines whether a fragment or a complete RTF document is generatedf
#' @param id A unique identifier for the table (strongly recommended).
#' @param widths RTF requires specified left margin and column widths, this allows user control over these (inches)
#' @param footnote Any footnotes to include under the table.
#' @param filename A filename to write resulting rtf file to
#' @param append A boolean for whether or not to append to given filename
#' @param point Main font point size
#' @param ... additional arguments Fto renderer. Unused
#' @return A text string rendering of the given table
#' @importFrom utils tail
#' @export
#'
rtf.tangram <- function(
  object,
  id       = NA,
  caption  = NA,
  fragment = FALSE,
  widths   = NA,
  footnote = NA,
  filename = NA,
  append   = FALSE,
  point    = 9,
  ...)
{
  if(is.na(id)) id <- ""

  # Scale by inverse of conway's constant
  widths <- round((point/9)^(0.7671241)*1440*cumsum(if(is.na(widths)) est_column_widths(object) else widths))

  header <- if(fragment) "" else
    paste0(
      "{\\rtf1 \\ansi \\deff0",
      "{\\fonttbl",
      "  {\\f0\\froman\\fprq2\\fcharset0 Liberation Serif{\\*\\falt Times New Roman};}",
      "  {\\f1 Courier;}",
      "}",
      "\\paperw",min(round(tail(widths,1)*1440)+720, 15840),
      "\\paperh12240",
      "\\margl720\\margr720\\margt1440\\margb1440",
      "\\landscape"
    )

  caption <- if(is.na(caption)) "" else
    paste0("{\\pard\\b\\fs",round(point*2.4)," ",rtfify(caption),"\\par}\n")

  # Construct row open and close text
  celldecl <-
  rowopen <- paste0(
    "\\trowd \\trgaph90",
    "\\trleft", widths[1]
  )
  rowclose <- "\\row\n"

  # Construct RTF Row Openings (see pg84, RTF Pocket Guide)
  firstrowopen <- paste0(rowopen, paste0("\\clbrdrt\\brdrs\\cellx",  tail(widths,-1), collapse=''), '\n')
  lasthdropen  <- paste0(rowopen, paste0("\\clbrdrb\\brdrdb\\cellx", tail(widths,-1), collapse=''), '\n')
  lastrowopen  <- paste0(rowopen, paste0("\\clbrdrb\\brdrs\\cellx",  tail(widths,-1), collapse=''), '\n')
  rowopen      <- paste0(rowopen, paste0("\\cellx", tail(widths,-1), collapse=''), '\n')

  # Construct cell open and close text
  cellopen    <- paste0("\\pard\\qc\\intbl\\fs", round(point*2), " ")
  hdrcellopen <- paste0("\\pard\\ql\\intbl\\fs", round(point*2), " ")
  cellclose   <- "\\cell\n"

  trailer <- if(fragment) "" else "}"

  nrows <- rows(object)
  ncols <- cols(object)
  text  <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row)
  {
    if(last_header_row == 0 && any(sapply(object[[row]], function(x) !inherits(x, "cell_header")))) last_header_row <<- row - 1
    sapply(1:ncols, FUN=function(col)
    {
      #if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col]  <<- paste0(if(inherits(object[[row]][[col]], "cell_header") && last_header_row != 0) hdrcellopen else cellopen,
                                rtf(object[[row]][[col]], id=id, point=point),
                                cellclose)
      if(!is.null(attr(object[[row]][[col]], "colspan"))) warning("colspan not supported for rtf tangram rendering")
      if(!is.null(attr(object[[row]][[col]], "rowspan"))) warning("rowspan not supported for rtf tangram rendering")
    })
  })
  rowtext <- apply(text, 1, function(x) paste(x, collapse=''))

  fullrows <- sapply(1:nrows, function(row) {
    hdropen <- if(row == 1)                { firstrowopen } else
               if(row == last_header_row)  { lasthdropen  } else
               if(row == nrows)            { lastrowopen  } else
                                           { rowopen      }
    paste0(hdropen, rowtext[row], rowclose)
  })
  pasty <-  paste0(fullrows, collapse='')

  result <- paste0(header, caption, pasty, trailer, collapse='')

  if(!is.na(filename)) cat(result, file=filename, append=append)

  invisible(result)
}


