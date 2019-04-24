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

# Turn a passed pformat into a function (or leave alone)
pfunc <- function(pformat)
{
  if(inherits(pformat,"function")) return(pformat)

  if(is.null(pformat)) pformat <- "%1.3f"

  function(p)
  {
    if(is.na(p) || is.nan(p) || p <0 || p>1) return("NA")

    y <- render_f(p, pformat)

    # Check for all zeros once formated
    test <- grep("[^0\\.]+", y)
    if(length(test) > 0) return(y) # It's good!

    # Otherwise, append less than
    paste0("<", substr(y, 1, nchar(y)-1), "1")
  }
}


#' S3 object to return number of rows/cols in object
#'
#' Number of rows/cols in provided object
#'
#' @param x object; object to determine requested count
#' @rdname rowscols
#'
#' @export
rows <- function(x)
{
  UseMethod("rows", x)
}

#' @rdname rowscols
#' @export
cols <- function(x)
{
  UseMethod("cols", x)
}

#' @export
#' @rdname rowscols
rows.list <- function(x)
{
  length(x)
}

#' @export
#' @rdname rowscols
cols.list <- function(x)
{
  if(length(x) >= 1)
  {
    length(x[[1]])
  }
  else
  {
    0
  }
}

#' Construct a table cell from an object
#'
#' Any R object can be used as a cell value. Attributes
#' are used to store additional classs of that cell
#' attached to the object. This is a helper function
#' to attach all the additional attributes to the
#' provided object
#'
#' Certain attributes have special meaning:
#' - 'names' is appended to the front of a value, e.g. "P=" for a p-value.
#' - 'sep' is used to join values, e.g. ", " for a list of values.
#' - 'class' denotes special rendering handling, e.g. generally passed as CSS class to HTML5
#' - 'reference' a list of reference symbols to put inside the cell
#' - 'row' and 'col' should refer to the row / column if key generation is needed
#' - 'subrow' and 'subcol' further delinate the key value of a cell for key generation
#'
#' @param x R object to attach attributes too
#' @param ... Each additional argument becomes an attribute for the object
#' @return The modified R object
#' @export
cell <- function(x, ...)
{
  attribs     <- list(...)
  add_class   <- if("class" %in% names(attribs)) attribs[['class']] else NULL
  final_class <- if(inherits(x, "cell")) c(add_class, class(x)) else c(add_class, "cell", class(x))
  for(i in names(attribs)) attr(x, i) <- attribs[[i]]
  class(x) <- unique(final_class)
  x
}


#' Create an cell_label (S3) object of the given text.
#'
#' A cell_label object represents a label cell inside a table. It can
#' also contain units.
#'
#' @param text character; The text of the label. May include a subset of LaTeX greek or math.
#' @param units character; An optional field that contains units
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#'
#' @return A tangram object
#' @export
#'
#' @examples
#' cell_label("Compaction Method")
#' cell_label("Concentration", "mg/dl")
#' cell_label("Concentration", "mg/dl", subcol="A")
cell_label <- function(text, units=NULL, class=NULL, ...)
{
  cell(text,
       class=c(class, "cell_label"),
       units=units, ...)
}

#' Create a cell_header object of the given text.
#'
#' A cell_header object represents a label cell inside a table. It can
#' also contain units.
#'
#' @param text character; The text of the label. May include a subset of LaTeX greek or math.
#' @param units character; An optional field that contains units
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#'
#' @return A cell_header object
#' @export
#' @examples
#' cell_header("Yahoo")
#' cell_header("Concentration", "mg/dl")
#' cell_header("Concentration", "mg/dl", src="A")
cell_header <- function(text, units=NULL, class=NULL, ...)
{
  if(is.null(units)) units <- attr(text, "units")
  cell(text,
       class=c(class, "cell_header", "cell_label"),
       units=units,
       ...)
}

#' Create a cell_subheader object of the given text.
#'
#' A cell_subheader object represents a label cell inside a table. It can
#' also contain units.
#'
#' @param text character; The text of the label. May include a subset of LaTeX greek or math.
#' @param units character; An optional field that contains units
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#'
#' @return A cell_subheader object.
#' @export
#' @examples
#' cell_subheader("Concentration")
#' cell_subheader("Concentration", "mg/dl")
#' cell_subheader("Concentration", "mg/dl", src="A")
cell_subheader <- function(text, units=NULL, class=NULL, ...)
{
  if(is.null(units)) units <- attr(text, "units")
  cell(text,
       class=c(class, "cell_subheader", "cell_header", "cell_label"),
       units=units,
       ...)
}

#' Create an cell_n (S3) object of the given statistic
#'
#' A cell_n object contains an n value. Essentially, this is just a helper that appends the cell_n class to the given
#' object and makes sure it's a cell S3 object as well.
#'
#' @param n The numerical value
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param hdr logical; Construct an n value for a header (defaults to FALSE)
#' @param possible numerical; The total N that was possible
#' @param ... optional extra information to attach
#' @return A cell_n object.
#' @export
#' @examples
#' cell_n(20)
cell_n <- function(n, class=NULL, hdr=FALSE, possible=NULL, ...)
{
  n <- if(hdr) paste0("(N=",n,")") else as.character(n)
  cell(n, class=c("cell_n", class), ...)
}
