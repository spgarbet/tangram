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

# Turn a passed pformat into a function (or leave alone)
pfunc <- function(pformat)
{
  if(class(pformat) == "function") return(pformat)

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

#' @export
#' @rdname rowscols
rows.table_builder <- function(x) rows(x$table)

#' @export
#' @rdname rowscols
cols.table_builder <- function(x) cols(x$table)

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
#'
cell <- function(x, ...)
{
  UseMethod("cell", x)
}

#' @export
cell.default <- function(x, ...)
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

#' Create a interquartile range cell object of the given data
#'
#' Construct a cell which has the 3 interquartile ranges specified.
#'
#' @param x numeric vector whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute. Set to FALSE for speedup with many probs.
#' @param type integer; specify algorithm to use in constructing quantile. See quantile for more information.
#' @param msd logical; compute an msd attribute containing mean and standard deviation
#' @param quant numeric; The quantiles to display. Should be an odd length vector, since the center value is highlighted.
#' @param ... additional arguments to constructing cell
#'
#' @return A cell_quantile object.
#' @export
#' @importFrom stats quantile
#' @importFrom stats sd
#' @examples
#' require(stats)
#' cell_iqr(rnorm(100), '3')
cell_iqr <- function(x,
                     format = NA,
                     na.rm  = TRUE,
                     names  = FALSE,
                     type   = 8,
                     msd    = FALSE,
                     quant  = c(0.25, 0.50, 0.75),
                     ...
                          )
{
  if(length(quant) %% 2 == 0) stop("cell_iqr quant argument must be an odd length")

  y <- quantile(x, quant, na.rm, names, type)

  if(is.na(format)) format <- format_guess(y)
  ql <- sapply(y, function(x) render_f(x, format))
  if(msd) attr(ql, "msd") <- c(render_f(mean(x, na.rm=TRUE), format),
                               render_f(sd(x, na.rm=TRUE), format))
  cell(ql, class="cell_iqr", ...)
}

#' Create named value cells
#'
#' A cell object with
#' additionally contain an interval with a low and high of a
#' specified width.
#'
#' @param values vector; to create cell values from
#' @param names character; names to apply to values
#' @param sep character; separator to use when rendering
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... additional attributes to attach to cell
#' @return A cell object with named values
#' @export
#' @examples
#' cell_named_values(1.0, "one")
cell_named_values <- function(values, names, class=NULL, sep=", ", ...)
{
  names(values) <- names
  cell(values, class=c(class, "cell_value"), sep=sep, ...)
}

#' Create a cell representing a range
#'
#' Useful for things such as confidence intervals.
#'
#' @param low character or numeric; lower value of range
#' @param high character or numeric; upper value of range
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param sep character; separator to use when rendering
#' @param ... additional attributes to attach to cell
#'
#' @return A cell object denoting a range
#' @export
#' @examples
#' cell_range(-1.0, 1.0)
cell_range <- function(low, high, class=NULL, sep=", ", ...)
{
  cell(c(low, high), class=c(class, "cell_range"), sep=sep, ...)
}

#' Create a cell_estimate object of the given estimate
#'
#' A cell_estimate object contains a statistical estimate. It may
#' additionally contain an interval with a low and high of a
#' specified width.
#'
#' @param value The value of the estimate
#' @param low Specifies a lower interval for the estimate.
#' @param high Specifies an upper interval for the estimate.
#' @param name character; An optional name to apply to the value
#' @param class character; additional classs to apply
#' @param sep character; option separator character for the range
#' @param ... optional extra information to attach
#'
#' @return A cell_estimate object.
#' @export
#' @examples
#' cell_estimate(1.0, 0.5, 1.5)
#' cell_estimate(1.0, 0.5, 1.5, name="one")
cell_estimate <- function(value, low, high, name=NULL, class=NULL, sep=", ", ...)
{
  cell(list(cell_named_values(value, names=name),
            cell_range(low, high, sep=sep)),
       class=c(class, "cell_estimate"),
       ...)
}


#' Create an cell_fraction (S3) object of the given statistic
#'
#' A cell_fraction object contains a statistical result of a fraction/percentage.
#'
#' @param numerator numeric; The value of the numerator
#' @param denominator numeric; The value of the denominator
#' @param format numeric or character; a string formatting directive
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_fraction object.
#' @export
#' @examples
#' cell_fraction(1, 4, 0.25, 25)
cell_fraction <- function(numerator, denominator, format=3, class=NULL, ...)
{
  ratio      <- render_f(numerator / denominator, format)
  percentage <- render_f(100 * numerator / denominator, format)
  cell_named_values(c(numerator, denominator, ratio, percentage),
                    c("numerator", "denominator", "ratio", "percentage"),
                    class=c(class, "cell_fraction"),
                    ...)
}

#' Create an cell_fstat (S3) object of the given statistic
#'
#' A cell_fstat object contains a statistical result of an F-test.
#'
#' @param f The value of the f-statistic
#' @param df1 1st dimension degrees of freedom
#' @param df2 2nd dimension degrees of freedom
#' @param p The p-value of the resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_fstat object.
#' @export
#' @examples
#' cell_fstat(4.0, 10, 20, 0.004039541, reference=1,)
cell_fstat <- function(f, df1, df2, p, class=NULL, ...)
{
  cell_named_values(c(f, df1, df2, p), names=c("F", "df1", "df2", "P"), class=c(class, "cell_fstat", "statistics"), ...)
}

#' Create an cell_chi2 (S3) object of the given statistic
#'
#' A cell_chi2 object contains a statistical result of an X^2-test.
#'
#' @param chi2 The value of the X^2 statistic
#' @param df degrees of freedom
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_chi2 object.
#' @export
#' @examples
#' cell_chi2(5.6, 2, 0.06081)
cell_chi2 <- function(chi2, df, p, class=NULL, ...)
{
  cell_named_values(c(chi2, df, p),
                    c("\U03C7^{2}", "df", "P"),
                    class=c(class, "cell_chi2", "statistics"),
                    ...)
}

#' Create an cell_studentt (S3) object of the given statistic
#'
#' A cell_studentt object contains a statistical result of an t-test.
#'
#' @param t The value of the X^2 statistic
#' @param df degrees of freedom
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_studentt object.
#' @export
#' @examples
#' cell_studentt(2.0, 20, 0.02963277)
cell_studentt <- function(t, df, p, class=NULL, ...)
{
  cell_named_values(c(t, df, p),
                    c("t", "df", "P"),
                    class=c(class, "cell_studentt", "statistics"),
                    ...)
}

#' Create an cell_spearman (S3) object of the given statistic
#'
#' A cell_spearman object contains a statistical result of an spearman-test.
#'
#' @param S The value of the spearman statistic
#' @param rho The rho value of the test
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_spearman object.
#' @export
#' @examples
#' cell_spearman(20, 0.2, 0.05)
cell_spearman <- function(S, rho, p, class=NULL, ...)
{
  cell_named_values(c(S, rho, p),
                    names=c("S", "\U03A1", "P"),
                    class=c(class, "cell_spearman", "statistics"),
                    ...)
}

#' Create an cell_n (S3) object of the given statistic
#'
#' A cell_n object contains a statistical result of an X^2-test.
#'
#' @param n The value of the X^2 statistic
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_n object.
#' @export
#' @examples
#' cell_n(20)
cell_n <- function(n, class=NULL, ...)
{
  cell_named_values(n, "N", class=c(class, "cell_n"), ...)
}

#' AOV model as cell
#'
#' Construct a cell from an analysis of variance model
#'
#' @param x The aov object to turn into a renderable cell
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is an F-statistic
#' @export
#' @examples
#' cell(aov(x~y,data.frame(x=rnorm(10), y=rnorm(10))))
cell.aov <- function(x, pformat="%1.3f", ...)
{
  pformat <- pfunc(pformat)
  test <- summary(x)[[1]]
  cell_fstat(f   = render_f(test$'F value'[1], "%.2f"),
             df1 = test$Df[1],
             df2 = test$Df[2],
             p   = pformat(test$'Pr(>F)'[1]),
             ...)
}

#' Construct hypothesis test cell
#'
#' Construct a cell from a hypothesis test
#'
#' Currently handles cor.test, t.test and chisq.test objects
#'
#' @param x The htest object to convert to a rendereable cell
#' @param format numeric or character; A formatting directive applied to statistics
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @examples
#' cell(cor.test(rnorm(10), rnorm(10), method="spearman"))
#' cell(cor.test(rnorm(10), rnorm(10)))
#' cell(chisq.test(rpois(10,1)))
#' cell(t.test(rnorm(10), rnorm(10)))
cell.htest <- function(x, format=2, pformat="%1.3f", ...)
{
  pformat <- pfunc(pformat)

  #reference <- if(is.null(reference)) "" else paste0("^^",reference, "^^")
  if(names(x$statistic) == "X-squared")
    cell_chi2(render_f(x$statistic, format), x$parameter[1], pformat(x$p.value), ...)
  else if(x$method == "Spearman's rank correlation rho")
    cell_spearman(as.character(x$statistic), render_f(x$estimate,format), pformat(x$p.value), ...)
  else if(names(x$statistic) == "V") # wilcox.test
    cell(paste0("V=", x$statistic, ", P=", pformat(x$p.value)),
         class="statistics", ...)
  else
    cell_studentt(render_f(x$statistic, format), render_f(x$parameter[1],format), pformat(x$p.value), ...)
}

