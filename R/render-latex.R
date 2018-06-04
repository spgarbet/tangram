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

latexreference <- function(object)
{
  if(is.null(attr(object,"reference"))) "" else
    paste0("\\textsuperscript{", htmlEscape(attr(object, "reference")), "}")
}


#' Render to LaTeX methods for tangram cell objects
#'
#' Each of these methods will render the cell object as a LaTeX fragment
#'
#' @param object object; the item to render to latex
#' @param caption character; Caption to display on table
#' @param footnote character; Footnote to include on table
#' @param fragment logical; Is this a complete LaTeX document or just the table fragment
#' @param filename character; filename to write LaTex into
#' @param append logical; Should the write be an append operation or overwrite
#' @param na.blank logical; Should NA's be displayed as blanks
#' @param cgroup.just character; The text of the column justification used in the table
#' @param arraystretch numeric; The arraystretch parameter used for vertical spacing
#' @param style character; can be null or "nejm" for different table styling
#' @param pct_width numeric; a scaling to be applied to the entire table
#' @param placement character; placement directive, defaults to "H"
#' @param ... additional arguments
#' @return the LaTeX rendering
#' @include compile-cell.R
#' @include render-latex-map.R
#'
#' @examples
#' \dontrun{
#' latex(cell_label("123"))
#' latex(cell_iqr(rnorm(20)))
#' latex(cell_estimate(2.1,0.8, 3.3))
#' latex(cell_fraction(45, 137))
#' tbl <- tangram(drug~bili, pbc)
#' latex(tbl)
#' }
#' @rdname latex
#' @export
latex <- function(object, ...)
{
  UseMethod("latex", object)
}

#' @rdname latex
#' @export
latex.default <- function(object, ...)
{
  warning(paste("summary unhandled class : ", paste(base::class(object), collapse=', ')))

  latexify(paste0(as.character(object), collapse=" "))
}

#' @rdname latex
#' @export
latex.cell <- function(object, ...)
{
  sep  <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")

  if(is.null(names(object)))
  {
    latexify(paste(object, collapse=sep))
  } else {
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    latexify(paste(paste0(name, as.character(object)), collapse=sep))
  }
}

#' @rdname latex
#' @export
latex.cell_label <- function(object, ...)
{
  # Turn "*" for interaction terms into a break
  label <- gsub("\\\\\\*", "$\\times$\n\n\u00a0\u00a0", object)

  # Turn leading spaces into a set of non breaking html space
  label <- gsub("^\\s+", "\u00a0\u00a0\u00a0\u00a0", label)

  label <- latexify(label)

  if(is.null(attr(object, 'units')))
    label
  else
    paste0(label, " {\\textit{\\scriptsize ", latexify(attr(object, 'units')), "}}", latexreference(object))
}

latex.logical <- function(object, na.blank=TRUE, ...)
{
  if(is.na(object))
  {
    if(na.blank) "" else "NA"
  } else {
    as.character(object)
  }
}

#' @rdname latex
#' @export
latex.cell_n <- function(object, ...)
{
  #idx <- index(object, id)
  latexify(object)
}

#' @rdname latex
#' @export
latex.cell_header <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  if(inherits(object, "cell_n"))
    paste0("\\textbf{N=", latex.cell_n(object, ...), "}")
  else # Peel down to cell_label
    paste0("\\textbf{",  latex(object, ...), "}", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_subheader <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  if(inherits(object, "cell_n"))
    paste0("{\\scriptsize $N=", latex.cell_n(object, ...), "$}")
  else # Peel down to cell_label
    paste0("{\\scriptsize ",   latex(object, ...), "}", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_iqr <- function(object,...)
{
  mid        <- floor(length(object)/2) + 1
  ltx        <- sapply(object, latexify)
  results    <- paste0(
    "{\\scriptsize ",
    ltx[1:(mid-1)],
    "}~\\textbf{",
    ltx[mid],"}~{\\scriptsize ",
    ltx[(mid+1):length(ltx)], "}",
    latexreference(object),
    collapse=' '
  )

  z <- attr(object, "msd")

  if(is.null(z)) results else paste0(results, ' ', z[1], '$\\pm$', z[2])
}

#' @rdname latex
#' @export
latex.cell_estimate <- function(object,...)
{
  paste0("(", latex(object[1]), ",~", latex(object[2]), ")", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_fstat <- function(object,style="",...)
{
  if(style=="nejm")
    paste0("$",object[4],"$", latexreference(object))
  else
    paste0("$F_{",object[2],",",object[3],"}=",object[1],",~P=",object[4],"$", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_fraction <- function(object,style="",...)
{
  den <- object["denominator"]
  num <- object["numerator"]

  if(style=="nejm")
    paste0(num, " (", object["percentage"], "\\%)", latexreference(object))
  else
    paste0(object["ratio"], "~$\\frac{", num, "}{", den, "}$", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_chi2 <- function(object,style="",...)
{
  if(style=="nejm")
    paste0(object[3], latexreference(object))
  else
    paste0("$\\chi^2_{", object[2], "}=", object[1], ",~P=",object[3],"$", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_studentt <- function(object,...)
{
  paste0("$T_{", object[2], "}=", object[1], ",~P=",object[3],"$", latexreference(object))
}

#' @rdname latex
#' @export
latex.cell_spearman <- function(object,...)
{
  paste0("$S=", object[1], ",~P=",object[3],"$", latexreference(object))
}

#' @rdname latex
#' @export
latex.tangram <- function(object,
                          caption="Table",
                          footnote=NULL,
                          fragment=TRUE,
                          filename=NULL,
                          append=FALSE,
                          na.blank=TRUE,
                          cgroup.just=NULL,
                          arraystretch=1.2,
                          pct_width=1.0,
                          placement="H",
                          style="",
                          ...)
{
  if(is.null(footnote) && !is.null(attr(object, "footnote"))) footnote <- attr(object, "footnote")
  footnote <- if(is.null(footnote)) "" else paste0(latexify(paste0(footnote, collapse="\n\n")), "\n")

  result <- ""
  if(!fragment) result <- paste(result,
                                "\\documentclass{report}",
                                "\\usepackage{geometry}",
                                "\\begin{document}",
                                sep="\n")
  if(style=="nejm") result <- if(style=="nejm") paste0(result,
                                                      "\\definecolor{nejm-yellow}{RGB}{255,251,237}\n",
                                                      "\\definecolor{nejm-header}{RGB}{247,244,239}\n")
  result <- paste0(result, "\\begin{table}[",placement,"]\n\\centering\n")

  if(style=="nejm") result <- paste0(result, "{\\fontfamily{cmss}\\selectfont\n")


  if(pct_width != 1.0) result <- paste0(result, "\\scalebox{", pct_width, "}{\n")
#    stylehdr <- paste(stylehdr,
#                      paste0("\\bigskip\\begin{minipage}{",pct_width,"\\linewidth}\\centering"),
#                      paste0("\\resizebox{\\columnwidth}{!}{ \\renewcommand{\\arraystretch}{", arraystretch, "}"),
#                      sep='\n')

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  last_header_col <- 0
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header"))
      {
        last_header_row <<- row - 1
        last_header_col <<- col - 1
      }
      text[row,col] <<- latex(object[[row]][[col]], na.blank, style=style, ...)
    })
  })

  pasty <- apply(text, 1, function(x) paste0(paste(x, collapse=" & "), "\\\\\n"))

  if(is.null(cgroup.just)) {
    cgroup.just <- paste0(c(rep("l", last_header_col), rep("c", ncols-last_header_col)),collapse="")
    if(style=="nejm") cgroup.just <- paste0("|", cgroup.just, "|")
  }

  if(style=="nejm") result <- paste0(result, "\\rowcolors{2}{nejm-yellow}{white}\n")
  result <- paste0(result, "\\begin{tabular}{",cgroup.just,"}\n")
  result <- if(style=="nejm"){
              paste0(result, "\\hline\n\\rowcolor{nejm-header}\\multicolumn{",ncols,"}{|l|}{",latexify(caption),"} \\\\\n\\hline\n")
            } else {
              paste0(result, "\\hline\\hline\n")
            }

  if(last_header_row > 0) result <- paste0(result, paste0(as.vector(pasty)[1:last_header_row], collapse=''))
  if(style!="nejm") result <- paste0(result, "\\hline\n")

  result <- paste0(result, paste0(as.vector(pasty)[(last_header_row+1):nrows], collapse=''))

  result <- paste0(result, if(style=="nejm") "\\hline\n" else "\\hline\\hline\n")

  result <- paste0(result, "\\end{tabular}\n")

  if(pct_width != 1.0) result <- paste0(result, "}\n")

  if(style=="nejm") result <- paste0(result, "}\n")

  result <- paste0(result, "\\caption{",latexify(caption),"}\n")

  if(nchar(footnote) > 0) result <- paste0(result, "\n\n\\vspace{0.2cm}\n\\raggedright{\\begin{footnotesize}",footnote,"\\end{footnotesize}}\n")

  result <- paste0(result, "\\end{table}\n")

  if(!fragment) result <- paste0(result, "\\end{document}\n")

  if(!is.null(filename)) cat(result, file=filename, append=append)

  cat(result)
  invisible(result)
}
