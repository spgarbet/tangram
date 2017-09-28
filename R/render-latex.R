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
#' @param ... additional arguments
#' @return the LaTeX rendering
#' @include compile-cell.R
#' @include render-latex-map.R
#'
#' @examples
#' latex(cell_label("123"))
#' latex(cell_iqr(rnorm(20)))
#' latex(cell_estimate(2.1,0.8, 3.3))
#' latex(cell_fraction(45, 137))
#' latex(table_builder()   %>%
#'         row_header("row") %>%
#'         col_header(1,2,3) %>%
#'         add_col("A","B","C"))
#' latex(tangram(drug~bili, pbc))
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
    paste0(label, " {\\textit{\\scriptsize ", latexify(attr(object, 'units')), "}}")
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
    paste0("\\textbf{",  latex(object, ...), "}")
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
    paste0("{\\scriptsize ",   latex(object, ...), "}")
}

#' @rdname latex
#' @export
latex.cell_iqr <- function(object,...)
{
  paste0("{\\scriptsize ", latexify(object[1]), "}~\\textbf{",
         latexify(object[2]),
         "}~{\\scriptsize ", latexify(object[3]), "}")
}

#' @rdname latex
#' @export
latex.cell_estimate <- function(object,...)
{
  paste0("(", latex(object[1]), ",~", latex(object[2]), ")")
}

#' @rdname latex
#' @export
latex.cell_fstat <- function(object,...)
{
  paste0("$F_{",object[2],",",object[3],"}=",object[1],",~P=",object[4],"$")
}

#' @rdname latex
#' @export
latex.cell_fraction <- function(object,...)
{
  den <- object["denominator"]
  num <- object["numerator"]
  paste0(object["ratio"], "~$\\frac{", num, "}{", den, "}$")
}

#' @rdname latex
#' @export
latex.cell_chi2 <- function(object,...)
{
  paste0("$\\chi^2_{", object[2], "}=", object[1], ",~P=",object[3],"$")
}

#' @rdname latex
#' @export
latex.cell_studentt <- function(object,...)
{
  paste0("$T_{", object[2], "}=", object[1], ",~P=",object[3],"$")
}

#' @rdname latex
#' @export
latex.cell_spearman <- function(object,...)
{
  paste0("$S=", object[1], ",~P=",object[3],"$")
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
                          ...)
{
  header <- if(fragment) "" else
              paste0("\\documentclass{report}\n",
                     "\\usepackage{geometry}\n",
                     "\\begin{document}\n"
                     )
  footer <- if(fragment) "" else "\\end{document}"

  if(is.null(footnote) && !is.null(attr(object, "footnote")))
    footnote <- attr(object, "footnote")
  footnote <- if(is.null(footnote)) "" else
    paste0(latexify(paste0(footnote, collapse="\n\n")), "\n")

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
      text[row,col] <<- latex(object[[row]][[col]], na.blank, ...)
    })
  })

  pasty <- apply(text, 1, function(x) paste0(paste(x, collapse=" & "), "\\\\\n"))

  if(is.null(cgroup.just)) cgroup.just <- paste0(c(rep("l", last_header_col), rep("c", ncols-last_header_col)),collapse="")

  tableHdr <- paste("\\bigskip\\begin{minipage}{\\linewidth}\\centering",
                     paste0("\\resizebox{\\columnwidth}{!}{ \\renewcommand{\\arraystretch}{", arraystretch, "} \\begin{tabular}{",cgroup.just,"}"),
                     "\\hline\\hline",
                      if(last_header_row == 0) "" else
                        paste0(paste0(as.vector(pasty)[1:last_header_row], collapse=''), "\\hline "),
                     sep="\n"
                     )

  tableBdy <- paste0(paste0(as.vector(pasty)[(last_header_row+1):nrows], collapse=''),
                    "\\hline\\hline\n",
                    "\\end{tabular} } \\par\\bigskip\n",
                    latexify(caption),
                    "\\end{minipage}\n")

  result <- paste0(header, tableHdr, tableBdy, footnote, footer, sep="\n")

  if(!is.null(filename)) cat(result, file=filename, append=append)

  cat(result)
  invisible(result)
}
