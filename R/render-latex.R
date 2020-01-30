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
#' There are addition arguments possible to control the rendering, but
#' due to some oddities between CRAN requirements and how R handles defaults
#' (for full details see the source code)
#' they are as follows
#'
#' * cgroup.just character; The text of the column justification used in the table
#'
#' * arraystretch numeric; The arraystretch parameter used for vertical spacing
#'
#' * style character; can be null or "nejm" for different table styling
#'
#' * pct_width numeric; a scaling to be applied to the entire table
#'
#' * placement character; placement directive, defaults to "H"
#'
#' @param object object; the item to render to latex
#' @param fragment logical; Is this a complete LaTeX document or just the table fragment
#' @param filename character; filename to write LaTex into
#' @param append logical; Should the write be an append operation or overwrite
#' @param na.blank logical; Dispaly NAs as blanks.
#' @param ... additional arguments
#' @return the LaTeX rendering
#' @include compile-cell.R
#' @include render-latex-map.R
#' @examples
#' \dontrun{
#' latex(cell_label("123"))
#' latex(hmisc_iqr(rnorm(20)))
#' latex(hmisc_fraction(45, 137))
#' tbl <- tangram(drug~bili, pbc, "tbl")
#' latex(tbl)
#' }
#' @rdname latex
#' @export
latex <- function(object, ...)
{
  UseMethod("latex", object)
}

# Helper function to see if in knitr context
# Not perfect, a bit of a hack
# https://stackoverflow.com/questions/33107908/how-to-tell-if-code-is-executed-within-a-knitr-rmarkdown-context
isKnitr <- function()
{
  ("knitr" %in% .packages()) &&
  (isTRUE(getOption('knitr.in.progress')))
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
latex.cell <- function(object, na.blank=TRUE, ...)
{
  if(is.logical(object))
  {
    if(is.na(object))
    {
      return(if(na.blank) "" else "NA")
    } else {
      return(as.character(object))
    }
  }

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
    paste0(label, " {\\textit{\\relsize{-1} ", latexify(attr(object, 'units')), "}}")
}

#' @rdname latex
#' @export
latex.logical <- function(object, ...)
{
  if(is.na(object))
  {
    NA
  } else {
    as.character(object)
  }
}


#' @rdname latex
#' @export
latex.cell_header <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  paste0("\\textbf{",  latex(object, ...), "}")
}

#' @rdname latex
#' @export
latex.cell_subheader <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  paste0("{\\relsize{-1} ",  latex(object, ...), "}")
}

#' @rdname latex
#' @export
latex.tangram <- function(object,
                          fragment=TRUE,
                          filename=NULL,
                          append=FALSE,
                          ...)
{
  # Nasty Automagic handling of special LaTeX parameters
  # Because a tangram object is constructed via a call, that object can be passed to
  # a print function in a rendering context and the additional ... arguments are needed.
  # These are lost in the REPL and need to be recovered.
  # However, someone could call this function directly and those arguments need to take precedence
  # Additionally there needs to be defaults for decent default handling.
  na.blank     <- NULL # These lines are to pass CRAN checks, because it can't fathom assign
  cgroup.just  <- NULL
  arraystretch <- NULL
  pct_width    <- NULL
  relsize      <- NULL
  placement    <- NULL
  style        <- NULL
  pandoc_md    <- NULL
  render_style <- NULL # An override
  defaults <- list(
    na.blank=TRUE,
    cgroup.just=NULL,
    arraystretch=1.2,
    pct_width=1.0,
    relsize=0,
    placement="H",
    style="hmisc",
    pandoc_md=FALSE
  )
  obj.args  <- attr(object, "args")
  obj.args[["style"]] <- attr(object, "style") # And yet another special case
  if("render_style" %in% names(obj.args)) obj.args[["style"]] <- obj.args[["render_style"]]
  call.args <- list(...)
  if("render_style" %in% names(call.args)) warning("`render_style` argument ignored in direct latex call. Use `style` instead.")
  for(i in names(defaults))
  {
    assign(i,
           if(i %in% names(call.args)) { call.args[[i]] } else
           if(i %in% names(obj.args))  { obj.args[[i]]  } else
                                       { defaults[[i]]  }
          )
  }

  if(pct_width != 1.0) stop("tangram now uses longtable and width scaling is no longer supported try relsize=-2")

  # Find footnote
  footnote <- attr(object, "footnote")
  footnote <- if(is.null(footnote)) "" else paste0(latexify(paste0(footnote, collapse="\n\n")), "\n")

  # Find caption
  caption <- attr(object, "caption")
  caption <- if(is.null(caption) || is.na(caption)) "" else latexify(caption)

  result <- ""
  if(!fragment)
  {
    result <- paste(result,
                    "\\documentclass{report}",
                    "\\usepackage{geometry}",
                    "\\begin{document}",
                    sep="\n")
  }
  else if(pandoc_md ||isKnitr())
  {
    result <- paste0(result, "\n```{=latex}\n")
  }

  if(style=="nejm") result <- paste0(result,
                                    "\\definecolor{nejm-yellow}{RGB}{255,251,237}\n",
                                    "\\definecolor{nejm-header}{RGB}{247,244,239}\n")

  if(style=="lancet") result <- paste0(result, "\\definecolor{lancet-red}{RGB}{245,224,220}\n")

  if(relsize != 0) result <- paste0(result, "{\\relsize{",relsize,"}\n")
  if(style %in% c("nejm", "lancet")) result <- paste0(result, "{\\fontfamily{cmss}\\selectfont\n")


  #if(pct_width != 1.0) result <- paste0(result, "\\scalebox{", pct_width, "}{\n")


  nrows <- rows(object)
  ncols <- cols(object)
  text  <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

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

      colspan <- attr(object[[row]][[col]], "colspan")
      if(!is.null(colspan) && colspan > 1)
      {
        text[row, col] <<- paste0("\\multicolumn{",colspan,"}{c}{", text[row, col], "}")
        for(i in (col+1):(col+colspan-1)) object[[row]][[i]] <<- NA
      }

      rowspan <- attr(object[[row]][[col]], "rowspan")
      if(!is.null(rowspan) && rowspan > 1)
      {
        text[row, col] <<- paste0("\\multirow{",rowspan,"}{*}{", text[row, col], "}")
        for(k in (row+1):(row+rowspan-1))
        {
          object[[k]][[col]] <<- cell(" ")
        }
      }
    })
  })

  pasty <- apply(text, 1, function(x) paste0(paste(na.omit(x), collapse=" & "), "\\\\\n"))

  # Special coloring
  if(style=="lancet")
  {
    for(i in 1:length(pasty))
    {
      parity <- attr(object[[i]][[1]], "parity")
      target <- attr(object[[last_header_row+1]][[1]], "parity")
      if(parity == target || i <= last_header_row  )
        pasty[i] <- paste0("\\rowcolor{lancet-red}", pasty[i])
    }
  }

  if(is.null(cgroup.just)) {
    cgroup.just <- paste0(c(rep("l", last_header_col), rep("c", ncols-last_header_col)),collapse="")
    if(style=="nejm")   cgroup.just <- paste0("|", cgroup.just, "|")
    if(style=="lancet") cgroup.just <- paste0("!{\\color{lancet-red}\\vrule width 4pt}", cgroup.just, "!{\\color{lancet-red}\\vrule width 4pt}")
  }

  result <- paste0(result, "\\renewcommand*{\\arraystretch}{", arraystretch, "}\n")
  if(style=="nejm") result <- paste0(result, "\\rowcolors{2}{nejm-yellow}{white}\n")

  result <- paste0(result, "\\begin{longtable}[",placement,"]{",cgroup.just,"}\n")

  if(caption != "") result <- paste0(result, "\\caption{",caption,"} \\\\ \n")


  result <- if(style=="nejm"){
              if(caption=="")
                paste0(result, "\\hline\\hline\n") else
                paste0(result, "\\hline\n\\rowcolor{nejm-header}\\multicolumn{",ncols,"}{|l|}{Table \\thetable{}: ",caption,"} \\\\\n\\hline\n")
            } else if(style=="hmisc") {
              paste0(result, "\\hline\\hline\n")
            } else {
              paste0(result, "\n")
            }

  if(last_header_row > 0) result <- paste0(result, paste0(as.vector(pasty)[1:last_header_row],collapse=""))
  if(style=="hmisc") result  <- paste0(result, "\\hline\n")
  if(style=="lancet") result <- paste0(result, "\\hlineB{2.5}\n")

  result <- paste0(result, paste0(as.vector(pasty)[(last_header_row+1):nrows], collapse=''))


  if(style=="hmisc")  result <- paste0(result, "\\hline\\hline\n")
  if(style=="nejm")   result <- paste0(result, "\\hline\n")
  if(style=="lancet") result <- paste0(result, "\n")

  if(nchar(footnote) > 0 && style=="nejm") result <- paste0(result, "\\rowcolor{white}")
  if(nchar(footnote) > 0) result <- paste0(result, "\\multicolumn{",ncols,"}{p{0.7\\columnwidth}}{", footnote,"}\n")

  result <- paste0(result, "\\end{longtable}\n")

  #if(pct_width != 1.0) result <- paste0(result, "}\n")
  if(relsize != 0) result <- paste0(result, "}\n")


  if(style %in% c("nejm","lancet")) result <- paste0(result, "}\n")

  if(!fragment)
  {
    result <- paste0(result, "\\end{document}\n")
  } else if(pandoc_md || isKnitr())
  {
    result <- paste0(result, "```\n\n")
  }

  result <- gsub("$\\s*$", "", result)

  if(!is.null(filename)) cat(result, file=filename, append=append)

  class(result) <- c("knit_asis", "latex", "character")
  attr(result, "knit_cacheable") <- NA

  result
}
