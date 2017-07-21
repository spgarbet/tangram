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

#' @importFrom htmltools htmlEscape
reference <- function(object)
{
  if(is.null(attr(object,"reference"))) "" else
    paste0("<sup>", htmlEscape(attr(object, "reference")), "</sup>")
}

# Provide the clipboard copy javascript function
clipboard_js <- function()
{
  filename <- file.path(system.file(package="tangram"), "extdata", "js", "clipboard.min.js")
  content  <- readChar(filename, file.info(filename)$size)

  paste("<script type=\"text/javascript\">", content, "</script>", sep='')
}

#' Return a CSS file as a string
#'
#' Given a filename, this function will load the file name from the current working directory.
#' If it is not found from the current working directory it will search in the package for a
#' a matching filename and load that instead. If an id is specified, that will be prepended
#' to all CSS selectors (TODO: make this substitution more robust). The result is returned
#' as a string.
#'
#' @param filename Name of the CSS file to load
#' @param id       CSS id to prepend to all entries
#' @return String of possibly modified CSS file
#' @export
#' @examples
#' custom_css("lancet.css", "tbl1")
custom_css <- function(filename, id=NA)
{
  content <- suppressWarnings(tryCatch(readChar(filename, file.info(filename)$size), error=function(e) NA))
  if(is.na(content))
  {
    filename2 <- file.path(system.file(package="tangram"), "extdata", "css", filename)
    content <- suppressWarnings(tryCatch(readChar(filename2, file.info(filename2)$size), error=function(e) NA))
  }

  if(is.na(content))
  {
    stop(paste("cannot open file '", filename, "': No such file or directory", sep=''))
  }

  if(is.na(id)) return(content)

  # sub in a given id
  gsub("\\n([a-zA-Z.#])", paste("\n#",id," \\1",sep=''), paste("\n",content,sep=''), perl=TRUE)
}

# Helper function to include extra fonts
html5_extra_fonts <- function()
{
  paste("<script type=\"text/javascript\">",
        "var ss = document.createElement(\"link\");",
        "ss.rel  = \"stylesheet\";",
        "ss.type = \"text/css\";",
        "ss.href = \"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\";",
        "document.getElementsByTagName(\"head\")[0].appendChild(ss);",
        "</script>",
        sep="\n")
}

#' S3 html5 Method function for use on a tangram to generate HTML5
#'
#' @param object The cell to render to HTML5
#' @param id A unique identifier for traceability in indexing
#' @param ... additional arguments to renderer.
#' @export
html5 <- function(object, id, ...)
{
  UseMethod("html5", object)
}

# Helper function to turn a vector of strings into html5 class specifier
html5_class <- function(classes)
{
  paste0("class=\"",
         paste(classes[!is.na(classes)], collapse=" "),
         "\"")
}

#' Default conversion to HTML5 for an abstract table element
#'
#' Gives a warning and produces an empty <td></td> cell
#'
#' @param object The cell to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return An empty html5 td of the given class
#' @export
html5.default <- function(object, id, ..., class=NA)
{
  warning(paste("html5 unhandled class : ", base::class(object), collapse=', '))
  html5.cell(object, id, ..., class=class)
}

#' Default conversion to HTML5 for a character cell
#'
#' Produces table cell
#'
#' @param object The cell to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return An empty html5 td of the given class
#' @export
html5.character <- function(object, id, ..., class=NA)
{
  html5.cell(object, id, ..., class=class)
}

#' Convert a tangram class into an HTML5 string
#'
#' Given a tangram class, a series of conversion creates an HTML5
#' representation of the table. It may be an HTML5 fragment or it may
#' be a complete web page.
#'
#' The package includes several css files for styling. At present the following exist: 'hmisc.css', 'lancet.css', 'lancet-stripped.css' and 'nejm.css'
#'
#' @param object The cell table to render to HTML5
#' @param id A unique identifier for the table (strongly recommended). If not provided, caption will be used.
#' @param caption A string caption for the table
#' @param css A string that is the href to the css for complete HTML5
#' @param fragment A boolean flag that determines whether a fragment or a complete HTML5 document is generatedf
#' @param inline A string containing a filename to include as inline CSS. It first searches the drive for the file, if that fails it looks inside the package for a matching css file.
#' @param footnote Any footnotes to include under the table.
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table in HTML5
#' @export
html5.tangram <- function(object, id=NA, caption=NA, css=NA, fragment=TRUE, inline=NA, footnote=NA, ...)
{
  if(!is.na(css)) css <- paste("<link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\"/>", sep='')
  if(is.na(id))
  {
    warning("No id specified for later traceability of table elements")
    id <- ""
  }

  scoped <- if(is.na(inline)) "" else paste("<style>", custom_css(inline,id=id),"</style>", sep='')
  figdiv <- if(is.na(id)) "<div class=\"figure\">" else paste("<div class=\"figure\" id=\"", id,"\">",sep='')
  fontld <- if(fragment) "" else html5_extra_fonts()

  header <- paste0("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
                   css,
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">",
	                 "<title>",caption,"</title>",
                   "</head><body>")
  intro  <- paste0(fontld,
                   clipboard_js(),
                   figdiv,
                   scoped)
  if(!is.na(caption)) intro <- paste0(intro, "<div class=\"caption\">",caption,"</div>")
  intro <- paste(intro,
		              "<div class=\"figbody\">",
			            "<table class=\"tangram\">",
                  sep='')

  if(is.na(footnote) && !is.null(attr(object, "footnote")))
  {
    footnote <- attr(object, "footnote")
  }
  footnote <- if(is.na(footnote)) "" else
  {
    paste("<div class=\"footnote\">", footnote, "</div>", sep='')
  }

  footnote <- gsub("\\^(.)\\^", "<sup>\\1</sup>", footnote, fixed=FALSE)

  if(fragment)
  {
    footer <- paste0("</table></div>", footnote,
                     "</div><script>new Clipboard('.data');</script>")
  } else {
    intro  <- paste0(header, intro)
    footer <- paste0("</table></div>", footnote,
                     "</div><script>new Clipboard('.data');</script></body></html>")
  }

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- html5(object[[row]][[col]], id, ...)
    })
  })
  pasty <- apply(text, 1, function(x) paste(x, collapse=""))

  if(last_header_row > 0)
  {
    tableHdr <- "<thead>"
    sapply(1:last_header_row,
           FUN=function(row) {
             if(row < 2) tableHdr <<- paste(tableHdr, "<tr>", pasty[row], "</tr>", sep='')
             else        tableHdr <<- paste(tableHdr, "<tr class=\"subheaderrow\">", pasty[row], "</tr>", sep='')
           }
    )
    tableHdr <- paste(tableHdr, "</thead>", sep='')
  }
  else tableHdr <- ""

  tableBdy <- paste(
    "<tbody>",
    paste("<tr>",pasty[(last_header_row+1):length(pasty)], "</tr>",collapse=""),
    "</tbody>",
    sep="",
    collapse=""
  )

  final <- paste(intro, tableHdr, tableBdy, footer, sep="\n")
  class(final) <- c("html", "character")
  final
}

#' Convert an abstract cell object into an HTML5 string
#'
#' Given a cell class create an HTML5 representation.
#'
#' @param object The cell to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given cell as a <td> with several <span>'s.
#' @importFrom htmltools htmlEscape
#' @export
html5.cell <- function(object, id, ..., class=NULL)
{
  sep  <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")

  x <- if(is.null(names(object)))
  {
    paste(object, collapse=sep)
  } else {
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    paste(paste0(name, as.character(object)), collapse=sep)
  }

  paste0("<td ",
         html5_class(c(class, attr(object, "parity"))),
         ">", htmlEscape(x), reference(object), "</td>")
}

#' Convert an abstract cell_subheader object into an HTML5 string
#'
#' Given a cell_subheader class create an HTML5 representation.
#'
#' @param object The cell subheader to render to HTML5
#' @param id A unique identifier for traceability
#' @param class additional class attributes for CSS rendering
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given subheader as a <td> with several <span>'s.
#' @importFrom htmltools htmlEscape
#' @export
html5.cell_subheader <- function(object, id, ..., class=NULL)
{
  cls <- class(object)
  class(object) <- cls[2:length(cls)]
  html5(object, id, ..., class=c(NULL, "subheader"))
}

#' Convert an abstract cell_header object into an HTML5 string
#'
#' Given a cell_header class create an HTML5 representation.
#'
#' @param object The cell subheader to render to HTML5
#' @param id A unique identifier for traceability
#' @param class additional class attributes for CSS rendering
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given subheader as a <td> with several <span>'s.
#' @importFrom htmltools htmlEscape
#' @export
html5.cell_header <- function(object, id, ..., class=NULL)
{
  cls <- class(object)
  class(object) <- cls[2:length(cls)]
  if("cell_n" %in% class(object))
  {
    html5.cell_n(object, id, ..., class=c(class, "header"))
  } else {
    html5(object, id, ..., class=c(class, "header"))
  }
}


#' Convert a cell_label object into an HTML5 string
#'
#' Given a cell_label class create an HTML5 representation.
#'
#' @param object The cell label to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given label as a <td> with several <span>'s.
#' @importFrom htmltools htmlEscape
#' @export
html5.cell_label <- function(object, id, ..., class=NULL)
{
  # Turn leading spaces into a set of non breaking html space
  label <- gsub("^\\s+", "&nbsp;&nbsp;&nbsp;&nbsp;", htmlEscape(object))
  # Turn "*" for interaction terms into a break
  label <- gsub("\\*", "&times;<br/>&nbsp;&nbsp;", label)

  if(is.null(attr(object, "units")))
      paste0("<td ",
             html5_class(c(class, attr(object, "parity"), "tg-label")),
             ">",
             "<span class=\"variable\">",
             label,
             "</span>",
             reference(object),
             "</td>")
  else
      paste0("<td ",
             html5_class(c(class, attr(object, "parity"), "tg-label")),
             ">",
             "<span class=\"variable\">",
             label,
             "</span>",
             "<span class=\"units\">",
             htmlEscape(attr(object,"units")),
             "</span>",
             reference(object),
             "</td>")
}

#' Convert a cell_estimate object into an HTML5 string
#'
#' Given a cell_estimate class create an HTML5 representation.
#'
#' @param object The cell estimate to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given estimate as a <td> with several <span>'s.
#' @export
html5.cell_estimate <- function(object, id, ..., class=NULL)
{
  idx <- index(object, id)

  paste0("<td ",
            html5_class(c(class, attr(object, "parity"), "data", "estimate")),
            " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
            ">",
          htmlEscape(object[[1]]),
          " (",htmlEscape(paste0(object[[2]], collapse = ", ")),")",
          reference(object),
          "</td>")
}

#' Convert a cell_iqr object into an HTML5 string
#'
#' Given a cell_iqr class create an HTML5 representation.
#'
#' @param object The cell iqr to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given quantile as a <td> with several <span>'s.
#' @export
#'
html5.cell_iqr <- function(object, id, ..., class=NULL)
{
  idx <- index(object, id)
  ref <- if(is.null(attr(object,"reference"))) "" else paste0("<sup>", htmlEscape(attr(object, "reference")), "</sup>")

  mid <- floor(length(object)/2) + 1
  y <- as.character(object)
  x <- y[mid] <- paste0("*", y[mid], "*")

  result <- paste0(y, collapse=' ')

  z <- attr(object, "msd")

  result <-
      paste0("<td class=\"", attr(object, "parity"),"\"><span ",
         html5_class(c(class, attr(object, "parity"), "data", "quantile")),
         ">",
         paste0("<span class=\"q25\">", htmlEscape(object[1:(mid-1)]), "</span>", collapse=""),
         "<span class=\"q50\">", htmlEscape(object[mid]), "</span>",
         paste0("<span class=\"q75\">", htmlEscape(object[(mid+1):length(object)]), "</span>", collapse=""),
         reference(object))

  if(is.null(z)) paste0(result, "</td>") else
  {
    paste0(result, '<br/><span>', htmlEscape(z[1]), '&plusmn;', htmlEscape(z[2]), "</span></td>")
  }
}


#' Convert an abstract cell_n object into an HTML5 string
#'
#' Given a cell_n class create an HTML5 representation.
#'
#' @param object The cell n to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given n as a <td> with several <span>'s.
#' @export
#'
html5.cell_n <- function(object, id, ..., class=NULL)
{
  ref <- if(is.null(attr(object,"reference"))) "" else paste0("<sup>", htmlEscape(attr(object, "reference")), "</sup>")

  idx <- index(object, id)

  paste0("<td ",
         html5_class(c(class, attr(object, "parity"), "data", "N")),
         " data-clipboard-text=\"","{",idx[1]," N=",idx[3],"}\"",
         "><span class=\"N\">",
         htmlEscape(object),
         "</span>",
         reference(object),
         "</td>")
}


#' Convert a cell_fstat object into an HTML5 string
#'
#' Given a cell_fstat class create an HTML5 representation.
#'
#' @param object The cell fstat to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given fstat as a <td> with several <span>'s.
#' @export
html5.cell_fstat <- function(object, id, ..., class=NULL)
{
  idx <- index(object, id)
  paste0(
    "<td ",
    html5_class(c(class, attr(object, "parity"), "data", "statistics")),
    " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
    ">",
    "<span class=\"statistic\"><span class=\"description\">F",
    "<sub>",object["df1"],",",object["df2"],"</sub> = </span>",
    object["F"], ",</span>",
    "<span class=\"pvalue\"><span class=\"description\">P = </span>",
    object["P"],
    reference(object),
    "</span>",
    "</td>"
  )
}


#' Convert an abstract cell_fraction object into an HTML5 string
#'
#' Given a cell_fraction class create an HTML5 representation.
#'
#' @param object The cell fraction to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given fraction as a <td> with several <span>'s.
#' @export
#'
html5.cell_fraction <- function(object, id, ..., class=NULL)
{
  idx        <- index(object, id)
  ratio      <- gsub("\\.", "<div class=\"align\">.</div>", object["ratio"])
  percentage <- object["percentage"]
  den        <- object["denominator"]
  num        <- sprintf(paste("%",nchar(den),"s",sep=''), object["numerator"]) # Adds some spaces to match

  paste0("<td class=\"", attr(object, "parity"),"\"><span ",
               html5_class(c(class, attr(object, "parity"),  "fraction")),
               " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"", ">",
           "<span class=\"ratio\">",       ratio,      "</span>",
           "<span class=\"percentage\">",  percentage, "</span>",
           "<span class=\"numerator\">",   num,        "</span>",
           "<span class=\"denominator\">", den,        "</span>",
         "</span>",reference(object),"</td>")
}

#' Convert an abstract cell_chi2 object into an HTML5 string
#'
#' Given a cell_chi2 class create an HTML5 representation.
#'
#' @param object The cell chi2 to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given chi2 as a <td> with several <span>'s.
#' @export
#'
html5.cell_chi2 <- function(object, id, ..., class=NULL)
{
  idx <- index(object, id)

  paste0("<td ",
         html5_class(c(class, attr(object, "parity"), "data", "statistics")),
         " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
         ">",
         "<span class=\"statistic\"><span class=\"description\"><span class=\"nobr\">&chi;<span class=\"supsub\">2<br/>",
         object[2],
         "</span></span>",
         " = </span>",
         object[1],
         ",</span><span class=\"pvalue\"><span class=\"description\">P = </span>",
         object[3],
         "</span>",
          reference(object),
         "</td>"
  )
}

