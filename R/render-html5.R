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

htmlsub_table <-list(
  c("\u00A0",                                             "&nbsp;&nbsp;"),  # Special spaces
  c("(^|[^\\\\])\\*\\*((\\\\.|[^\\*\\\\])*)\\*\\*",       "\\1<strong>\\2</strong>"),  # Bold
  c("(^|[^\\\\])__((\\\\.|[^\\_\\\\])*)__",               "\\1<strong>\\2</strong>"),  # Bold
  c("(^|[^\\\\])\\*((\\\\.|[^\\*\\\\])*)\\*",             "\\1<em>\\2</em>"),          # Italic
  c("(^|[^\\\\])_((\\\\.|[^\\_\\\\])*)_",                 "\\1<em>\\2</em>"),          # Italic
  c("(^|[^\\\\])~~((\\\\.|[^\\~\\\\])*)~~",               "\\1<del>\\2</del>"),        # Strikethrough
  c("(^|[^\\\\])`((\\\\.|[^\\`\\\\])*)`",                 "\\1<code>\\2</code>"),      # Code

  c("(^|[^\\\\])\\^((\\\\.|[^\\^\\\\])*)\\^~((\\\\.|[^~\\\\])*)~",             "\\1<span class=\"supsub\">\\2<br/>\\4</span>"),  # Super + Sub
  c("(^|[^\\\\])~((\\\\.|[^~\\\\])+)~\\^((\\\\.|[^\\^\\\\])*)\\^",             "\\1<span class=\"supsub\">\\4<br/>\\2</span>"),  # Sub + Super

  c("(^|[^\\\\])~((\\\\.|[^~\\\\])+)~",                   "\\1<sub>\\2</sub>"),        # Subscript
  c("(^|[^\\\\])\\^((\\\\.|[^\\^\\\\])+)\\^",             "\\1<sup>\\2</sup>"),        # Superscript

  c("(^|[^\\\\])\\\\frac{([^}]*)}{([^}]*)}",              "\\1&nbsp;<span class=\"fraction\"><span class=\"numerator\">\\2</span>/<span class=\"denominator\">\\3</span></span>"),# Make fractions
  c("\\n", "<br/>"),
  c("\\\\(.)",                                            "\\1")         # convert escaped characters
)

# Convert a td string to th
th <- function(x, fixed_thead)
{
  x <- gsub("</td>", "</th>", x)
  if(fixed_thead)
    gsub("<td", "<th style=\"position:sticky;top:0;background-color: #FFFFFF;\"", x)
  else
    gsub("<td", "<th", x)
}

#' @include iify.R
#' @importFrom htmltools htmlEscape
htmlify <- function(x)
{
  x <- htmlEscape(x) # Use the default necessary set of escapes

  # Special handling of leading spaces
  leading <- nchar(stringr::str_match(x, "^\\s+")[1,1])
  if(is.na(leading)) leading <- 0
  leading <- ceiling(leading/2)

  x <- sub("^\\s+", paste0(rep("&nbsp;&nbsp;&nbsp;&nbsp;", leading), collapse=""), x)

  # Apply set of regexs to text
  iify(x, htmlsub_table)
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

  if(is.null(id) || is.na(id) || id==" " || id=="") return(content)

  # sub in a given id
  gsub("\\n([a-zA-Z.#])", paste("\n    #",id," \\1",sep=''), paste("\n",content,sep=''), perl=TRUE)
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
html5 <- function(object, id, ...) UseMethod("html5", object)

# Helper function to turn a vector of strings into html5 class specifier
html5_class <- function(classes, attrs)
{
  cls <- if(is.null(classes)) "" else
           paste0(" class=\"", paste(classes[!is.na(classes)], collapse=" "), "\"")

  rsp <- if(is.null(attrs[['rowspan']])) "" else
           paste0(" rowspan=\"", attrs[['rowspan']], "\"")

  csp <- if(is.null(attrs[['colspan']])) "" else
           paste0(" colspan=\"", attrs[['colspan']], "\"")

  paste0(cls, rsp, csp)
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

#' Default conversion to HTML5 for a logical cell
#'
#' Produces table cell or nothing if it's an NA. This is useful
#' for dealing with rowspan and colspan.
#'
#' @param object The cell to render to HTML5
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return An empty html5 td of the given class
#' @export
html5.logical <- function(object, id, ..., class=NA)
{
  if(is.na(object)) return("") # Deal with rowspan / colspan

  if(object) html5.character("True", id, ..., class=class) else
             html5.character("False",id, ..., class=class)
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
#' @param fragment A boolean flag that determines whether a fragment or a complete HTML5 document is generatedf
#' @param style A string containing a style filename to include as inline CSS. It first searches the drive for the file, if that fails it looks inside the package for a matching css file.
#' @param footnote Any footnotes to include under the table.
#' @param fixed_thead logical; fixes the header using position sticky in CSS defaults to FALSE
#' @param inline DEPRECATED
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table in HTML5
#' @export
html5.tangram <- function(object, id=NULL, caption=NULL, fragment=NULL, style=NULL, footnote=NULL, inline=NULL, fixed_thead=NULL, ...)
{
  # Unused at present
  #if(!is.na(css)) css <- paste("<link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\"/>", sep='')

  if(is.null(id))          id          <- attr(object, "id")
  if(is.null(fragment))    fragment    <- attr(object, "fragment")
  if(is.null(fragment))    fragment    <- TRUE
  if(is.null(caption))     caption     <- attr(object, "caption")
  if(is.null(style))       style       <- attr(object, "style")
  if(is.null(fixed_thead)) fixed_thead <- attr(object, "fixed_thead")
  if(is.null(fixed_thead)) fixed_thead <- FALSE

  if(!is.null(inline))
  {
    warning("Deprecated inline argument to tangram::html5() call. Use style instead.")
    if(is.null(style))
    {
      style <- substr(inline, 1, nchar(style)-4)
    } else stop("inline and style argument specified in call to tangram::html5() use only style.")
  }

  # Default back to hmisc for style
  if(is.null(style)) style <- "hmisc"

  if(is.null(id))
  {
    warning("No unique id for table specified. CSS styling will be unstable")
    id <- ""
  }

  scoped <- if(is.na(style)) "" else paste("<style>", custom_css(paste0(style,".css"),id=id),"</style>", sep='')
  figdiv <- if(is.null(id)) "<div class=\"figure\">" else paste("<div class=\"figure\" id=\"", id,"\">",sep='')
  fontld <- if(fragment) "" else html5_extra_fonts()

  header <- paste0("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
#                   css,
                   "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">",
	                 "<title>",caption,"</title>",
                   "</head><body>")
  intro  <- paste0(fontld,
                   figdiv,
                   scoped)
  if(!is.null(caption)) intro <- paste0(intro, "<div class=\"caption\">",caption,"</div>")
  intro <- paste(intro,
		              "<div class=\"figbody\">",
			            "<table class=\"tangram\">",
                  sep='')

  if(is.null(footnote)) footnote <- attr(object, "footnote")
  footnote <- if(is.null(footnote)) "" else
  {
    paste("<div class=\"footnote\">", paste(htmlify(footnote), collapse=" "), "</div>", sep='')
  }

  if(fragment)
  {
    footer <- paste0("</table></div>", footnote,"</div>")
  } else {
    intro  <- paste0(header, intro)
    footer <- paste0("</table></div>", footnote, "</div></body></html>")
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
      colspan <- attr(object[[row]][[col]], "colspan")
      if(!is.null(colspan) && colspan > 1)
      {
        for(i in (col+1):(col+colspan-1)) object[[row]][[i]] <<- NA
      }
      rowspan <- attr(object[[row]][[col]], "rowspan")
      if(!is.null(rowspan) && rowspan > 1)
      {
        for(i in (row+1):(row+rowspan-1)) object[[i]][[col]] <<- NA
      }

    })
  })
  pasty <- apply(text, 1, function(x) paste(x, collapse=""))

  if(last_header_row > 0)
  {
    tableHdr <- "<thead>"
    sapply(1:last_header_row,
      FUN=function(row) {
        if(row < 2) tableHdr <<- paste(tableHdr, "<tr>", th(pasty[row], fixed_thead), "</tr>", sep='')
        else        tableHdr <<- paste(tableHdr, "<tr class=\"subheaderrow\">", th(pasty[row], FALSE), "</tr>", sep='')
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
  attr(final, "knit_cacheable") <- NA
  class(final) <- c("knit_asis", "html", "character")
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

  if(is.null(class))
  {
    idx <- match("cell", base::class(object))
    if(idx > 1) class <- base::class(object)[1:(idx-1)]
  }

  x <- if(is.null(names(object)))
  {
    paste(object, collapse=sep)
  } else {
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    paste(paste0(name, as.character(object)), collapse=sep)
  }

  paste0("<td",
         html5_class(c(class, attr(object, "parity")), attributes(object)),
         ">", htmlify(x), "</td>")
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
#' @importFrom stringr str_match
#' @export
html5.cell_label <- function(object, id, ..., class=NULL)
{
  # Turn leading spaces into a set of non breaking html space
  leading <- nchar(stringr::str_match(object, "^\\s+")[1,1])
  if(is.na(leading)) leading <- 0
  leading <- ceiling(leading/2)

  label <- gsub("^\\s+",
                paste0(rep("&nbsp;&nbsp;&nbsp;&nbsp;", leading), collapse=""),
                htmlify(object))
  # Turn "*" for interaction terms into a break
  label <- gsub("\\*", "&times;<br/>&nbsp;&nbsp;", label)

  if(is.null(attr(object, "units")))
      paste0("<td",html5_class(c(class, attr(object, "parity"), "tg-label"), attributes(object)),">",
             "<span class=\"variable\">",
             label,
             "</span></td>")
  else
      paste0("<td",html5_class(c(class, attr(object, "parity"), "tg-label"), attributes(object)),">",
             "<span class=\"variable\">",
             label,
             "</span>",
             "<span class=\"units\">",
             htmlify(attr(object,"units")),
             "</span></td>")
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
  paste0("<td",
         html5_class(c(class, attr(object, "parity"), "data", "N"), attributes(object)),
         "><span class=\"N\">",
         htmlify(object),
         "</span></td>")
}
