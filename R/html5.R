#######
# Given the compiled tree of data, render as a text html5

clipboard_js <- function()
{
  filename <- file.path(system.file(package="tangram"), "extdata", "js", "clipboard.min.js")
  content <- readChar(filename, file.info(filename)$size)

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
#'
#' @return String of possibly modified CSS file
#' @export
#'
#' @examples
#'
#' custom_css("lancet.css", "tbl1")
#'
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

#' S3 html5 Method function for use on abstract table class
#'
#' @param object The cell to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer.
#' @export
#'

html5 <- function(object, caption, ...)
{
  UseMethod("html5", object)
}

html5_class <- function(classes)
{
  paste("class=\"",
        paste(classes[!is.na(classes)], collapse=" "),
        "\"",
        sep='')
}

#' Default conversion to HTML5 for an abstract table element
#'
#' Gives a warning and produces an empty cell
#'
#' @param object The cell to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given cell as a <td> with several <span>'s.
#' @export
#'
html5.default <- function(object, caption, ..., class=NA)
{
  warning(paste("html5 unhandled class : ", base::class(object), collapse=', '))
  paste("<td ",
        html5_class(c(class, attr(object, "parity"))),
        "></td>",
        sep='')
}

#' Convert an abstract cell object into an HTML5 string
#'
#' Given a cell class create an HTML5 representation.
#'
#' @param object The cell to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given cell as a <td> with several <span>'s.
#' @export
#'
html5.cell <- function(object, caption, ..., class=NA)
{
    paste("<td ",
        html5_class(c(class, attr(object, "parity"))),
        "></td>",
        sep='')
}

#' Convert an abstract cell_n object into an HTML5 string
#'
#' Given a cell_n class create an HTML5 representation.
#'
#' @param object The cell n to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given n as a <td> with several <span>'s.
#' @export
#'
html5.cell_n <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)

  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "N")),
        " data-clipboard-text=\"","{",idx[1]," N=",idx[3],"}\"",
        "><span class=\"N\">",
        object$n,
        "</span></td>",
        sep='')
}

#' Convert an abstract cell_subheader object into an HTML5 string
#'
#' Given a cell_subheader class create an HTML5 representation.
#'
#' @param object The cell subheader to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given subheader as a <td> with several <span>'s.
#' @export
#'
html5.cell_subheader <- function(object, caption, ...)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  if(inherits(object, "cell_n"))
    html5.cell_n(object, caption, class=c("subheader", "header"))
  else
    html5(object, caption, class=c("subheader", "header"))
}

#' Convert an abstract cell_header object into an HTML5 string
#'
#' Given a cell_header class create an HTML5 representation.
#'
#' @param object The cell header to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given header as a <td> with several <span>'s.
#' @export
#'
html5.cell_header <- function(object, caption, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  if(inherits(object, "cell_n"))
    html5.cell_n(object, caption, class=c("header"))
  else
    html5(object, caption, class=c("header"))
}

#' Convert an abstract cell_label object into an HTML5 string
#'
#' Given a cell_label class create an HTML5 representation.
#'
#' @param object The cell label to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given label as a <td> with several <span>'s.
#' @export
#'
html5.cell_label <- function(object, caption, ..., class=NA)
{
  # Turn leading spaces into a set of non breaking html space
  label <- gsub("^\\s+", "&nbsp;&nbsp;&nbsp;&nbsp;", object$label)
  # Turn "*" for interaction terms into a break
  label <- gsub("\\*", "&times;<br/>&nbsp;&nbsp;", label)

  if(is.na(object$units))
      paste("<td ",
            html5_class(c(class, attr(object, "parity"), "tg-label")),
            ">",
            "<span class=\"variable\">",
            label,
            "</span>",
            "</td>",
            sep="")
  else
      paste("<td ",
            html5_class(c(class, attr(object, "parity"), "tg-label")),
            ">",
            "<span class=\"variable\">",
            label,
            "</span>",
            "<span class=\"units\">",
            object$units,
            "</span>",
            "</td>",
            sep="")
}




#' Convert an abstract cell_estimate object into an HTML5 string
#'
#' Given a cell_estimate class create an HTML5 representation.
#'
#' @param object The cell estimate to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given estimate as a <td> with several <span>'s.
#' @export
#'
html5.cell_estimate <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  if(is.na(object$low))
    paste("<td ",
            html5_class(c(class, attr(object, "parity"), "data", "estimate")),
            " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
            ">",
          render_f(object$value),
          "</td>",
          sep="")
  else
    paste("<td ",
            html5_class(c(class, attr(object, "parity"), "data", "estimate")),
            " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
            ">",
          render_f(object$value),
          " (",render_f(object$low),",",render_f(object$high),")",
          "</td>",
          sep="")
}

#' Convert an abstract cell_quantile object into an HTML5 string
#'
#' Given a cell_quantile class create an HTML5 representation.
#'
#' @param object The cell quantile to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given quantile as a <td> with several <span>'s.
#' @export
#'
html5.cell_quantile <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)


  paste("<td class=\"", attr(object, "parity"),"\"><span ",
        html5_class(c(class, attr(object, "parity"), "data", "quantile")),
        " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
        "><span class=\"q25\">",
        render_f(object$'25%', object$format),
        "</span><span class=\"q50\">",
        render_f(object$'50%', object$format),
        "</span><span class=\"q75\">",
        render_f(object$'75%', object$format),
        "</span></span></td>",
        sep="")
}

#' Convert an abstract cell_fstat object into an HTML5 string
#'
#' Given a cell_fstat class create an HTML5 representation.
#'
#' @param object The cell fstat to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given fstat as a <td> with several <span>'s.
#' @export
#'
html5.cell_fstat <- function(object, caption, ..., class=NA)
{
  ref <- if(is.na(object$reference)) "" else paste("<sup>", object$reference, "</sup>", sep="")
  idx <- index(object, caption)
  paste(
    "<td ",
    html5_class(c(class, attr(object, "parity"), "data", "statistics")),
    " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
    ">",
    "<span class=\"statistic\"><span class=\"description\">F",
    "<sub>",object$n1,",",object$n2,"</sub> = </span>",
    render_f(object$f, object$format), ",</span>",
    "<span class=\"pvalue\"><span class=\"description\">P = </span>",
    render_f(object$p, object$format),
    ref,
    "</span>",
    "</td>",
    sep=""
  )
}

#' Convert an abstract cell_fraction object into an HTML5 string
#'
#' Given a cell_fraction class create an HTML5 representation.
#'
#' @param object The cell fraction to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given fraction as a <td> with several <span>'s.
#' @export
#'
html5.cell_fraction <- function(object, caption, ..., class=NA)
{
  idx        <- index(object, caption)
  ratio      <- gsub("\\.", "<div class=\"align\">.</div>", render_f(object$ratio))
  percentage <- render_f(object$percentage)
  den        <- as.character(object$denominator)
  num        <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator) # Adds some spaces to match

  paste("<td class=\"", attr(object, "parity"),"\"><span ",
              html5_class(c(class, attr(object, "parity"),  "fraction")),
              " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"", ">",
          "<span class=\"ratio\">",       ratio,      "</span>",
          "<span class=\"percentage\">",  percentage, "</span>",
          "<span class=\"numerator\">",   num,        "</span>",
          "<span class=\"denominator\">", den,        "</span>",
        "</span></td>",
        sep="")
}

#' Convert an abstract cell_chi2 object into an HTML5 string
#'
#' Given a cell_chi2 class create an HTML5 representation.
#'
#' @param object The cell chi2 to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the HTML5 element
#' @return A text string rendering of the given chi2 as a <td> with several <span>'s.
#' @export
#'
html5.cell_chi2 <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  ref <- if(is.na(object$reference)) "" else paste("<sup>", object$reference, "</sup>", sep="")
  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "statistics")),
        " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
        ">",
        "<span class=\"statistic\"><span class=\"description\"><span class=\"nobr\">&chi;<span class=\"supsub\">2<br/>",
        object$df,
        "</span></span>",
        " = </span>",
        render_f(object$chi2),
        ",</span><span class=\"pvalue\"><span class=\"description\">P = </span>",
        render_f(object$p),
        ref,
        "</span>",
        "</td>",
        sep=""
  )
}

#' Convert an abstract table object into an HTML5 string
#'
#' Given a cell_table class, a series of conversion creates an HTML5
#' representation of the table. It may be an HTML5 fragment or it may
#' be a complete web page.
#'
#' The package includes several css files for styling. At present the following exist: 'hmisc.css', 'lancet.css', 'lancet-stripped.css' and 'nejm.css'
#'
#' @param object The cell table to render to HTML5
#' @param caption A string caption for the table
#' @param css A string that is the href to the css for complete HTML5
#' @param fragment A boolean flag that determines whether a fragment or a complete HTML5 document is generatedf
#' @param inline A string containing a filename to include as inline CSS. It first searches the drive for the file, if that fails it looks inside the package for a matching css file.
#' @param id A unique identifier for the table (strongly recommended).
#' @param footnote Any footnotes to include under the table.
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table
#' @export
#'
html5.cell_table <- function(object, caption=NA, css=NA, fragment=TRUE, inline=NA, id=NA, footnote=NA, ...)
{
  if(!is.na(css)) css <- paste("<link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\"/>", sep='')

  scoped <- if(is.na(inline)) "" else paste("<style>", custom_css(inline,id=id),"</style>", sep='')
  figdiv <- if(is.na(id)) "<div class=\"figure\">" else paste("<div class=\"figure\" id=\"", id,"\">",sep='')
  fontld <- if(fragment) html5_extra_fonts() else ""

  header <- paste("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
                  css,
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">",
	                "<title>",caption,"</title>",
                  "</head><body>", sep='')
  intro <-  paste(fontld,
                  clipboard_js(),
                  figdiv,
                  scoped, sep='')
  if(!is.na(caption)) intro <- paste(intro, "<div class=\"caption\">",caption,"</div>", sep='')
  intro <- paste(intro,
		              "<div class=\"figbody\">",
			            "<table class=\"summaryM\">",
                  sep='')

  if(is.na(footnote) && !is.null(attr(object, "footnote")))
  {
    footnote <- attr(object, "footnote")
  }
  footnote <- if(is.na(footnote)) "" else
  {
    paste("<div class=\"footnote\">", footnote, "</div>", sep='')
  }

  footnote <- gsub("\\^([a-zA-Z0-9_]+)\\^", "<sup>\\1</sup>", footnote, fixed=FALSE)

  if(fragment)
  {
    footer <- paste("</table></div>", footnote,
                    "</div><script>new Clipboard('.data');</script>", sep='')
  } else {
    intro  <- paste(header, intro, sep='')
    footer <- paste("</table></div>", footnote,
                    "</div><script>new Clipboard('.data');</script></body></html>", sep='')
  }

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- html5(object[[row]][[col]], caption)
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
