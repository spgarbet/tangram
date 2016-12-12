#######
# Given the compiled tree of data, render as a text html5

clipboard_js <- function()
{
  filename <- paste(system.file(package="tg"), "extdata", "js", "clipboard.min.js", sep='/')
  content <- readChar(filename, file.info(filename)$size)

  paste("<script type=\"text/javascript\">", content, "</script>", sep='')
}

#' @export
custom_css <- function(filename, id=NA)
{
  content <- suppressWarnings(tryCatch(readChar(filename, file.info(filename)$size), error=function(e) NA))
  if(is.na(content))
  {
    filename2 <- paste(system.file(package="tg"), "extdata", "css", filename, sep='/')
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

#' @export
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


# This is the default, do nothing
#' @export
html5.default <- function(object, caption, ..., class=NA)
{
  warning(paste("html5 unhandled class : ", base::class(object), collapse=', '))
  paste("<td ",
        html5_class(c(class, attr(object, "parity"))),
        "></td>",
        sep='')
}

#' @export
html5.cell <- function(object, caption, ..., class=NA)
{
    paste("<td ",
        html5_class(c(class, attr(object, "parity"))),
        "></td>",
        sep='')
}

#' @export
html5.cell_header_n <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)

  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "N")),
        " data-clipboard-text=\"","{",idx[1]," N=",idx[3],"}\"",
        "><em>N=",
        object$n,
        "</em></td>",
        sep='')
}

#' @export
html5.cell_n <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)

  paste("<td ",
          html5_class(c(class, attr(object, "parity"), "data", "N")),
          " data-clipboard-text=\"","{",idx["key"]," N=",idx["value"],"}\"",
        ">",
        object$n,
        "</td>",
        sep='')
}


#' @export
html5.cell_subheader <- function(object, caption, ...)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  if(inherits(object, "cell_n"))
    html5.cell_header_n(object, caption, class=c("subheader", "header"))
  else
    html5(object, caption, class=c("subheader", "header"))
}

#' @export
html5.cell_header <- function(object, caption, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  if(inherits(object, "cell_n"))
    html5.cell_header_n(object, caption, class=c("header"))
  else
    html5(object, caption, class=c("header"))
}

#' @export
html5.cell_label <- function(object, caption, ..., class=NA)
{
  if(is.na(object$units))
      paste("<td ",
            html5_class(c(class, attr(object, "parity"), "tg-label")),
            ">",
            "<div class=\"variable\">",
            gsub("^\\s+", "&nbsp;&nbsp;&nbsp;&nbsp;", object$label), # FIXME: replace all leading spaces with &nbsp;
            "</div>",
            "</td>",
            sep="")
  else
      paste("<td ",
            html5_class(c(class, attr(object, "parity"), "label")),
            ">",
            "<div class=\"variable\">",
            object$label,
            "</div>",
            "<div class=\"units\">",
            object$units,
            "</div>",
            "</td>",
            sep="")
}




#' @export
html5.cell_estimate <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  if(is.na(object$low))
    paste("<td ",
            html5_class(c(class, attr(object, "parity"), "data", "estimate")),
            " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
            "><strong>",
          object$value,
          "</strong></td>",
          sep="")
  else
    paste("<td ",
            html5_class(c(class, attr(object, "parity"), "data", "estimate")),
            " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
            "><strong>",
          object$value,
          "</strong>",
          " (",object$low,",",object$high,")",
          "</td>",
          sep="")
}

#' @export
html5.cell_quantile <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)

  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "quantile")),
        " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
        ">",
        object$'25%',
        " <strong>",
        object$'50%',
        "</strong> ",
        object$'75%',
        "</td>",
        sep="")
}

#' @export
html5.cell_fstat <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  paste(
    "<td ",
    html5_class(c(class, attr(object, "parity"), "data", "statistic")),
    " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
    ">",
    "<em>F</em>",
    "<sub>",object$n1,",",object$n2,"</sub> = ",
    object$f,
    ", <em>P</em> = ",
    object$p,
    "<sup>1</sup>",
    "</td>",
    sep=""
  )
}

#' @export
html5.cell_fraction <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  x <- sprintf("%0.3f",round(object$numerator/object$denominator,3))
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)

  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "percent")),
        " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
        ">",
        gsub("\\.", "<div class=\"align\">.</div>",x),
        "&nbsp;<sup>",
        num,
        "</sup>&frasl;<sub>",
        den,
        "</sub>",
        "</td>",
        sep="")
}

#' @export
html5.cell_chi2 <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)
  paste("<td ",
        html5_class(c(class, attr(object, "parity"), "data", "statistic")),
        " data-clipboard-text=\"","{",idx[1]," ",idx[3],"}\"",
        ">",
        "<span class=\"nobr\">&chi;<span class=\"supsub\">2<br/>",
        object$df,
        "</span></span>",
        " = ",
        object$chi2,
        ", <em>P</em> = ",
        object$p,
        "<sup>2</sup>",
        "</td>",
        sep=""
  )
}

#' @export
html5.cell_table <- function(object, caption=NA, css=NA, fragment=TRUE, inline=NA, id=NA, ...)
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

  if(fragment)
  {
    footer <- "</table></div></div><script>new Clipboard('.data');</script>"
  } else {
    intro  <- paste(header, intro, sep='')
    footer <- "</table></div></div><script>new Clipboard('.data');</script></body></html>"
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
