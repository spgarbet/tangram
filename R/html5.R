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
        html5_class(class),
        "></td>",
        sep='')
}

#' @export
html5.cell <- function(object, caption, ..., class=NA)
{
    paste("<td ",
        html5_class(class),
        "></td>",
        sep='')
}

#' @export
html5.cell_header_n <- function(object, caption, ..., class=NA)
{
  idx <- index(object, caption)[[1]]

  paste("<td ",
        html5_class(c(class, "data", "N")),
        " data-clipboard-text=\"","{",idx[1]," N=",idx[3],"}\"",
        "><em>N=",
        object$n,
        "</em></td>",
        sep='')
}

#' @export
html5.cell_n <- function(object, caption, ..., class=NA)
{
  # id <-  paste(sample(c(0:9, letters, LETTERS), replace=TRUE, 20), collapse="")
  #
  # paste("<td ",
  #       html5_class(c(class, "N")),
  #       paste(" id=\"", id, "\"", sep=''),
  #       ">",
  #       object$n,
  #       "</td>",
  #       "<script>document.getElementById('",
  #       id,
  #       "').addEventListener('click',function(){clipboard.copy('Testing 1.2.3.')})</script>",
  #       sep='')

  idx <- index(object, caption)[[1]]

  paste("<td ",
          html5_class(c(class, "data", "N")),
          " data-clipboard-text=\"","{",idx[1]," N=",idx[3],"}\"",
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
            html5_class(c(class, "tg-label")),
            ">",
            "<div class=\"variable\">",
            gsub("^\\s+", "&nbsp;&nbsp;", object$label), # FIXME: replace all leading spaces with &nbsp;
            "</div>",
            "</td>",
            sep="")
  else
      paste("<td ",
            html5_class(c(class, "label")),
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
  if(is.na(object$low))
    paste("<td ",
            html5_class(c(class, "estimate")),
            "><b>",
          object$low,
          "</b></td>",
          sep="")
  else
    paste("<td ",
            html5_class(c(class, "estimate")),
            "><b>",
          object$low,
          "</b>",
          " (",object$low,",",object$high,")",
          "</td>",
          sep="")
}

#' @export
html5.cell_quantile <- function(object, caption, ..., class=NA)
{
  paste("<td ",
        html5_class(c(class, "quantile")),
        ">",
        object$'25%',
        " <b>",
        object$'50%',
        "</b> ",
        object$'75%',
        "</td>",
        sep="")
}

#' @export
html5.cell_fstat <- function(object, caption, ..., class=NA)
{
  paste(
    "<td ",
    html5_class(c(class, "statistic")),
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
  x <- sprintf("%3s",round(100*object$numerator/object$denominator,0))
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)

  paste("<td ",
        html5_class(c(class, "percent")),
        ">",
        x,
        "<div class=\"align\">%</div> ",
        "<sup>",
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
  paste("<td ",
        html5_class(c(class, "statistic")),
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
html5.cell_table <- function(object, caption="Figure", css=NA, fragment=TRUE, inline=NA, id=NA, ...)
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
                  scoped,
                  "<div class=\"caption\">",caption,"</div>",
		              "<div class=\"figbody\">",
			            "<table class=\"summaryM\">",
                  sep='')

  if(fragment)
  {
    footer <- "</table></div></div><script>new Clipboard('.data');</script>"
  } else {
    intro  <- paste(header, intro, sep='')
    footer <- "</table></div></div><script>nnew Clipboard('.data');</script></body></html>"
  }

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      text[row,col] <<- html5(object[[row]][[col]], caption)
    })
  })
  pasty <- apply(text, 1, function(x) paste(x, collapse=""))

# FIXME: This is hardcoded at 2!!!!
  tableHdr <- paste(
    "<thead>",
    "<tr>",pasty[1],"</tr>",
    "<tr class=\"subheaderrow\">",pasty[2],"</tr>",
    sep=""
  )

  tableBdy <- paste(
    "<tbody>",
    paste("<tr>",pasty[3:length(pasty)], "</tr>",collapse=""),
    "</tbody>",
    sep="",
    collapse=""
  )

  final <- paste(intro, tableHdr, tableBdy, footer, sep="\n")
  class(final) <- c("html", "character")
  final
}
