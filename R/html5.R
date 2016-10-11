#######
# Given the compiled tree of data, render as a text html5

#' @export
custom_css <- function(filename, id=NA)
{
  content <- suppressWarnings(tryCatch(readChar(fileName, file.info(fileName)$size), error=function(e) NA))
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
  cat(gsub("\\n([a-zA-Z.])", paste(id," \\1"), paste("\n",content,sep=''), perl=TRUE))
}

#' @export
html5 <- function(object, ...)
{
  UseMethod("html5", object)
}


# This is the default, do nothing -- probably should be warning()
html5.default <- function(object, ...) "<td></td>"

html5.cell_subheader <- function(object, ...)
{
  paste("<td class=\"subheader\">",
        gsub("^N=","<em>N</em>=", object$label),
        "</td>",
        sep="")
}

html5.cell_header <- function(object, ...)
{
  paste("<td>",
        object$label,
        "</td>",
        sep="")
}

html5.cell_label <- function(object, ...)
{
  if(is.na(object$units))
      paste("<td class=\"label\">",
            "<div class=\"variable\">",
            gsub("^\\s+", "&nbsp;&nbsp;", object$label), # FIXME: replace leading spaces with &nbsp;
            "</div>",
            "</td>",
            sep="")
  else
      paste("<td class=\"label\">",
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
#' Use cases: To provide full html5 page with style links
#'            To provide fragment
#'            To provide fragment with inline style
html5.cell_table <- function(object, caption="Figure", css=NA, fragment=TRUE, inline=NA, ...)
{
  if(!is.na(css)) css <- paste("<link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\"/>", sep='')

  scoped <- if(!is.na(inline)) paste("<style scoped>", custom_css(inline),"</style>", sep='') else ""

  header <- paste("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
                  css,
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">",
	                "<title>",caption,"</title>",
                  "</head><body>", sep='')
  intro <-  paste("<div class=\"figure\">",
                  scoped,
                  "<div class=\"caption\">",caption,"</div>",
		              "<div class=\"figbody\">",
			            "<table class=\"summaryM\">",
                  sep='')

  if(fragment)
  {
    footer <- "</table></div></div>"
  } else {
    intro  <- paste(header, intro, sep='')
    footer <- "</table></div></div></body></html>"
  }

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      text[row,col] <<- html5(object[[row]][[col]])
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

html5.cell_estimate <- function(object, ...)
{
  if(is.na(object$low))
    paste("<td class=\"estimate\"><b>",
          object$low,
          "</b></td>",
          sep="")
  else
    paste("<td class=\"estimate\"><b>",
          object$low,
          "</b>",
          " (",object$low,",",object$high,")",
          "</td>",
          sep="")
}

html5.cell_quantile <- function(object, ...)
{
  paste("<td class=\"quantile\">",
        object$'25%',
        " <b>",
        object$'50%',
        "</b> ",
        object$'75%',
        "</td>",
        sep="")
}


html5.cell_fstat <- function(object, ...)
{
  paste(
    "<td class=\"statistic\">",
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

html5.cell_fraction <- function(object, ...)
{
  x <- sprintf("%3s",round(100*object$numerator/object$denominator,0))
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)

  paste("<td class=\"percent\">",
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

html5.cell_chi2 <- function(object, ...)
{
  paste("<td class=\"statistic\">",
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
