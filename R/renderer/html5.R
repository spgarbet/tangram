#######
# Given the compiled tree of data, render as a text html5

html5 <- function(x, ...) 
{
  UseMethod("html5", x)
}


# This is the default, do nothing -- probably should be warning()
html5.tg_cell <- function(object) "<td></td>" 

html5.tg_subheader <- function(object)
{
  paste("<td class=\"subheader\">",
        gsub("^N=","<em>N</em>=", object$label),
        "</td>",
        sep="")
}

html5.tg_header <- function(object)
{
  paste("<td>",
        object$label,
        "</td>",
        sep="")
}

html5.tg_label <- function(object) 
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


html5.tg_table <- function(object, css="Hmisc.css", caption="Figure")
{
  header <- paste("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"", css, "\"/>",
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">",
	                "<title>",caption,"</title>",
                  "</head><body><div class=\"figure\">",
                  "<div class=\"caption\">",caption,"</div>",
		              "<div class=\"figbody\">",
			            "<table class=\"summaryM\">",
                  sep="")
  
  footer <- "</table></div></div></body></html>"
  
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

  paste(header, tableHdr, tableBdy, footer, sep="\n")
}

html5.tg_estimate <- function(object)
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

html5.tg_quantile <- function(object)
{
  paste("<td class=\"quantile\">",
        sigfig(object$q25),
        " <b>",
        sigfig(object$q50),
        "</b> ",
        sigfig(object$q75),
        "</td>",
        sep="")
}


html5.tg_fstat <- function(object)
{
  paste(
    "<td class=\"statistic\">",
    "<em>F</em>",
    "<sub>",object$n1,",",object$n2,"</sub> = ",
    roundfig(object$f,2),
    ", <em>P</em> = ",
    roundfig(object$p,3),
    "<sup>1</sup>",
    "</td>",
    sep=""
  )
}

html5.tg_fraction <- function(object)
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

html5.tg_chi2 <- function(object)
{
  paste("<td class=\"statistic\">",
        "<span class=\"nobr\">&chi;<span class=\"supsub\">2<br/>",
        object$df,
        "</span></span>",
        " = ",
        roundfig(object$chi2,2),
        ", <em>P</em> = ",
        roundfig(object$p,3),
        "<sup>2</sup>",
        "</td>",
        sep=""
  )
}