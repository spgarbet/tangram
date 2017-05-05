est_column_widths <- function(object)
{
  ncols <- cols(object)
  # FIXME: Replace using text summary relative size
  c(0.5, rep(1, ncols))
}

#' S3 rtf Method function for use on abstract table class
#'
#' @param object The cell to render to HTML5
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer.
#' @export
#'

rtf <- function(object, caption, ...)
{
  UseMethod("rtf", object)
}

#' Convert an abstract table object into an RTF string or file
#'
#' Given a cell_table class, a series of conversion creates an rtf
#' representation of the table.
#'
#' @param object The cell table to render to HTML5
#' @param caption A string caption for the table
#' @param fragment A boolean flag that determines whether a fragment or a complete HTML5 document is generatedf
#' @param id A unique identifier for the table (strongly recommended).
#' @param widths RTF requires specified left margin and column widths, this allows user control over these (inches)
#' @param footnote Any footnotes to include under the table.
#' @param filename A filename to write resulting rtf file to
#' @param append A boolean for whether or not to append to given filename
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table
#' @export
#'
rtf.cell_table <- function(object, caption=NA, fragment=FALSE, id=NA, widths=NA, footnote=NA, append=FALSE, ...)
{
  header <- if(fragment) "" else
    paste(
      "{\\rtf1 \\ansi \\deff0",
      "{\\fonttbl",
      "  {\\f0\\froman\\fprq2\\fcharset0 Liberation Serif{\\*\\falt Times New Roman};}",
      "}",
      sep=''
    )

  widths <- cumsum(if(is.na(widths)) est_column_widths(object) else 1440*widths)

  # Construct row open and close text
  rowopen <- paste(
    "\\trowd \\trgaph180",
    "\\trleft", widths[1],
    paste("\\cellx", tail(widths,-1), sep='', collapse=''),
    sep='')
  rowclose <- "\\row"

  # Construct cell open and close text
  cellopen  <- "\\pard\\intbl "
  cellclose <- "\\cell"

  trailer <- "}"

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
  pasty <-  paste(
              rowopen,
              paste(
                cellopen,
                apply(text, 1, function(x) paste(x, collapse=paste(cellclose, cellopen, sep=''))),
                cellclose,
                sep=''
              ),
              rowclose,
              collapse=paste(rowclose, rowopen, sep='')
            )

  result <- paste(header, pasty, trailer, sep='')

  if(!is.na(filename)) cat(result, file=filename, append=append)

  result
}
