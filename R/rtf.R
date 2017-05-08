
#' Default conversion to RTF for an abstract table element
#'
#' Gives a warning and produces an empty cell
#'
#' @param object The cell to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @return A RTF string rendering of the given cell
#' @export
#'
rtf.default <- function(object, ...)
{
  warning(paste("rtf unhandled class : ", base::class(object), collapse=', '))
  ""
}

#'
#' Given a cell_label class create an RTF representation.
#'
#' @param object The cell label to render to RTF
#' @param caption A string caption for the table
#' @param ... additional arguments to renderer. Unused
#' @return An RTF text string rendering of the given label.
#'
rtf.cell_label <- function(object, ...)
{
  # Turn leading spaces into a set of non breaking html space
  label <- gsub("^\\s+", "    ", object$label)
  # Turn "*" for interaction terms into a break
  label <- gsub("\\*", "X\n  ", label)

  if(is.na(object$units))
      label
  else
      paste0(label,
            " \\fs16\\i\\b0 ",
            object$units,
            "\\i0 "
            )
}

#' Convert an abstract cell_n object into an RTF string
#'
#' Given a cell_n class create an RTF representation.
#'
#' @param object The cell n to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given n.
#' @export
#'
rtf.cell_n <- function(object, ...)
{
  #idx <- index(object, caption)  ## WHY CAPTION FIXME: REVISIT

  as.character(object$n)
}

#' Convert an abstract cell_header object into an RTF string
#'
#' Given a cell_header class create an RTF representation.
#'
#' @param object The cell header to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given header
#' @export
#'
rtf.cell_header <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  if(inherits(object, "cell_n"))
    paste0("{\\b N=", rtf.cell_n(object), "}")
  else # Peel down to cell_label
    paste0("{\\b ", rtf(object), "}")
}

#' Convert an abstract cell_subheader object into an RTF string
#'
#' Given a cell_subheader class create an RTF representation.
#'
#' @param object The cell header to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given header
#' @export
#'
rtf.cell_subheader <- function(object, ...)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  if(inherits(object, "cell_n"))
    paste0("{\\fs16 N=", rtf.cell_n(object), "}")
  else # Peel down to cell_label
    paste0("{\\fs16", rtf(object), "}")
}

#' Convert an abstract cell_quantile object into an RTF string
#'
#' Given a cell_quantile class create an RTF representation.
#'
#' @param object The cell quantile to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the RTF element
#' @return An RTF string rendering of the given quantile.
#' @export
#'
rtf.cell_quantile <- function(object, ..., class=NA)
{
  #idx <- index(object, caption)

  paste0("\\fs16 ",
        render_f(object$'25%', object$format),
        " \\b\\fs18 ",
        render_f(object$'50%', object$format),
        "\\b0\\fs16  ",
        render_f(object$'75%', object$format)
  )
}

#' Convert an abstract cell_fstat object into an RTF string
#'
#' Given a cell_fstat class create an RTF representation.
#'
#' @param object The cell fstat to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @param class An additional class attribute for the RTF element
#' @return A text string rendering of the given fstat as a <td> with several <span>'s.
#' @export
#'
rtf.cell_fstat <- function(object, caption, ..., class=NA)
{
  ref <- if(is.na(object$reference)) "" else paste0("{\\super ", object$reference, "}")
  #idx <- index(object, caption)
  paste0(
    "F{\\sub ",
    object$n1,",",object$n2, "}=",
    render_f(object$f, object$format),
    ", P=",
    render_f(object$p, object$format),
    ref
  )
}

#' Given a cell class create an RTF representation.
#'
#' @param object The cell to render to RTF
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given cell.
#' @export
#'
rtf.cell <- function(object, ...) ""

est_column_widths <- function(object)
{
  ncols <- cols(object)
  # FIXME: Replace using text summary relative size
  c(0, rep(1.5, ncols))
}

#' S3 rtf Method function for use on abstract table class
#'
#' @param object The cell to render to RTF
#' @param ... additional arguments to renderer.
#' @export
#'
rtf <- function(object, ...)
{
  UseMethod("rtf", object)
}

#' Convert an abstract table object into an RTF string or file
#'
#' Given a cell_table class, a series of conversion creates an rtf
#' representation of the table.
#'
#' @param object The cell table to render to RTF
#' @param caption A string caption for the table
#' @param fragment A boolean flag that determines whether a fragment or a complete RTF document is generatedf
#' @param id A unique identifier for the table (strongly recommended).
#' @param widths RTF requires specified left margin and column widths, this allows user control over these (inches)
#' @param footnote Any footnotes to include under the table.
#' @param filename A filename to write resulting rtf file to
#' @param append A boolean for whether or not to append to given filename
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table
#' @export
#'
rtf.cell_table <- function(object, caption=NA, fragment=FALSE, id=NA, widths=NA, footnote=NA, filename=NA, append=FALSE, ...)
{
  header <- if(fragment) "" else
    paste(
      "{\\rtf1 \\ansi \\deff0",
      "{\\fonttbl",
      "  {\\f0\\froman\\fprq2\\fcharset0 Liberation Serif{\\*\\falt Times New Roman};}",
      "}",
      "\\paperw15840\\paperh12240",
      "\\margl720\\margr720\\margt1440\\margb1440",
      "\\landscape",
      sep=''
    )

  widths <- 1440*cumsum(if(is.na(widths)) est_column_widths(object) else widths)

  # Construct row open and close text
  celldecl <-
  rowopen <- paste0(
    "\\trowd \\trgaph90",
    "\\trleft", widths[1]
  )
  rowclose <- "\\row\n"

  # Construct RTF Row Openings (see pg84, RTF Pocket Guide)
  firstrowopen <- paste0(rowopen, paste0("\\clbrdrt\\brdrs\\cellx",  tail(widths,-1), collapse=''), '\n')
  lasthdropen  <- paste0(rowopen, paste0("\\clbrdrb\\brdrdb\\cellx", tail(widths,-1), collapse=''), '\n')
  lastrowopen  <- paste0(rowopen, paste0("\\clbrdrb\\brdrs\\cellx",  tail(widths,-1), collapse=''), '\n')
  rowopen      <- paste0(rowopen, paste0("\\cellx", tail(widths,-1), collapse=''), '\n')

  # Construct cell open and close text
  cellopen  <- "\\pard\\qc\\intbl\\fs18 "
  cellclose <- "\\cell\n"

  trailer <- "}"

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- rtf(object[[row]][[col]])
    })
  })
  rowtext <- paste0(
    cellopen,
    apply(text, 1, function(x) paste(x, collapse=paste(cellclose, cellopen, sep=''))),
    cellclose
  )
  fullrows <- sapply(1:nrows, function(row) {
    hdropen <- if(row == 1)                { firstrowopen } else
               if(row == last_header_row)  { lasthdropen  } else
               if(row == nrows)            { lastrowopen  } else
                                           { rowopen      }
    paste0(hdropen, rowtext[row], rowclose)
  })
  pasty <-  paste0(fullrows, collapse='')

  result <- paste0(header, pasty, trailer, collapse='')

  if(!is.na(filename)) cat(result, file=filename, append=append)

  result
}
