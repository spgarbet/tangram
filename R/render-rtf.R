
#' Default conversion to RTF for an abstract table element
#'
#' Gives a warning and produces an empty cell
#'
#' @param object The cell to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return A RTF string rendering of the given cell
#' @export
#'
rtf.default <- function(object, id, ...)
{
  warning(paste("rtf unhandled class : ", base::class(object), collapse=', '))
  ""
}

#'
#' Given a cell_label class create an RTF representation.
#'
#' @param object The cell label to render to RTF
#' @param id A string caption for the table
#' @param point
#' @param ... additional arguments to renderer. Unused
#' @return An RTF text string rendering of the given label.
#'
rtf.cell_label <- function(object, id, ..., point=18)
{
  # Turn leading spaces into a set of non breaking html space
  label <- gsub("^\\s+", "    ", object$label)
  # Turn "*" for interaction terms into a break
  label <- gsub("\\*", "X\n  ", label)

  if(is.na(object$units))
      label
  else
      paste0(label,
            " {\\fs", round(point*1.6),
            "\\i\\b0 ",
            object$units,
            "}"
            )
}

#' Convert an abstract cell_n object into an RTF string
#'
#' Given a cell_n class create an RTF representation.
#'
#' @param object The cell n to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given n.
#' @export
#'
rtf.cell_n <- function(object, id, ...)
{
  idx <- index(object, id)

  as.character(object$n)
}

#' Convert an abstract cell_header object into an RTF string
#'
#' Given a cell_header class create an RTF representation.
#'
#' @param object The cell header to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given header
#' @export
#'
rtf.cell_header <- function(object, id, ...)
{
  cls <- class(object)

  class(object) <- cls[2:length(cls)]

  if(inherits(object, "cell_n"))
    paste0("{\\b N=", rtf.cell_n(object, id, ...), "}")
  else # Peel down to cell_label
    paste0("{\\b ", rtf(object, id, ...), "}")
}

#' Convert an abstract cell_subheader object into an RTF string
#'
#' Given a cell_subheader class create an RTF representation.
#'
#' @param object The cell header to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given header
#' @export
#'
rtf.cell_subheader <- function(object, id, ..., point=9)
{
  cls <- class(object)

  class(object) <- cls[3:length(cls)]

  fontsize <- paste0("\\fs", round(point*1.6), " ")

  if(inherits(object, "cell_n"))
    paste0("{", fontsize, " N=", rtf.cell_n(object, id, ...), "}")
  else # Peel down to cell_label
    paste0("{", fontsize, rtf(object, id, ...), "}")
}

#' Convert an abstract cell_quantile object into an RTF string
#'
#' Given a cell_quantile class create an RTF representation.
#'
#' @param object The cell quantile to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given quantile.
#' @export
#'
rtf.cell_quantile <- function(object, id, ..., point=9)
{
  idx <- index(object, id)

  small <- paste0("\\fs", round(point*1.6), " ")
  large <- paste0("\\fs", round(point*2.0), " ")

  paste0("{",small,
        render_f(object$'25%', object$format),
        " \\b",large,
        render_f(object$'50%', object$format),
        "\\b0",small," ",
        render_f(object$'75%', object$format),
        "}"
  )
}

dttm_datetime <- function()
{
  date  <- as.POSIXlt(Sys.time())
  value <- date$wday
  value <- bitwShiftL(value, 9)
  value <- value + date$year
  value <- bitwShiftL(value, 4)
  value <- value + date$mon + 1
  value <- bitwShiftL(value, 5)
  value <- value + date$mday
  value <- bitwShiftL(value, 5)
  value <- value + date$hour
  value <- bitwShiftL(value, 6)
  value <- value + date$min

  value
}

comments <- function(object, id)
{
  idx <- index(object, id)
  atntime <- format(Sys.time(), "\\yr%Y\\mo%m\\dy%d\\hr%H\\min%M\\sec%S")
  i <- idx
#  sapply(idx, function(i){
    paste0(
    "{",
      "{\\*\\atrfend 1}",
      "{",
        "{\\*\\atnid ", i['key'], "}",
        "{\\*\\atnauthor ", Sys.getenv("USER"), "}",
        "\\chatn{\\*\\annotation",
          "{\\*\\atnref 1}",
          "{\\*\\atndate ", dttm_datetime(), "}",
          "{", i['key'], " tangram ", i['src'], "}",
        "}",
      "}",
    "}"
    )
#  })
}

#' Convert an abstract cell_fstat object into an RTF string
#'
#' Given a cell_fstat class create an RTF representation.
#'
#' @param object The cell fstat to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given fstat as a <td> with several <span>'s.
#' @export
#'
rtf.cell_fstat <- function(object, id, ...)
{
  ref <- if(is.na(object$reference)) "" else paste0("{\\super ", object$reference, "}")
  idx <- comments(object, id)

  paste0(
    "{",
      "{\\*\\atrfstart 1}",
      "F{\\sub ",
      object$n1,",",object$n2, "}=",
      render_f(object$f, object$format),
      ", P=",
      render_f(object$p, object$format),
    "}",
    idx,
    ref
  )
}

#' Given a cell class create an RTF representation.
#'
#' @param object The cell to render to RTF
#' @param id A unique identifier for traceability
#' @param ... additional arguments to renderer. Unused
#' @return An RTF string rendering of the given cell.
#' @export
#'
rtf.cell <- function(object, id, ...) ""

est_column_widths <- function(object)
{
  nrows <- rows(object)
  ncols <- cols(object)

  lens <- sapply(1:ncols, FUN=function(col) {
    max(sapply(1:nrows, FUN=function(row) {
      nchar(summary(object[[row]][[col]]))
    }))
  })

  c(0, pmax(lens/16, rep(0.4, ncols)))
}

#' S3 rtf Method function for use on abstract table class
#'
#' @param object The cell to render to RTF
#' @param id A unique identifier for the table (strongly recommended). If not provided, caption will be used.
#' @param ... additional arguments to renderer.
#' @export
#'
rtf <- function(object, id, ...)
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
#' @param point Main font point size
#' @param ... additional arguments to renderer. Unused
#' @return A text string rendering of the given table
#' @export
#'
rtf.cell_table <- function(
  object,
  id       = NA,
  caption  = NA,
  fragment = FALSE,
  widths   = NA,
  footnote = NA,
  filename = NA,
  append   = FALSE,
  point    = 9,
  ...)
{
  if(is.na(id))
  {
    warning("No id specified for later traceability of table elements")
    id <- ""
  }

  # Scale by inverse of conway's constant
  widths <- round((point/9)^(0.7671241)*1440*cumsum(if(is.na(widths)) est_column_widths(object) else widths))

  header <- if(fragment) "" else
    paste0(
      "{\\rtf1 \\ansi \\deff0",
      "{\\fonttbl",
      "  {\\f0\\froman\\fprq2\\fcharset0 Liberation Serif{\\*\\falt Times New Roman};}",
      "}",
      "\\paperw",min(round(tail(widths,1)*1440)+720, 15840),
      "\\paperh12240",
      "\\margl720\\margr720\\margt1440\\margb1440",
      "\\landscape"
    )

  caption <- if(is.na(caption)) "" else
    paste0("{\\pard\\b\\fs",round(point*2.4)," ",caption,"\\par}\n")

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
  cellopen  <- paste0("\\pard\\qc\\intbl\\fs", round(point*2), " ")
  cellclose <- "\\cell\n"

  trailer <- if(fragment) "" else "}"

  nrows <- rows(object)
  ncols <- cols(object)
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  # Render it all
  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- rtf(object[[row]][[col]], id=id, point=point)
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

  result <- paste0(header, caption, pasty, trailer, collapse='')

  if(!is.na(filename)) cat(result, file=filename, append=append)

  result
}