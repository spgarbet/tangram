#' @importFrom stringi stri_trans_nfc
#' @importFrom stringi stri_trans_nfd
#' @importFrom utils capture.output
iify <- Vectorize(function(x, sub_table)
{
  y <- as.character(x)                        # Make sure a character string was passed
  if(is.null(x) || nchar(y) == 0) return("")  # Abort early for zero characters

  ## Kludge for converting from "byte" to the current encoding
  ## in a way which preserves the hex notation.
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) y[encBytes] <- capture.output(cat(y[encBytes], sep = "\n"))

  ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
  ## processing of accented characters. Doing this early to
  ## circumvent pecularities in gsub() (and nchar()) when working in
  ## the C locale.
  y <- stri_trans_nfd(y)

  ## Run all conversions as appropriate not inside "$"
  pieces  <- strsplit(y, "(?<!\\\\)\\$", perl=TRUE)[[1]]
  for(i in 1:length(pieces))
  {
    if((i %% 2) != 0)
    {
      for (subst in sub_table) pieces[i] <- gsub(subst[1], subst[2], pieces[i], perl = TRUE)
    }
  }
  y <- paste0(pieces, collapse="")

  ## Convert result to UTF-8 NFC encoding, although most non-ASCII
  ## content has probably been converted to LaTeX commands.
  stri_trans_nfc(y)
}, vectorize.args="x")
