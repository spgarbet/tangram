#' @rdname tangram
#' @export
tangram.clmm2 <- function(x, id=NULL, style="hmisc", caption=NULL, footnote=NULL, digits=NULL, ...)
{
  tangram.summary.clmm2(summary(x), id, style, caption, footnote, digits, ...)
}

#' @rdname tangram
#' @importFrom stats naprint
#' @export
tangram.summary.clmm2 <- function(x, id=NULL, style="hmisc", caption=NULL, footnote=NULL, digits=NULL, pformat="%1.3f", include_p=FALSE, ...)
{
  if(is.null(digits))  digits  <- 3
  if(is.null(pformat)) pformat <- hmisc_p

  nxi <- length(x$xi)   # Number of Threshold coefficients  @ 1:nxi
  p   <- length(x$beta) # Number of Location coefficients   @ (nxi+1):(nxi+p)
  k   <- length(x$zeta) # Number of Scale coefficients      @ (nxi+p+1):(nxi+p+k)
  u   <- x$estimLambda  # Number of Link coefficients       @ (nxi+p+k+1):(nxi+p+k+u)

  # All coefficients in a table
  coef <- tangram(x$coefficients, digits=digits, id=id, style=style, caption=caption, footnote=footnote, ...) %>%
    set_style(style)      %>%
    set_caption(caption)  %>%
    set_footnote(footnote)

  # Fix up the names and p-values
  for(i in 2:length(coef))
  {
    coef[[i]][[1]] <- cell_subheader(paste0("  ", coef[[i]][[1]]))
    coef[[i]][[5]] <- cell(hmisc_p(x$coefficients[i-1,4], pformat, include_p=include_p))
  }

  threshold  <- if(nxi > 0)
  {
    select_row(coef, 2:(nxi+1)) %>%
    insert_row(0, cell_header("Threshold coefficients",  colspan=5), NA, NA, NA, NA)
  } else
  {
    insert_row(coef,  0, cell_header("No threshold coefficients",  colspan=5), NA, NA, NA, NA) %>%
    select_row(1)
  }

  location  <- if(p > 0)
  {
    select_row(coef, c(1,1+(nxi+1):(nxi+p))) %>%
    insert_row(1, cell_header("Location coefficients"), NA, NA, NA, NA)
  } else
  {
    insert_row(coef,  0, cell_header("No location coefficients"), NA, NA, NA, NA) %>%
    select_row(1)
  }

  scale     <- if(k > 0)
  {
    select_row(coef, 1+(nxi+p+1):(nxi+p+k)) %>%
    insert_row(0, cell_header("Scale coefficients"), NA, NA, NA, NA)
  } else
  {
    insert_row(coef,  0, cell_header("No scale coefficients"), NA, NA, NA, NA) %>%
    select_row(1)
  }

  link      <- if(u > 0)
  {
    select_row(coef, 1+(nxi+p+k+1):(nxi+p+k+u)) %>%
    insert_row(0, cell_header("Link coefficients"), NA, NA, NA, NA)
  } else
  {
    insert_row(coef,  0, cell_header("No link coefficients"), NA, NA, NA, NA) %>%
    select_row(1)
  }

  tbl <- (location+scale+link+threshold) %>%
  home() %>%
  new_row() %>% add_col("", "", "", "", "")

  if(length(x$stDev) > 0)
  {
    tbl <- tbl  %>%
      new_row() %>%
      add_col(cell_header("Random effects"), cell_subheader("Var"), cell_subheader("Std Dev"), "", "")

    for(i in dim(x$varMat)[1])
    {
      tbl <- new_row(tbl) %>%
             add_col(cell_subheader(paste0("  ", rownames(x$varMat)[i])),
                     render_f(x$varMat[i,1], digits),
                     render_f(x$varMat[i,2], digits),
                     "",
                     "")
    }
  } else {
    tbl <- new_row(tbl) %>% add_col(cell_header("No random effects"),"","","","")
  }

  tbl %>%
  new_row() %>% add_col("", "", "", "", "") %>%
  new_row() %>% add_col("Log-likelihood", render_f(x$logLik, digits), "", "", "") %>%
  new_row() %>% add_col("AIC", render_f(-2*x$logLik + 2*x$edf, digits), "", "", "") %>%
  new_row() %>% add_col("Condition", render_f(x$condHess, digits), "", "", "") %>%
  new_row() %>% add_col(cell(naprint(x$na.action), colspan=5), NA, NA, NA, NA)
}
