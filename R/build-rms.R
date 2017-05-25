#' @include S3-Cell.R
#' @include table_builder.R
#' @include main.R
#'
#' @importFrom stats anova
#'
## Special thanks to Jennifer Thompson for providing the original code for this
## Function to determine label value given a string
create.label <- function(v, label.data, data.set){
  ## For rows involving interactions, replace variable names with labels (if using) and keep
  ##  all other default descriptions (eg, Nonlinear Interactions, f(A,B) vs. Af(B) + Bg(A))
  int.label.str <- NULL

  if(length(grep('*', v, fixed = TRUE)) > 0){
    ## Separate all pieces of interaction description; replace variable names with labels
    int.terms <- unlist(lapply(strsplit(v, ' ')[[1]], FUN = function(x){
      if(x %in% names(data.set)){
        ifelse(x %in% label.data[,'variable'],
          label.data[match(x, label.data$variable), 'intlabel'],
          x)
      } else{
        x
      }
    }))

    ## Recombine all elements of interaction row description
    int.label.str <- paste(int.terms, collapse = ' ')
  }

  ifelse(!is.null(int.label.str), int.label.str,
    ifelse(!is.null(label.data) & v %in% label.data$variable,
      label.data$varlabel[match(v, label.data$variable)],
      v))
}

extract_label_data <- function(object.anova, data.set, short.labels)
{
  label.data <- NULL

  object.anova <- as.data.frame(object.anova)[1:(nrow(object.anova)-4),]

  ## Create indicator for whether shortened labels will be used; if so, create new column with
  ## original labels, replaced with short versions as indicated by names of short.labels
  use.short <- FALSE
  if(!is.null(data.set)){
    if('Labels' %in% names(Hmisc::contents(data.set)$contents)){
      label.data <- data.frame(variable = names(data.set),
                               varlabel = as.vector(Hmisc::contents(data.set)$contents$Labels),
                               stringsAsFactors = FALSE)

      ## Remove and warn of any elements of short.labels that aren't in names(data.set)
      if(length(setdiff(names(short.labels), names(data.set))) > 0){
        message(paste("Note: these elements of short.labels do not appear in names(data.set):",
                      paste(setdiff(names(short.labels), names(data.set)), collapse = ', ')))
      }

      short.labels <- short.labels[names(short.labels) %in% names(data.set)]
      if(length(short.labels) > 0){
        use.short <- TRUE

        ## All short labels are the same as variable labels by default
        label.data$shortlabel <- label.data$varlabel

        ## Replace desired variable labels with short versions
        for(i in 1:length(short.labels)){
          varnum <- match(names(short.labels)[i], label.data$variable)
          if(!is.na(varnum)){
            label.data$shortlabel[varnum] <- short.labels[i]
          }
        }
      }
    }
  }
  # Create a third column for interaction variable use
  label.data$intlabel <- if(use.short) label.data$shortlabel else label.data$varlabel

  unlist(lapply(1:nrow(object.anova), FUN = function(i){
    ifelse(object.anova$var.line[i] != 1, '', create.label(object.anova$var[i], label.data, data.set))
  }))

  label.data
}

rms_variable <- function(model.sum,
                         model.anova,
                         model.name,
                         var,
                         varname,
                         rnd.digits,
                         lowhigh)
{
  results <- model.anova[var,]

  tbl <- new_table_builder(NA, NA)

  tbl <- if(lowhigh) {
           col_header(tbl, "", "", model.name, "") %>%
           col_header(     "Low", "High", "Est CI", "Test Statistic")
         } else {
           col_header(tbl, model.name, "") %>%
           col_header(    "Est CI", "Test Statistic")
         }

  tbl <- row_header(tbl, varname)

  if(lowhigh)
  {
    tbl <- if(var %in% rownames(model.sum)){
      tbl                                                                           %>%
      add_col(cell_estimate(form(model.sum[var, 'Low'], rnd.digits),
                            src=paste(var, ":Low", sep='')))                        %>%
      add_col(cell_estimate(form(model.sum[var, 'High'], rnd.digits),
                            src=paste(var, ":High",sep='')))
    } else {
      add_col(tbl, "", "")
    }
  }

  tbl <- if(var %in% rownames(model.sum))
  {
    add_col(tbl, cell_estimate(form(model.sum[var, 'Effect'], rnd.digits),
      low= form(model.sum[var,'Lower 0.95'], rnd.digits),
      high=form(model.sum[var,'Upper 0.95'], rnd.digits),
      src=paste(var,":Effect")
    ))
  } else {
    add_col(tbl, "")
  }

  add_col(tbl,
    cell_fstat(f   = form(results['F'], "%.2f"),
               n1  = results['d.f.'],
               n2  = model.anova['ERROR','d.f.'],
               p   = form(results['P'], "%1.3f"),
               src = paste(var,":Test",sep='')))
}

rms_stats <- function(model.anova, lowhigh)
{
  anit <- model.anova['TOTAL NONLINEAR + INTERACTION',]
  ant  <- model.anova['TOTAL NONLINEAR',]
  om   <- model.anova['TOTAL',]

  padding <- function(tbl) if(lowhigh) add_col(tbl, "", "", "") else tbl

  new_table_builder(NA, NA)                                   %>%
  row_header("All Nonlinear & Interaction Terms")             %>%
  padding()                                                   %>%
  add_col(cell_fstat(f   = form(anit['F'], "%.2f"),
                     n1  = anit['d.f.'],
                     n2  = model.anova['ERROR','d.f.'],
                     p   = form(anit['P'], "%1.3f"),
                     src = paste("NonlinearAndInteraction",":Test",sep='')))        %>%
  new_line()                                                  %>%
  row_header("All Nonlinear Terms", sub=FALSE)                %>%
  padding()                                                   %>%
  add_col(cell_fstat(f   = form(ant['F'], "%.2f"),
                     n1  = ant['d.f.'],
                     n2  = model.anova['ERROR','d.f.'],
                     p   = form(ant['P'], "%1.3f"),
                     src = paste("NonlinearTerms",":Test",sep='')))        %>%
  new_line()                                                  %>%
  row_header("Overall Model", sub=FALSE)                      %>%
  padding()                                                   %>%
  add_col(cell_fstat(f   = form(om['F'], "%.2f"),
                     n1  = om['d.f.'],
                     n2  = model.anova['ERROR','d.f.'],
                     p   = form(om['P'], "%1.3f"),
                     src = paste("OverallModel",":Test",sep='')))
}

rms_model_fit <- function(rms.model, rnd.stats, lowhigh)
{
  results <- rms.model$stats
  padding <- function(x) if(lowhigh) add_col(x, "", "") else x

  new_table_builder(NA, NA)                                   %>%
  row_header("Model Likelihood Ratio")                        %>%
  padding()                                                   %>%
  add_col(cell_estimate(form(results['Model L.R.'],rnd.stats),
                        src=paste('ModelLR', sep='')))        %>%
  add_col("")                                                 %>%
  new_line()                                                  %>%
  row_header("R^2", sub=FALSE)                                %>%
  padding()                                                   %>%
  add_col(cell_estimate(form(results['R2'],rnd.stats),
                        src=paste('R2', sep='')))             %>%
  add_col("")
}

#' Combine information from summary.rms(), anova.rms(), and other rms object info to create a
#' single pretty table of model results. The rms and Hmisc packages are required.
#'
#' @param rms.model Object of class rms, or list of named objects
#' @param data.set Data frame from which to get variable labels. Defaults to NULL, in which case
#' variable names will be used.
#' @param short.labels Named vector of variable labels to replace in interaction rows. Must be in
#' format c("variable name" = "shortened label").
#' @param footnote A string to add to the table as a footnote.
#' @param rnd.digits Number of digits to round reference, comparison, result and CI values to.
#' Defaults to 2.
#' @param rnd.stats Number of digits to round model LR, R2, etc to. Defaults to rnd.digits.
#' @export
summary_rms <- function(rms.model,
                        data.set = NULL,
                        short.labels = NULL,
                        footnote = NULL,
                        rnd.digits = 2,
                        rnd.stats  = rnd.digits)
{
  if(!requireNamespace("rms", quietly = TRUE)) {
        stop("tangram::summary_rms requires the rms package, please install it.",
             call. = FALSE)
    }

  # If the argument is not a list, make it so
  if(!('list' %in% class(rms.model))) rms.model <- list(rms.model)

  # Check Arguments
  if(!all(sapply(rms.model, function(x) 'rms' %in% class(x)))){
    stop('rms.model(s) must be of class rms')
  } else if(!(is.null(data.set) | 'data.frame' %in% class(data.set))){
    stop('data.set must be of class data.frame')
  }

  # Grab main variables from design
  vars         <- rms.model[[1]]$Design$name

  #######################
  # Compute Model summaries for variables
  model.sum <- lapply(1:length(rms.model), function(i) {
    m <- rms.model[[i]]
    x <- summary(m)

    ## Models for which summary() produces both coefficients and ratios: Take only ratios, variable
    ## column = row above ratio row
    ## These model classes will have ratios presented, not beta coefficients
    use.ratios <- c('lrm', 'cph')
    Type <- NULL # Eliminate CRAN Check False Positive
    if(sum(!is.na(match(use.ratios, class(m)))) > 0){
      quantity <- rownames(x)
      rownames(x) <- c(NA, quantity[1:(nrow(x)-1)])
      x <- subset(x, Type == 2)
    }

    ## If model is a Poisson model, exponentiate all point estimates, CLs to get IRRs
    if('Glm' %in% class(m)) {
      if(m$family$family == 'poisson') {
        x[,c('Effect', 'Lower 0.95', 'Upper 0.95')] <-
          exp(x[,c('Effect', 'Lower 0.95', 'Upper 0.95')])
      }
    }

    x
  })

  #############################
  # Deal with Anova model(s)
  model.anova <- lapply(1:length(rms.model), function(i) {
    m <- anova(rms.model[[i]])
    quantities <- gsub('^ ', '', gsub(' +\\(.*\\)$| : .*$', '', rownames(m)))
    rownames(m) <- quantities
    m
  })

  #############################
  # Get ready for labeling
  label.data <- extract_label_data(model.anova[[1]], data.set, short.labels)

  #############################
  # Commence building Table
  master_table <- cell_table(length(vars)+2, length(rms.model), FALSE)
  for(row in 1:length(vars))
  {
    var <- vars[row]
    label <- create.label(var, label.data, data.set)
    for(col in 1:length(rms.model))
    {
      master_table[[row]][[col]] <- rms_variable(
        model.sum[[col]], model.anova[[col]],
        names(rms.model)[[col]],
        var, label, rnd.digits,
        col==1
      )$table
    }
  }

  for(col in 1:length(rms.model))
  {
    master_table[[length(vars)+1]][[col]] <- rms_stats(model.anova[[col]], col==1)$table
    master_table[[length(vars)+2]][[col]] <- rms_model_fit(rms.model[[col]], rnd.stats, col==1)$table
  }

  flat <- table_flatten(master_table)

  if(!is.null(footnote)) attr(flat, "footnote") <- footnote

  flat
}
