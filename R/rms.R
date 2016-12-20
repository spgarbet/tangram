
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

extract_data_set_labels <- function(object.anova, data.set, short.labels)
{
  label.data <- NULL

  object.anova <- object.anova[1:(nrow(object.anova)-4),]

  ## Create indicator for whether shortened labels will be used; if so, create new column with
  ## original labels, replaced with short versions as indicated by names of short.labels
  use.short <- FALSE
  if(!is.null(data.set)){
    if('Labels' %in% names(contents(data.set)$contents)){
      label.data <- data.frame(variable = names(data.set),
                               varlabel = as.vector(contents(data.set)$contents$Labels),
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
}


take1_summary_rms <- function(object.model,
                        data.set = NULL,
                        short.labels = NULL,
                        rm.rows = c('nl.int', 'nl', 'int', 'none'))
{
  # Check Arguments
  if(!('rms' %in% class(model.obj))){
    stop('model.obj must be of class rms')
  } else if(!(is.null(data.set) | 'data.frame' %in% class(data.set))){
    stop('data.set must be of class data.frame')
  }

  ## -- General setup ----------------------------------------------------------------------------
  ## Which rows (if any) to remove from final?
  rm.rows <- match.arg(rm.rows)

  ## These model classes will have ratios presented, not beta coefficients
  use.ratios <- c('lrm', 'cph')

  ######## Create Summaries
  ## -- summary.rms() results --------------------------------------------------------------------
  mod.sum <- as.data.frame(summary(object.model)) ## Often won't print due to duplicate rownames

  ## Get variable name in every row
  mod.sum$quantity  <- rownames(mod.sum)
  rownames(mod.sum) <- NULL

  ## Create variable column ##
  ## Models for which summary() produces both coefficients and ratios: Take only ratios, variable
  ## column = row above ratio row
  if(sum(!is.na(match(use.ratios, class(object.model)))) > 0){
    mod.sum$var <- c(NA, mod.sum$quantity[1:(nrow(mod.sum) - 1)])
    mod.sum <- subset(mod.sum, Type == 2)
  } else{
    mod.sum$var <- mod.sum$quantity

    ## If model is a Poisson model, exponentiate all point estimates, CLs to get IRRs
    if('Glm' %in% class(object.model)){
      if(object.model$family$family == 'poisson'){
        mod.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')] <-
          exp(mod.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')])
      }
    }
  }

  ## For categorical covariates, var column currently is of format
  ##  "variable name - comparison category:reference category"; split out components
  var.split <- lapply(mod.sum$var, FUN = function(x){ strsplit(x, split = ' - |:')[[1]] })
  mod.sum$var <- unlist(lapply(var.split, FUN = function(x){ x[1] }))

  ## Create new low/high variables with categories (not factor numbers) or formatted numbers
  mod.sum$low.char  <- unlist(lapply(var.split, FUN = function(x){ x[3] }))
  mod.sum$low.char  <- with(mod.sum, ifelse(is.na(low.char), Low, as.character(low.char)))
  mod.sum$high.char <- unlist(lapply(var.split, FUN = function(x){ x[2] }))
  mod.sum$high.char <- with(mod.sum, ifelse(is.na(high.char), High, as.character(high.car)))

  ## -- anova.rms() results ----------------------------------------------------------------------
  ## Create data frame of anova() results, add column to indicate variable/quantity
  mod.anova <- as.data.frame(anova(object.model))
  mod.anova$line <- rownames(mod.anova); rownames(mod.anova) <- NULL
  mod.anova$var <- gsub('^ ', '', gsub(' +\\(.*\\)$| : .*$', '', mod.anova$line))

  ## Remove rows requested in rm.rows
  if(rm.rows != 'none'){
    remove.these <- NULL
    if(rm.rows %in% c('nl', 'nl.int')){
      remove.these <- c(remove.these, '^ *Nonlinear', '^ *f\\(A,B\\)')
    }
    if(rm.rows %in% c('int', 'nl.int')){
      remove.these <- c(remove.these, '^ *All Interactions', '^ *Nonlinear Interaction')
    }

    mod.anova <- mod.anova[-unique(unlist(sapply(remove.these, grep, mod.anova[,'var']))),]
  }

  ## For each row, create unique var value that is as descriptive as possible
  cur.var <- NULL
  for(i in 1:nrow(mod.anova)){
    if((!is.null(data.set) & mod.anova$var[i] %in% names(data.set)) |
        length(grep('*', mod.anova$var[i], fixed = TRUE)) > 0){
      cur.var <- mod.anova$var[i]
    } else{
      if(length(grep('^TOTAL|ERROR', mod.anova$var[i])) == 0){
        mod.anova$var[i] <- ifelse(is.null(cur.var),
          gsub('^All ', '', mod.anova$var[i]),
          paste(cur.var, gsub('^All ', '', mod.anova$var[i])))
      }
    }
  }

  ## Create count variable for each line for a given quantity
  ##  (eg, multiple levels of categorical covariate)
  mod.anova$var.line <- unlist(lapply(unique(mod.anova$var),
    FUN = function(x){1:nrow(subset(mod.anova, var == x))}))

  ## -- Variable labels --------------------------------------------------------------------------
  ## If data.set is NULL or has no labels, use variable names
  label.data <- extract_data_set_labels(mod.anova, data.set, short.labels)

  # Interface results to TG
  tbl    <- cell_table(length(label.data)+2, 2,FALSE)

}

rms_variable <- function(model.sum,
                         model.anova,
                         var)
{
  varname <- var # Simplification for now, need full variable name
  results <- model.anova[var,]

  tbl <- new_table_builder(NA, NA)                            %>%
  col_header("Low", "High", "Est CI", "Test Statistic")       %>%
  row_header(varname)

  tbl <- if(var %in% rownames(model.sum))
  {
    tbl                                                                           %>%
    add_col(cell_estimate(model.sum[var, 'Low'], src=paste(var, ":Low", sep=''))) %>%
    add_col(cell_estimate(model.sum[var, 'High'],src=paste(var, ":High",sep=''))) %>%
    add_col(cell_estimate(model.sum[var, 'Effect'],
      low= model.sum[var,'Lower 0.95'],
      high=model.sum[var,'Upper 0.95'],
      src=paste(var,":Effect")
    ))
  } else {
    tbl %>% add_col(NA, NA, NA)
  }

  tbl %>%
  add_col(cell_fstat(f   = form(results['F'], "%.2f"),
                     n1  = results['d.f.'],
                     n2  = model.anova['ERROR','d.f.'],
                     p   = form(results['P'], "%1.3f"),
                     src = paste(var,":Test",sep='')))
}

summary_rms <- function(rms.model,
                        data.set = NULL,
                        short.labels = NULL,
                        rm.rows = c('nl.int', 'nl', 'int', 'none'),
                        footnote = NULL)
{
  # Check Arguments
  if(!('rms' %in% class(rms.model))){
    stop('rms.model must be of class rms')
  } else if(!(is.null(data.set) | 'data.frame' %in% class(data.set))){
    stop('data.set must be of class data.frame')
  }

  vars         <- rms.model$Design$name

  #######################
  # Compute Model summaries for variables
  model.sum    <- summary(rms.model)

  ## Models for which summary() produces both coefficients and ratios: Take only ratios, variable
  ## column = row above ratio row
  if(sum(!is.na(match(use.ratios, class(rms.model)))) > 0){
    quantity <- rownames(model.sum)
    rownames(model.sum) <- c(NA, quantity[1:(nrow(model.sum)-1)])
    model.sum <- subset(model.sum, Type == 2)
  }

  ## If model is a Poisson model, exponentiate all point estimates, CLs to get IRRs
  if('Glm' %in% class(rms.model)) {
    if(rms.model$family$family == 'poisson') {
      model.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')] <-
        exp(model.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')])
    }
  }

  #############################
  # Deal with Anova model
  model.anova <- anova(rms.model)
  quantities <- gsub('^ ', '', gsub(' +\\(.*\\)$| : .*$', '', rownames(model.anova)))
  rownames(model.anova) <- quantities

  #############################
  # Commence building Table
  master_table <- cell_table(length(vars), 1, FALSE)
  for(row in 1:length(vars))
  {
    var <- vars[row]
    master_table[[row]][[1]] <- rms_variable(model.sum, model.anova, var)$table
  }

  flat <- table_flatten(master_table$table)

  if(!is.null(footnote)) attr(flat, "footnote") <- footnote

  flat
}
