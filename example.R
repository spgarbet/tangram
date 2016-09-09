setwd("~/Projects/tg")

data(pbc)

library(Hmisc)
library(stringr)
library(R6)
library(dplyr)
devtools::load_all()



#f <- formula(albumin ~ age)
#f <- formula(drug ~ stage::Categorical)
#f <- formula(drug + log(age) ~ bili + albumin + stage::Categorical + protime + sex + age + spiders)
f <- formula(drug ~ bili + albumin + stage::Categorical + protime + sex + age + spiders)

#f <- formula(drug ~ log(albumin))
test_table <- summary_table(f, pbc)

#lbl_stage <- label(pbc["stage"])
#pbc$stage <- factor(pbc$stage, levels=1:4, ordered=TRUE) # Make a factor, instead of guessing
#label(pbc$stage) <- lbl_stage

summary_table(drug ~ age[5], pbc)


f <- formula(drug ~ bili)
test_table <- summary_table(f, pbc)
index(test_table)

x <- html5(test_table, caption="Table 9: Descriptive Statistics by drug", css="nejm.css");
write(x, "test-nejm.html")

x <- html5(test_table, caption="Table 9: Descriptive Statistics by drug", css="hmisc.css");
write(x, "test-hmisc.html")

# TODO
#index(table)
#latex(table)

# Try nesting...

### Make up data
n <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))

key <- function(row, col, label, subrow=NA, subcol=NA)
{
  rv <- if(is.na(subrow)) row$value else paste(row$value, '[',subrow,']',sep='')
  cv <- if(is.na(subcol)) col$value else paste(col$value, '[',subcol,']',sep='')
  paste(rv,":",cv,":",label,sep='')
}

### Now create custom function
summarize_count <- function(row, column)
{
  ### Getting Data
    datar <- row$data[,1]
    datac <- column$data[,1]

### Grabbing categories
  col_categories <- levels(datac)

  n <- 1
  m <- length(col_categories)

############# LABELLING-- SIMPLFY
  # Label for the table cell
  row_lbl <- tg_table(1, 1)
  row_lbl[[1]][[1]] <- derive_label(row)

  col_lbl <- tg_table(2, 2+length(col_categories))
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[length(col_categories)+2]] <- tg_header("Test Statistic")

  # 1 X (M+2)
  tbl <- tg_table(1, m+2, TRUE)


#### Needs to be an tg_n column type!!!
  N <- length(unique(datar))

  tbl[[1]][[1]] <- tg_label(as.character(N),
    src=paste(row$value, ":", column$value,":N",sep=''))

#### This is the real thing
  # The quantiles by category
  y <- rep(NA, length(col_categories))
  sapply(1:length(col_categories), FUN=function(category) {
    cat_name <- col_categories[category]
    x <- datar[datac == col_categories[category]]
    xx <- aggregate(x, by=list(x), FUN=length)$x

    tbl[[1]][[category+1]] <<- tg_quantile(
        tg_format(row$format, quantile(xx, na.rm=TRUE)),
        src=key(row, column, 'quant', subcol=cat_name))
### Labeling AGAIN
    col_lbl[[1]][[category+1]] <<- tg_header(cat_name)
    col_lbl[[2]][[category+1]] <<- tg_subheader(paste("N=",length(unique(x)),sep=''),
        src=key(row, column, "N", subcol=cat_name))
  })

  test <- summary(aov(glm(x ~ treatment, aggregate(datar, by=list(id=datar, treatment=datac), FUN=length), family=poisson)))[[1]]

  tbl[[1]][[m+2]] <- tg_fstat(f   = tg_format("%.2f", test$'F value'[1]),
                              n1  = test$Df[1],
                              n2  = test$Df[2],
                              p   = tg_format("%1.3f", test$'Pr(>F)'[1]),
                              src = key(row, column, "F"))

##### LABELING SIMPLFY
  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}


counter <- hmisc_style <- list(
  Type = function(x) {"Data"},
  Data = list(
    Data = summarize_count
  )
)

tbl <- summary_table(f, df, counter)

tbl
