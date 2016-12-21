setwd("~/Projects/tg")

data(pbc)

library(Hmisc)
library(stringr)
library(R6)
library(dplyr)
devtools::load_all()

label(pbc$bili) <- "Serum Bilirubin"
units(pbc$bili) <- "mg/dl"

summary_table(1 ~ stage::Categorical, pbc)
summary_table(stage::Categorical ~ 1, pbc)

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

### Make up data
n  <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))
label(df$id) <- "ID"

### Now create custom function for counting events with a category
summarize_count <- function(row, column)
{
  ### Getting Data for row column ast nodes, assuming no factors
  datar <- row$data[,1]
  datac <- column$data[,1]

  ### Grabbing categories
  col_categories <- levels(datac)

  # 1 X (M+2)
  m <- length(col_categories)
  tbl <- tg_table(1, m+2)

  # Create N value in upper left corner
  tbl[[1]][[1]] <- tg_n(length(unique(datar)), src=key(row, column, "N"))

  # The quantiles by category
  n_labels <- lapply(1:length(col_categories), FUN=function(category) {
    cat_name <- col_categories[category]
    x <- datar[datac == cat_name]
    xx <- aggregate(x, by=list(x), FUN=length)$x

    # Fill in the quantiles shifted one to the right
    tbl[[1]][[category+1]] <<- tg_quantile(
        tg_format(row$format, quantile(xx, na.rm=TRUE)),
        src=key(row, column, subcol=cat_name))

    # For the sub header of, the columns
    tg_n(length(unique(x)), src=key(row, column, "N", subcol=cat_name))
  })

  # Test a poisson model
  test <- summary(aov(glm(x ~ treatment, aggregate(datar, by=list(id=datar, treatment=datac), FUN=length), family=poisson)))[[1]]

  # Create the f statistics column, in the last far right place
  tbl[[1]][[m+2]] <- tg_fstat(f   = tg_format("%.2f", test$'F value'[1]),
                              n1  = test$Df[1],
                              n2  = test$Df[2],
                              p   = tg_format("%1.3f", test$'Pr(>F)'[1]),
                              src = key(row, column, "F"))

# Create the headers
  row_header(tbl) <- derive_label(row)
  col_header(tbl) <- list(c("N", col_categories, "Test Statistic"),
                          c(NA,  n_labels,       NA)              )

  tbl
}

summary_table(event ~ id["%1.0f"],df, summarize_count)

# Contrast with
summaryM(id ~ event, data=df, test=TRUE)
