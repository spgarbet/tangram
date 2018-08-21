## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tangram)

pbc <- tangram::pbc

# This is necessary for changing size of chunks in LaTeX. Why isn't this patched in knitr?
# https://stackoverflow.com/questions/25646333/code-chunk-font-size-in-rmarkdown-with-knitr-and-latex/46526740
# ?highlight
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

## ----pbc, comment="", size="scriptsize"----------------------------------
tbl1 <- tangram("drug~bili+albumin+stage::Categorical+protime+sex+age+spiders",
        pbc, id="joe", caption="Context Aware Compilation")
summary(tbl1)

## ---- results="asis"-----------------------------------------------------
x <- tangram("drug~bili[2]+albumin+stage::Categorical+protime+sex+age+spiders",
        pbc,
        id="tbl2",
        msd=TRUE,
        quant=seq(0, 1, 0.25),
        style="hmisc",
        caption = "Table Rmd Style",
        pct_width=0.6)
rmd(x)

## ---- results="asis"-----------------------------------------------------
tangram("drug~bili[2]+albumin+stage::Categorical+protime+sex+age+spiders",
        pbc,
        id="tbl2",
        msd=TRUE,
        quant=seq(0, 1, 0.25),
        style="hmisc",
        caption = "Table Hmisc Style",
        pct_width=0.6)

## ---- results="asis"-----------------------------------------------------
tangram("drug~bili[2]+albumin+stage::Categorical+protime+sex+age+spiders", pbc,
        style="nejm", caption = "Table NEJM Style", id="tbl3",
        pct_width=0.8)

## ---- results="asis"-----------------------------------------------------
tangram('drug~bili["%4.03f"]+albumin+stage::Categorical[1]+protime+sex[1]+age+spiders[1]', 
        id="tbl4",
        data=pbc,
        pformat = 5,
        style="lancet",
        caption = "Table Lancet Style",
        pct_width = 0.8
       )

## ---- comment=""---------------------------------------------------------
index(tangram("drug ~ bili + albumin + stage::Categorical + protime + sex + age + spiders",
              pbc, id="tbl2"))[1:20,]

## ---- results='asis'-----------------------------------------------------
x <- round(rnorm(375, 79, 10))
y <- round(rnorm(375, 80,  9))
y[rbinom(375, 1, prob=0.05)] <- NA
attr(x, "label") <- "Global score, 3m"
attr(y, "label") <- "Global score, 12m"
tangram(1 ~ x+y,
        data.frame(x=x, y=y),
        style="hmisc", caption="Intercept Example", id="tbl5") %>%
del_row(2) %>% del_col(4)

## ---- results='asis'-----------------------------------------------------
d1 <- iris
d1$A <- d1$Sepal.Length > 5.1
attr(d1$A,"label") <- "Sepal Length > 5.1"
tangram(
   Species + 1 ~ A + Sepal.Width,
   data = d1,
   id="tbl6",
   style="nejm",
   caption="Example All Summary"
) %>%
drop_statistics() %>%
del_col(6)

## ----extension, results='asis'-------------------------------------------

### Make up some data, which has events nested within an id
n  <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))
attr(df$id, "label") <- "ID"

### Now create custom function for counting events with a category
summarize_count <- function(table, row, column, ...)
{
  ### Getting Data for row column ast nodes, assuming no factors
  datar <- row$data
  datac <- column$data

  ### Grabbing categories
  col_categories <- levels(datac)

  n_labels <- lapply(col_categories, FUN=function(cat_name){
    x <- datar[datac == cat_name]
    cell_n(length(unique(x)), subcol=cat_name)
  })

  # Test a poisson model
  test <- summary(aov(glm(x ~ treatment,
                      aggregate(datar, by=list(id=datar, treatment=datac), FUN=length),
                      family=poisson)))[[1]]
  test <- hmisc_fstat(f = render_f(test$'F value'[1], "%.2f"),
                      df1 = test$Df[1], df2 = test$Df[2],
                      p = hmisc_p(test$'Pr(>F)'[1]))
  # Build the table
  table                                              %>%
  # Create Headers
  row_header(derive_label(row))                      %>%
  col_header("N", col_categories, "Test Statistic")  %>%
  col_header("",  n_labels,       ""              )  %>%
  # Add the First column of summary data as an N value
  add_col(cell_n(length(unique(datar))))             %>%
  # Now add quantiles for the counts
  table_builder_apply(col_categories, FUN=
    function(tbl, cat_name) {
      # Compute each data set
      x  <- datar[datac == cat_name]
      xx <- aggregate(x, by=list(x), FUN=length)$x

      # Add a column that is a quantile
      add_col(tbl, hmisc_iqr(xx, row$format, na.rm=TRUE))
  })                                                 %>%
  # Now add a statistical test for the final column
  add_col(test)
}

tangram(event ~ id["%1.0f"], df, id="tbl7", transforms=summarize_count)

