### Make up data
n <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))

ast <- Parser$new()$run(event ~ id)$reduce(df)
row <- ast$right
column <- ast$left

### Now create custom function for counting events with a category
summarize_count <- function(row, column)
{
  ### Getting Data
  datar <- row$data[,1]
  datac <- column$data[,1]

  ### Grabbing categories
  col_categories <- levels(datac)

  # 1 X (M+2)
  m <- length(col_categories)
  tbl <- tg_table(1, m+2)

  tbl[[1]][[1]] <- tg_n(length(unique(datar)), src=key(row, column, "N"))

  # The quantiles by category
  y <- rep(NA, length(col_categories))
  n_labels <- lapply(1:length(col_categories), FUN=function(category) {
    cat_name <- col_categories[category]
    x <- datar[datac == cat_name]
    xx <- aggregate(x, by=list(x), FUN=length)$x

    tbl[[1]][[category+1]] <<- tg_quantile(xx, row$format, src=key(row, column, subcol=cat_name))

    tg_n(length(unique(x)), src=key(row, column, "N", subcol=cat_name))
  })

  # The statistical test
  test <- summary(aov(glm(x ~ treatment, aggregate(datar, by=list(id=datar, treatment=datac), FUN=length), family=poisson)))[[1]]

  # The results
  tbl[[1]][[m+2]] <- tg_fstat(f   = tg_format("%.2f", test$'F value'[1]),
                              n1  = test$Df[1],
                              n2  = test$Df[2],
                              p   = tg_format("%1.3f", test$'Pr(>F)'[1]),
                              src = key(row, column, "F"))

  # Row / Column Headers
  row_header(tbl) <- derive_label(row)
  col_header(tbl) <- list(c("N", col_categories, "Test Statistic"),
                          c(NA,  n_labels,       NA)              )

  tbl
}

tbl <- summary_table(event ~ id, df, summarize_count)

tbl















> tbl <- summary_table(event ~ id,df, counter)
> tbl
======================================================================================
     N          A                 B                 C              Test Statistic
             (N=246)           (N=245)           (N=247)
--------------------------------------------------------------------------------------
id  250  3.00 *4.00* 5.00  3.00 *4.00* 5.00  3.00 *4.00* 5.00  F_{2,735}=0.02, P=0.981
======================================================================================
> # Contrast with
> summaryM(id ~ event, data=df, test=TRUE)


Descriptive Statistics  (N=3000)

+--+-----------------------+-----------------------+-----------------------+--------------------------+
|  |A                      |B                      |C                      |  Test                    |
|  |(N=1000)               |(N=1000)               |(N=1000)               |Statistic                 |
+--+-----------------------+-----------------------+-----------------------+--------------------------+
|id|    69.00/127.00/192.00|    56.75/122.00/190.00|    66.75/127.00/186.00|F=1.93 d.f.=2,2997 P=0.146|
+--+-----------------------+-----------------------+-----------------------+--------------------------+
