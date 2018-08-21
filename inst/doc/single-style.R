## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tangram)
pbc <- tangram::pbc

## ----style, results='asis'-----------------------------------------------
cat(custom_css("lancet.css"))

## ------------------------------------------------------------------------
head(pbc)

## ---- results="asis"-----------------------------------------------------
tangram("drug ~ bili[2] + albumin + stage::Categorical + protime + sex + age + spiders",
        pbc, "tbl2", caption="Hmisc::PBC")

## ---- results='asis'-----------------------------------------------------
set.seed(1234)
x <- round(rnorm(375, 79, 10))
y <- round(rnorm(375, 80,  9))
y[rbinom(375, 1, prob=0.05)] <- NA
attr(x, "label") <- "Global score, 3m"
attr(y, "label") <- "Global score, 12m"
tangram(1 ~ x+y,
        data.frame(x=x, y=y),
        after=hmisc_intercept_cleanup,
        caption="Intercept", id="tbl5")

