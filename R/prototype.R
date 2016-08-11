# For test example
library(Hmisc)
getHdata(pbc)


f <- formula("drug ~ bili + albumin + stage + protime + sex + age + spiders")
model.matrix(f, data=pbc)

row0 <- pbc[FALSE,]
model.matrix(f, data=row0)


# S4
setClass("Fraction", slots=list(numerator="numeric", denominator="numeric"))

# Objects to return from functions
f <- new("Fraction", numerator=numerator, denominator=denominator)

setMethod("rmarkdown",
          "Fraction",
          function(x) { paste(round(100*x@numerator/x@denominator),
          "% $$\\frac{", x@numerator,
          "}{", x@denominator,"}$$", sep="")
          }
)

summary.fraction <- function(x)
{
   paste(round(100*x$numerator/x$denominator)),
         " ", 
         x$numerator,
         "/",
         x$denominator, sep="")
}

ftest <- function(n1, n2, fstat, pvalue)
{
    structure(list(n1=n1, n2=n2, fstat=fstat, pvalue=pvalue),
              class="ftest")
}

rmarkdown.ftest <- function(x)
{
  paste("$$F_{", x$n1, ",", x$n2, "} = ", 
        round(x$fstat, 2),
        " P = ",
        round(x$pvalue, 3),
        "$$",   
        sep=""
  )
}
summary.ftest <- function(x)
{
  paste("F_{", x$n1, ",", x$n2, "} = ", 
        round(x$fstat, 2),
        " P = ",
        round(x$pvalue, 3),
        "$$",  
        sep=""
  )
}


# Defaults from Hmisc
pearson <- function()
{
}

kruskal <- function()
{
}


summaryTable <- function(
   formula,
   data,
   FUN=list(categorical=pearson, numerical=kruskal) )
{


}



tbl <- summaryTable(drug ~ bili + albumin + sex, data=pbc)


