## Lots of my tables are really data frames that I put together the way I want them. This one is
## the result of two model.results() calls combined (same outcome/covariates at two time points).

library(Hmisc)

load('exampledf.Rdata')

## My example data.frame is pretty wide. Thought I might use html() to enable scrolling.

html(results.rbans, scroll = TRUE) ## :( Couldn't get it to work with file = '' either.


load('exampledf.Rdata')

# Munge the data frame a bit
data <- results.rbans[,2:11]
rownames(data) <- results.rbans[,1]
data$est.ci.x <- gsub("\\\\\\n", " ", data$est.ci.x)
data$est.ci.y <- gsub("\\\\\\n", " ", data$est.ci.y)
colnames(data) <- c("Low", "High", "Est CI", "Test Statistic", "DF", "P Value", "Est CI", "Test Statistic", "DF", "P Value")

summary_frame(data, header=c("", "", "", "Group X", "", "", "", "Group Y", "", ""))

## Seems like I need to install hevea or htlatex; couldn't do this successfully and honestly if I
## have to spend that much time on it I'd rather just use pandoc.table(). Same with figuring out
## all the html() options; there are so many and I wasn't understanding the documentation.

## Wish list:
## - specify column headers
## - column/row groups like in latex() - in this example, columns 4-7 represent 3-month time point
##   and 8-11 represent 12-month, so would be nice to specify that in column groups
## - ability to specify shading, or at least column/row sections: in this example, could shade 3m
##   columns to more easily differentiate from 12m columns, or insert gridlines to separate.
## - I reserve the right to think of more :)
