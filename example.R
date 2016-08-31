setwd("Projects/tg")

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

f <- formula(drug ~ log(albumin))
test_table <- summary_table(f, pbc)

#lbl_stage <- label(pbc["stage"])
#pbc$stage <- factor(pbc$stage, levels=1:4, ordered=TRUE) # Make a factor, instead of guessing
#label(pbc$stage) <- lbl_stage

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
