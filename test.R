library(Hmisc)
getHdata(pbc)


source("tableGrammar/compiler.R")


lbl_stage <- label(pbc["stage"])
pbc$stage <- factor(pbc$stage, levels=1:4, ordered=TRUE) # Make a factor, instead of guessing
label(pbc$stage) <- lbl_stage

test_table <- tg_summary(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#test_table <- tg_summary(drug ~ bili, pbc)
#test_table <- tg_summary(drug ~ bili + albumin + protime + age, pbc)

summary(test_table)

x <- html5(test_table, caption="Table 9: Descriptive Statistics by drug", css="nejm.css");
write(x, "tableGrammar/test.html")

# TODO
#index(table)
#latex(table)
