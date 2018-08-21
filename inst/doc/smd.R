## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- results=FALSE------------------------------------------------------
library(Matching)
library(tangram)
## Right heart cath dataset
rhc <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")

## ---- results='asis'-----------------------------------------------------
vars <- c("age","sex","race","edu","income","ninsclas","cat1","das2d3pc","dnr1",
          "ca","surv2md1","aps1","scoma1","wtkilo1","temp1","meanbp1","resp1",
          "hrt1","pafi1","paco21","ph1","wblc1","hema1","sod1","pot1","crea1",
          "bili1","alb1","resp","card","neuro","gastr","renal","meta","hema",
          "seps","trauma","ortho","cardiohx","chfhx","dementhx","psychhx",
          "chrpulhx","renalhx","liverhx","gibledhx","malighx","immunhx",
          "transhx","amihx")

formula <- paste0("swang1~", paste0(vars, collapse="+"))

tangram(formula, rhc, "unmatched", smd, "Unmatched", style="nejm", pct_width=0.5)

## ---- results='asis'-----------------------------------------------------
## Fit model
psModel <- glm(formula = formula,
               family  = binomial(link = "logit"),
               data    = rhc)

## Predicted probability of being assigned to RHC
rhc$pRhc <- predict(psModel, type = "response")
## Predicted probability of being assigned to no RHC
rhc$pNoRhc <- 1 - rhc$pRhc

## Predicted probability of being assigned to the
## treatment actually assigned (either RHC or no RHC)
rhc$pAssign <- NA
rhc$pAssign[rhc$swang1 == "RHC"]    <- rhc$pRhc[rhc$swang1   == "RHC"]
rhc$pAssign[rhc$swang1 == "No RHC"] <- rhc$pNoRhc[rhc$swang1 == "No RHC"]
## Smaller of pRhc vs pNoRhc for matching weight
rhc$pMin <- pmin(rhc$pRhc, rhc$pNoRhc)

listMatch <- Match(Tr       = (rhc$swang1 == "RHC"),      # Need to be in 0,1
                   ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                   X        = log(rhc$pRhc / rhc$pNoRhc),
                   ## 1:1 matching
                   M        = 1,
                   ## caliper = 0.2 * SD(logit(PS))
                   caliper  = 0.2,
                   replace  = FALSE,
                   ties     = TRUE,
                   version  = "fast")
## Extract matched data
rhcMatched <- rhc[unlist(listMatch[c("index.treated","index.control")]), ]

tangram(formula, rhcMatched, "matched", smd, "Propensity Score Matched", style="nejm", pct_width=0.5)

## ---- results='asis'-----------------------------------------------------
tangram(formula, rhc, "weighted", smd,
        "Propensity Score Weighted",
        style="nejm",
        weight=rhc$pMin/rhc$pAssign, pct_width=0.5)

