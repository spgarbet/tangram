## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tangram)
pbc <- tangram::pbc

## ----iris1, results="asis"-----------------------------------------------
tangram(iris, id="iris1", style="nejm")

## ----iris2, results="asis"-----------------------------------------------
tangram(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
      iris, "iris2", caption="Iris Stats", style="nejm")

## ---- results="asis"-----------------------------------------------------
iris %>%
  group_by(Species) %>%
  summarise(Mean=mean(Petal.Length),
            Median=median(Petal.Length),
            SD=round(sd(Petal.Length),3) ) %>%
  tangram("iris3", caption="Petal Length (dplyr example)", style="nejm", id="iris3")

