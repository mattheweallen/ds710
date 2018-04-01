#install.packages("learningr")
library(learningr)
head(alpe_d_huez2)
head(alpe_d_huez)

yn_to_logical <- function(x) {
  y <- rep.int(NA, length(x))
  y[x == "Y"] <- TRUE
  y[x == "N"] <- FALSE
}

alpe_d_huez$DrugUse <- yn_to_logical(alpe_d_huez$DrugUse)
