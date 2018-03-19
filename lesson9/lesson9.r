install.packages("learningr")
#C:\Users\irgepi\AppData\Local\Temp\RtmpKYTdLS\downloaded_packages\learningr_0.29.zip\learningr
library(learningr)

yn_to_logical <- function(x) {
  y <- rep.int(NA, length(x))
  y[x == "Y"] <- TRUE
  y[x == "N"] < FALSE
  y
}

alpe_d_huez$DrugUse <- yn_to_logical(alpe_d_huez$DrugUse)
?grep
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]
?data
data(english_monarchs, package = "learningr")
head(english_monarchs)
View(english_monarchs)

library(stringr)
?str_detect
multiple_kingdoms <- str_detect(english_monarchs$domain, fixed(","))
english_monarchs[multiple_kingdoms, c("name","domain")]
