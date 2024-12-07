getwd()
install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)
library(tidyverse)
interests <- read_excel('./interests.xlsx',sheet=2,col_types = c("date",  rep("numeric", 2)))
head(interests)
View(interests)

glimpse(interests)






