library(tidyverse)

source("kernel_matrix.R")
data = read_csv("data/qj4hqv0cmjigrorw.csv")

View(data)
library(lubridate)
ymd(data$TDATDT)
