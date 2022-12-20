# read excel pkg

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, here, readxl)

# import files
file_name <- list.files(here("tables"))

tb1 <- readxl::read_excel(here("tables", file_name),
                         sheet = 1)

tb2 <- readxl::read_excel(here("tables", file_name),
                          sheet = 2)

tb3 <- readxl::read_excel(here("tables", file_name),
                          sheet = 3)

# fix some errors
tb1$Date[1] <- "February 4, 2021"

# export files
write_csv(tb1, here("tables", "tb1.csv"))
write_csv(tb2, here("tables", "tb2.csv"))
write_csv(tb3, here("tables", "tb3.csv"))