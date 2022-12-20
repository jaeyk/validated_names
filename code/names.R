# Create names for pre-test perceptions check
# Want to create five groups of names: Asian, Asian (with White first names), Black, Hispanic, and White

### Clear terminal
cat("\014")

### Clear space
rm(list = ls())

### Load library
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tools, tidyr, rio)

### Set WD
setwd("~/Dropbox/asian-discrimination/perspective-taking/data/")

### Import data
# most common first names by ethnicity - https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TYJKEZ
df.first <- rio::import("firstnames.xlsx", which = 2)

# most common last names by ethnicity - https://www.census.gov/topics/population/genealogy/data/2010_surnames.html
df.last <- rio::import("Names_2010Census_Top1000.xlsx")

# most common first names among Chinese Americans (use for all Asians, given data limitations?) - https://blogs.iq.harvard.edu/english_first_n
white.asian.first.m <- c("Andrew", "Andy", "Dan", "Peter", "Albert", "Eric", "Alan", "Sam", "Alex", "David")

white.asian.first.f <- c("Amy", "Jenny", "Grace", "May", "Vivian", "Alice", "Jennifer", "Cecilia", "Jane", "Cindy")

### Extract name vectors
# first names
head(df.first)
summary(df.first)

asian.first.df <- tail(subset(df.first[order(df.first$obs), ], pctapi > 90), 10)
asian.first <- toTitleCase(tolower(asian.first.df$firstname))

black.first.df <- tail(subset(df.first[order(df.first$obs), ], pctblack > 80), 10)
black.first <- toTitleCase(tolower(black.first.df$firstname))

hispanic.first.df <- tail(subset(df.first[order(df.first$obs), ], pcthispanic > 90), 10)
hispanic.first <- toTitleCase(tolower(hispanic.first.df$firstname))

white.first.df <- tail(subset(df.first[order(df.first$obs), ], pctwhite > 90), 10)
white.first <- toTitleCase(tolower(white.first.df$firstname))

# last names
head(df.last)
summary(df.last)
colnames(df.last) <- c("surname", "rank", "obs", "propper100000", "cumprop", "white", "black", "api", "aian", "2prace", "hispanic")
df.last$api <- as.numeric(df.last$api)

asian.last.df <- tail(subset(df.last[order(df.last$obs), ], api > 90), 10)
asian.last <- toTitleCase(tolower(asian.last.df$surname))

black.last.df <- tail(subset(df.last[order(df.last$obs), ], black > 45), 10)
black.last <- toTitleCase(tolower(black.last.df$surname))

summary(df.last$hispanic)
hispanic.last.df <- tail(subset(df.last[order(df.last$obs), ], hispanic > 80), 10)
hispanic.last <- toTitleCase(tolower(hispanic.last.df$surname))

white.last.df <- tail(subset(df.last[order(df.last$obs), ], white > 90), 10)
white.last <- toTitleCase(tolower(white.last.df$surname))

### Create name combination vectors
asian <- tidyr::crossing(asian.first, asian.last)
black <- tidyr::crossing(black.first, black.last)
hispanic <- tidyr::crossing(hispanic.first, hispanic.last)
white <- tidyr::crossing(white.first, white.last)
white.asian <- tidyr::crossing(c(white.asian.first.f, white.asian.first.m), asian.last)
colnames(asian) <- colnames(black) <- colnames(hispanic) <- colnames(white) <- colnames(white.asian) <- c("first", "last")

df.names <- tibble(rbind(asian, black, hispanic, white, white.asian))
df.names$identity <- c(rep("Asian or Pacific Islander", 100),
                       rep("Black or African American", 100),
                       rep("Hispanic", 100),
                       rep("White", 100),
                       rep("White Asian", 200))

df.names
table(df.names$identity)

# Create full name variable
df.names$name <- paste(df.names$first, df.names$last)

df.names <- df.names[!duplicated(df.names$name), ]

write.csv(df.names$name, "../services/names.csv", row.names = F)

write.csv(df.names, "names.csv", row.names = F)
