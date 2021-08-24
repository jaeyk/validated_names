read_csv_custom <- function(i) {
    out <- read.csv(here("data", file_names[i]))[-c(1:2)]

    return(out)
}

df2comb <- function(df) {
    name <- c(
        df$name,
        df$name2,
        df$name3,
        df$name4,
        df$name5,
        df$name6,
        df$name7,
        df$name8,
        df$name9,
        df$name10,
        df$name11,
        df$name12,
        df$name13,
        df$name14,
        df$name15
    )

    race <- c(
        df$Q6,
        df$Q12,
        df$Q18,
        df$Q24,
        df$Q30,
        df$Q36,
        df$Q42,
        df$Q48,
        df$Q54,
        df$Q60,
        df$Q66,
        df$Q72,
        df$Q78,
        df$Q84,
        df$Q90
    )

    specific.race <- c(
        df$Q7,
        df$Q13,
        df$Q19,
        df$Q25,
        df$Q31,
        df$Q37,
        df$Q43,
        df$Q49,
        df$Q55,
        df$Q61,
        df$Q67,
        df$Q73,
        df$Q79,
        df$Q85,
        df$Q91
    )

    income <- c(
        df$Q4,
        df$Q10,
        df$Q16,
        df$Q22,
        df$Q28,
        df$Q34,
        df$Q40,
        df$Q46,
        df$Q52,
        df$Q58,
        df$Q64,
        df$Q70,
        df$Q76,
        df$Q82,
        df$Q88
    )

    education <- c(
        df$Q5,
        df$Q11,
        df$Q17,
        df$Q23,
        df$Q29,
        df$Q35,
        df$Q41,
        df$Q47,
        df$Q53,
        df$Q59,
        df$Q65,
        df$Q71,
        df$Q77,
        df$Q83,
        df$Q89
    )

    citizen <- c(
        df$Q2,
        df$Q9,
        df$Q15,
        df$Q21,
        df$Q27,
        df$Q33,
        df$Q39,
        df$Q45,
        df$Q51,
        df$Q57,
        df$Q63,
        df$Q69,
        df$Q75,
        df$Q81,
        df$Q87
    )

    df.comb <- tibble(name = name,
                      race = race,
                      specific.race = specific.race,
                      citizen = citizen,
                      education = education,
                      income = income)

    df.comb <- df.comb[df.comb$race != "", ]
    df.comb <- df.comb[df.comb$name != "", ]
    df.comb <- df.comb[df.comb$name != "x", ]

    df.all <- merge(names_df, df.comb, by = "name", all = F)

    df.all$w.asian <- NA
    df.all$w.asian[df.all$identity == "White Asian"] <- 1
    df.all$w.asian[df.all$identity != "White Asian"] <- 0

    df.all$identity[df.all$identity == "White Asian"] <- "Asian or Pacific Islander"
    df.all$identity <- fct_relevel(df.all$identity, "White")

    df.all$education.ord <- NA
    df.all$education.ord[df.all$education == "High school"] <- 1
    df.all$education.ord[df.all$education == "Bachelor's degree"] <- 2
    df.all$education.ord[df.all$education == "Master's degree"] <- 3
    df.all$education.ord[df.all$education == "Ph.D. degree"] <- 4

    df.all$income.ord <- NA
    df.all$income.ord[df.all$income == "Low income (Less than $40,100)"] <- 1
    df.all$income.ord[df.all$income == "Middle income\t($41,000 - $120,400)"] <- 2
    df.all$income.ord[df.all$income == "Upper income (More than $120,400)"] <- 3

    df.all$citizen <- as.numeric(as.factor(df.all$citizen)) - 2
    df.all <- df.all[df.all$citizen != -1, ]

    df.all$match <- as.numeric(df.all$identity == df.all$race)

    return(df.all)
}

df2summ <- function(df.all) {

    mod.match <- lm(match ~ identity + w.asian, data = df.all)

    mod.ed <- lm(education.ord ~ identity + w.asian, data = df.all)

    mod.income <- lm(income.ord ~ identity + w.asian, data = df.all)

    mod.citizen <- lm(citizen ~ identity + w.asian, data = df.all)

    model.names <- c("Correct", "Citizen", "Education", "Income")

    summ.match <-
        summ(mod.match,
             model.names = model.names[1],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                    "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                    "Black" = "identityBlack or African American"),
             cluster = "id",
             robust = "HC3",
             scale = TRUE) %>%
        tidy(conf.int = T) %>%
        mutate(model = "Correct (0/1)")

    summ.citizen <-
        summ(mod.citizen,
             model.names = model.names[2],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                     "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                     "Black" = "identityBlack or African American"),
             cluster = "id",
             robust = "HC3",
             scale = TRUE) %>%
        tidy(conf.int = T) %>%
        mutate(model = "Citizen (0/1)")

    summ.ed <-
        summ(mod.ed,
             model.names = model.names[3],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                       "Asian (White first name)" = "w.asian",
                       "Hispanic" = "identityHispanic",
                       "Black" = "identityBlack or African American"),
             cluster = "id",
             robust = "HC3",
             scale = TRUE) %>%
        tidy(conf.int = T) %>%
        mutate(model = "Education (1-4)")

    summ.income <-
        summ(mod.income,
             model.names = model.names[4],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                     "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                     "Black" = "identityBlack or African American"),
             cluster = "id",
             robust = "HC3",
             scale = TRUE) %>%
        tidy(conf.int = T) %>%
        mutate(model = "Income (1-3)")

    out <- bind_rows(summ.match, summ.citizen, summ.ed, summ.income)

    return(out)
}

df2plot <- function(df.all) {

    mod.match <- lm(match ~ identity + w.asian, data = df.all)

    mod.ed <- lm(education.ord ~ identity + w.asian, data = df.all)

    mod.income <- lm(income.ord ~ identity + w.asian, data = df.all)

    mod.citizen <- lm(citizen ~ identity + w.asian, data = df.all)

    model.names <- c("Correct", "Citizen", "Education", "Income")

    plot.match <-
        plot_summs(mod.match,
                   model.names = model.names[1],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American"),
                   cluster = "id",
                   robust = "HC3",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Correct (0/1)") +
        ylab("")

    plot.citizen <-
        plot_summs(mod.citizen,
                   model.names = model.names[2],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American"),
                   cluster = "id",
                   robust = "HC3",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Citizen (0/1)") +
        ylab("")

    plot.ed <-
        plot_summs(mod.ed,
                   model.names = model.names[3],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American"),
                   cluster = "id",
                   robust = "HC3",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Education (1-4)") +
        ylab("")

    plot.income <-
        plot_summs(mod.income,
                   model.names = model.names[4],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American"),
                   cluster = "id",
                   robust = "HC3",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Income (1-3)") +
        ylab("")

    out <- (plot.match + plot.citizen) / (plot.ed + plot.income)

    return(out)
}