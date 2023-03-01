clean_omnibus_df <- function(df) {

    #ID
    df$rid <- df$ResponseId

    # Age
    df$age <- parse_number(df$age)

    # Gender
    df$male <- if_else(df$Q118 == "Man", 1, 0)

    # Income
    df$Q114 <- parse_character(df$Q114)
    df$Q114[df$Q114 == "None or less than $2,999"] <- "1"
    df$Q114[df$Q114 == "$3,000-$4,999"] <- "2"
    df$Q114[df$Q114 == "$5,000-$6,999"] <- "3"
    df$Q114[df$Q114 == "$7,000-$8,999"] <- "4"
    df$Q114[df$Q114 == "$9,000-$10,999"] <- "5"
    df$Q114[df$Q114 == "$11,000-$12,999"] <- "6"
    df$Q114[df$Q114 == "$13,000-$14,999"] <- "7"
    df$Q114[df$Q114 == "$15,000-$16,999"] <- "8"
    df$Q114[df$Q114 == "$17,000-$19,999"] <- "9"
    df$Q114[df$Q114 == "$20,000-$21,999"] <- "10"
    df$Q114[df$Q114 == "$22,000-$24,999"] <- "11"
    df$Q114[df$Q114 == "$25,000-$29,999"] <- "12"
    df$Q114[df$Q114 == "$30,000-$34,999"] <- "13"
    df$Q114[df$Q114 == "$35,000-$39,999"] <- "14"
    df$Q114[df$Q114 == "$40,000-$44,999"] <- "15"
    df$Q114[df$Q114 == "$45,000-$49,999"] <- "16"
    df$Q114[df$Q114 == "$50,000-$59,999"] <- "17"
    df$Q114[df$Q114 == "$60,000-$69,999"] <- "18"
    df$Q114[df$Q114 == "$70,000-$79,999"] <- "19"
    df$Q114[df$Q114 == "$80,000-$89,999"] <- "20"
    df$Q114[df$Q114 == "$90,000-$104,999"] <- "21"
    df$Q114[df$Q114 == "$105,000-$119,999"] <- "22"
    df$Q114[df$Q114 == "$120,000 and over"] <- "23"
    names(df)[names(df) == 'Q114'] <- "income"
    df$income <- parse_number(df$income)

    # Education
    df$Q116 <- parse_character(df$Q116)
    df$Q116[df$Q116 == unique(df$Q116)[4]] <- "1"
    df$Q116[df$Q116 == unique(df$Q116)[2]] <- "2"
    df$Q116[df$Q116 == unique(df$Q116)[1]] <- "3"
    df$Q116[df$Q116 == unique(df$Q116)[4]] <- "4"
    df$Q116[df$Q116 == unique(df$Q116)[3]] <- "5"
    df$Q116[df$Q116 == unique(df$Q116)[6]] <- "6"
    names(df)[names(df) == 'Q116'] <- "edu"
    df$edu <- parse_number(df$edu)

    # Citizenship
    df$Q111 <- parse_character(df$Q111)
    df$us_citizen <- if_else(df$Q111 == "Yes", 1, 0)

    # Race
    df$Q112 <- parse_character(df$Q112)
    df$Q112[df$Q112 == unique(df$Q112)[5]] <- "AAPI"
    df$Q112[df$Q112 == unique(df$Q112)[2]] <- "Latino"
    df$Q112[df$Q112 == unique(df$Q112)[1]] <- "White"
    df$Q112[df$Q112 == unique(df$Q112)[6]] <- "Indigenous"
    df$Q112[df$Q112 == unique(df$Q112)[3]] <- "Black"
    df$Q112[df$Q112 == unique(df$Q112)[4]] <- "Other"
    names(df)[names(df) == 'Q112'] <- "race"

    df <- df[df$Finished == "True" & df$Status == "IP Address", ]

    return(df)
}

clean_immigrant_df <- function(df) {

    #ID
    df$rid <- df$ResponseId

    # Age
    df$Q92[df$Q92 == "eightey six"] <- "86"
    df$Q92[df$Q92 == "²2"] <- "22"
    df$Q92[df$Q92 == "J56"] <- "56"
    df$Q92 <- parse_number(df$Q92)
    names(df)[names(df) == 'Q92'] <- "age"

    # Gender
    df$Q93 <- parse_character(df$Q93)
    df$Q93 <- if_else(df$Q93 == "Male", 1, 0)
    names(df)[names(df) == 'Q93'] <- "male"

    # Income
    df$Q94[df$Q94 == "$0 - $24,999"] <- "1"
    df$Q94[df$Q94 == "$25,000 - $49,999"] <- "2"
    df$Q94[df$Q94 == "$50,000 - $74,999"] <- "3"
    df$Q94[df$Q94 == "$75,000 - $99,999"] <- "4"
    df$Q94[df$Q94 == "$100,000 +"] <- "5"
    df$Q94 <- parse_number(df$Q94)
    names(df)[names(df) == 'Q94'] <- "income"

    # Education
    df$Q96[df$Q96 == unique(df$Q96)[5]] <- "1"
    df$Q96[df$Q96 == unique(df$Q96)[7]] <- "2"
    df$Q96[df$Q96 == unique(df$Q96)[3]] <- "3"
    df$Q96[df$Q96 == unique(df$Q96)[1]] <- "4"
    df$Q96[df$Q96 == unique(df$Q96)[4]] <- "5"
    df$Q96[df$Q96 == unique(df$Q96)[6]] <- "6"
    df$Q96[df$Q96 == unique(df$Q96)[2]] <- "7"
    df$Q96 <- parse_number(df$Q96)
    names(df)[names(df) == 'Q96'] <- "edu"

    # Citizenship
    df$Q99 <- parse_number(df$Q99)
    df$Q99[df$Q99 < 1900] <- NA
    df$us_citizen <- if_else(!is.na(df$Q99) | df$Q111 == "Yes", 1, 0)

    # Race
    df$Q97 <- parse_character(df$Q97)
    df$Q97[df$Q97 == unique(df$Q97)[1]] <- "AAPI"
    df$Q97[df$Q97 == unique(df$Q97)[2]] <- "Latino"
    df$Q97[df$Q97 == unique(df$Q97)[3]] <- "White"
    df$Q97[df$Q97 == unique(df$Q97)[4]] <- "Indigenous"
    df$Q97[df$Q97 == unique(df$Q97)[5]] <- "Black"
    df$Q97[df$Q97 == unique(df$Q97)[7]] <- "AAPI"
    names(df)[names(df) == 'Q97'] <- "race"

    df <- df[df$Finished == "True" & df$Status == "IP Address", ]

    return(df)
}

clean_perceptions_df <- function(df) {

    #ID
    df$rid <- df$ResponseId

    df <- df[df$Finished == "True" & df$Status == "IP Address", ]

    return(df)
}

read_csv_custom <- function(i) {
    out <- read.csv(here("data", file_names[i]))
    out <- out[-c(1:2),]
    return(out)
}

df2comb <- function(df, dataset = "perceptions") {

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

    if (dataset == "perceptions") {

    df.comb <- tibble(rid = rep(df$rid, 15),
                      name = name,
                      race = race,
                      specific.race = specific.race,
                      citizen = citizen,
                      education = education,
                      income = income)
                      }

    if (dataset %in% c("immigrants", "omnibus")) {

    df.comb <- tibble(rid = rep(df$rid, 10),
                      name = name,
                      race = race,
                      specific.race = specific.race,
                      citizen = citizen,
                      education = education,
                      income = income,
                      res.age = rep(df$age, 10),
                      res.male = rep(df$male, 10),
                      res.income = rep(df$income, 10),
                      res.edu = rep(df$edu, 10),
                      res.citizenship = rep(df$us_citizen, 10),
                      res.race = rep(df$race, 10))

    }

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

model.names <- c("Correct", "Citizen", "Education", "Income")

df2summ <- function(df) {

    mod.match <- lm(match ~ identity + w.asian, data = df)

    mod.ed <- lm(education.ord ~ identity + w.asian, data = df)

    mod.income <- lm(income.ord ~ identity + w.asian, data = df)

    mod.citizen <- lm(citizen ~ identity + w.asian, data = df)

    summ.match <-
        summ(mod.match,
             model.names = model.names[1],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                    "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                    "Black" = "identityBlack or African American"),
             cluster = "rid",
             robust = "HC3",
             colors = "#000000") %>%
        tidy(conf.int = T) %>%
        mutate(model = "Correct (0/1)")

    summ.citizen <-
        summ(mod.citizen,
             model.names = model.names[2],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                     "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                     "Black" = "identityBlack or African American"),
             cluster = "rid",
             robust = "HC3",
             colors = "#000000") %>%
        tidy(conf.int = T) %>%
        mutate(model = "Citizen (0/1)")

    summ.ed <-
        summ(mod.ed,
             model.names = model.names[3],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                       "Asian (White first name)" = "w.asian",
                       "Hispanic" = "identityHispanic",
                       "Black" = "identityBlack or African American"),
             cluster = "rid",
             robust = "HC3",
             colors = "#000000") %>%
        tidy(conf.int = T) %>%
        mutate(model = "Education (1-4)")

    summ.income <-
        summ(mod.income,
             model.names = model.names[4],
             coefs = c("Asian" = "identityAsian or Pacific Islander",
                     "Asian (White first name)" = "w.asian",
                     "Hispanic" = "identityHispanic",
                     "Black" = "identityBlack or African American"),
             cluster = "rid",
             robust = "HC3",
             colors = "#000000") %>%
        tidy(conf.int = T) %>%
        mutate(model = "Income (1-3)")

    out <- bind_rows(summ.match, summ.citizen, summ.ed, summ.income)

    return(out)
}

df2plot_all <- function(df) {

    df %>%
        filter(term != "(Intercept)") %>%
        mutate(term = recode(term,
                             "identityAsian or Pacific Islander" = "Asian",
                             "w.asian" = "Asian (White first name)",
                             "identityHispanic" =  "Hispanic",
                             "identityBlack or African American" = "Black"
        )) %>%
        mutate(model = fct_relevel(model, "Correct (0/1)")) %>%
        ggplot(aes(x = term, y = estimate, ymax = estimate+(1.96*std.error), ymin = estimate-(1.96*std.error), col = Data, group = Data)) +
        geom_pointrange(aes(group = Data, color = Data),
                        position = position_dodge(width = 1), size = 1.5) +
        facet_wrap(~model) +
        theme_ipsum_ps() +
        labs(x = "", y = "",
             col = "Datasets") +
        coord_flip() +
        scale_colour_brewer(palette = "Set1") +
        geom_hline(yintercept = 0, linetype = "dashed")

}

df2plot <- function(df) {

    mod.match <- lm(match ~ identity + w.asian, data = df)

    mod.ed <- lm(education.ord ~ identity + w.asian, data = df)

    mod.income <- lm(income.ord ~ identity + w.asian, data = df)

    mod.citizen <- lm(citizen ~ identity + w.asian, data = df)

    model.names <- c("Correct", "Citizen", "Education", "Income")

    plot.match <-
        plot_summs(mod.match,
                   model.names = model.names[1],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American"),
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Income (1-3)") +
        ylab("")

    out <- (plot.match + plot.citizen) / (plot.ed + plot.income)

    return(out)
}

df2plot_covariate <- function(df) {

    mod.match <- lm(match ~ identity + w.asian + res.age + factor(res.male) + res.income + res.edu + factor(res.citizenship) + factor(res.race), data = df)

    mod.ed <- lm(education.ord ~ identity + w.asian + res.age + factor(res.male) + res.income + res.edu + factor(res.citizenship) + factor(res.race), data = df)

    mod.income <- lm(income.ord ~ identity + w.asian + res.age + factor(res.male) + res.income + res.edu + factor(res.citizenship) + factor(res.race), data = df)

    mod.citizen <- lm(citizen ~ identity + w.asian + res.age + factor(res.male) + res.income + res.edu + factor(res.citizenship) + factor(res.race), data = df)

    model.names <- c("Correct", "Citizen", "Education", "Income")

    plot.match <-
        plot_summs(mod.match,
                   model.names = model.names[1],
                   coefs = c("Asian" = "identityAsian or Pacific Islander",
                             "Asian (White first name)" = "w.asian",
                             "Hispanic" = "identityHispanic",
                             "Black" = "identityBlack or African American",
                             "Respondent age" = "res.age",
                             "Respondent = male" = "`factor(res.male)`",
                             "Respondent income" = "res.income",
                             "Respondent education" = "res.edu",
                             "Respondent = U.S. citizen" = "`factor(res.citizenship)`",
                             "Respondent = Black" =
                                 "`factor(res.race)`Black",
                             "Respondent = Indigenous" =
                                 "`factor(res.race)`Indigenous`",
                             "Respondent = Latino" =
                                 "`factor(res.race)`Latino",
                             "Respondent = White" =
                                 "`factor(res.race)`White"),
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                             "Black" = "identityBlack or African American",
                             "Respondent age" = "res.age",
                             "Respondent = male" = "`factor(res.male)`",
                             "Respondent income" = "res.income",
                             "Respondent education" = "res.edu",
                             "Respondent = U.S. citizen" = "`factor(res.citizenship)`",
                             "Respondent = Black" =
                                 "`factor(res.race)`Black",
                             "Respondent = Indigenous" =
                                 "`factor(res.race)`Indigenous`",
                             "Respondent = Latino" =
                                 "`factor(res.race)`Latino",
                             "Respondent = White" =
                                 "`factor(res.race)`White"),
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                             "Black" = "identityBlack or African American",
                             "Respondent age" = "res.age",
                             "Respondent = male" = "`factor(res.male)`",
                             "Respondent income" = "res.income",
                             "Respondent education" = "res.edu",
                             "Respondent = U.S. citizen" = "`factor(res.citizenship)`",
                             "Respondent = Black" =
                                 "`factor(res.race)`Black",
                             "Respondent = Indigenous" =
                                 "`factor(res.race)`Indigenous`",
                             "Respondent = Latino" =
                                 "`factor(res.race)`Latino",
                             "Respondent = White" =
                                 "`factor(res.race)`White"),
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
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
                             "Black" = "identityBlack or African American",
                             "Respondent age" = "res.age",
                             "Respondent = male" = "`factor(res.male)`",
                             "Respondent income" = "res.income",
                             "Respondent education" = "res.edu",
                             "Respondent = U.S. citizen" = "`factor(res.citizenship)`",
                             "Respondent = Black" =
                                 "`factor(res.race)`Black",
                             "Respondent = Indigenous" =
                                 "`factor(res.race)`Indigenous`",
                             "Respondent = Latino" =
                                 "`factor(res.race)`Latino",
                             "Respondent = White" =
                                 "`factor(res.race)`White"),
                   cluster = "rid",
                   robust = "HC3",
                   colors = "#000000",
                   scale = TRUE) +
        theme_ipsum_ps() +
        ggtitle("Income (1-3)") +
        ylab("")

    out <- (plot.match + plot.citizen) / (plot.ed + plot.income)

    return(out)
}

df2select <- function(df) {

    df %>%
        select(name, first, last, w.asian, identity, match)

}

df_rename <- function(df) {

    names(df)[names(df) == 'rid'] <- 'id'
    names(df)[names(df) == 'match'] <- 'correct'

    return(df)

}

# Standard error
se <- function(x) sqrt(var(x) / length(x))

# Min-max scaling
normalize <- function(x){(x- min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

# mutate satisficing variable
add_satisficing <- function(df) {

    df$num_secs <- parse_number(df$Duration..in.seconds.)

    threshold <- median(df$num_secs)*0.4

    message(glue("The the median duration is {threshold} minutees"))

    df$satisficing <- if_else(df$num_secs < threshold, 1, 0)

    if (length(df$rid) != 0) { # immigrants, omnibus

        df <- df %>% select(rid, satisficing)

    } else { # perceptions

        df <- df %>% select(ResponseId, satisficing)

    }

    return(df)
}