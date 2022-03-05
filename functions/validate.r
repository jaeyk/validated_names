#' extract best validated names from the correspodence between intended and perceived race
#'
#' @param resondent_race "Black," "White," "AAPI," "Latino," "Other," "Indigenous"
#' @param name_race "Black or African American," "White," "Asian or Pacific Islander," "Hispanic,"
#' @return
#' @importFrom
#' @export

better_than_mean_names <- function(respondent_race, name_race) {

    filtered <- study23 %>%
        filter(str_detect(res.race, respondent_race) &
               str_detect(identity, name_race))

    summed <- filtered %>%
        group_by(name) %>%
        summarize(pct_correct = mean(correct)) %>%
        mutate(mean_correct_score = mean(pct_correct)) %>%
        filter(pct_correct > mean_correct_score) %>%
        mutate(res.race = respondent_race,
               identity = if_else(length(unique(res.race)) == 1,
                                  name_race, respondent_race),
               mean_correct_score = round(mean_correct_score, 2)) %>%
        arrange(desc(pct_correct)) %>%
        select(name, res.race, identity, pct_correct, mean_correct_score)

    out <- summed %>%
        mutate(pct_quality_names = round(nrow(summed)/nrow(filtered), 2))

    return(out)

}

#' visualize the correct scores by last and first names
#'
#' @param resondent_race "Black," "White," "AAPI," "Latino," "Other," "Indigenous"
#' @param name_race "Black or African American," "White," "Asian or Pacific Islander," "Hispanic,"
#' @return
#' @importFrom
#' @export

viz_last_first_names <- function(respondent_race, name_race) {

    last_name <- study23 %>%
        filter(str_detect(res.race, respondent_race) &
                   str_detect(identity, name_race)) %>%
        group_by(last) %>%
        summarize(pct_correct = mean(correct)) %>%
        mutate(mean_pct_correct = mean(pct_correct),
               sd_pct_correct = sd(pct_correct)) %>%
        ggplot(aes(x = fct_reorder(last, pct_correct), pct_correct)) +
        geom_point() +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(
            title = "Sort by last name",
            subtitle = glue("Respondent race: {respondent_race},
                         Intended race: {respondent_race}"),
            x = "", y = "% correct") +
        coord_flip() +
        ylim(c(0.3 , 1))

    first_name <- study23 %>%
        filter(str_detect(res.race, respondent_race) &
                   str_detect(identity, name_race)) %>%
        group_by(first) %>%
        summarize(pct_correct = mean(correct)) %>%
        mutate(mean_pct_correct = mean(pct_correct),
               sd_pct_correct = sd(pct_correct)) %>%
        ggplot(aes(x = fct_reorder(first, pct_correct), pct_correct)) +
        geom_point() +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(
            title = "Sort by first name",
            subtitle = glue("Respondent race: {respondent_race},
                         Intended race: {respondent_race}"),
            x = "", y = "% correct") +
        coord_flip() +
        ylim(c(0.3 , 1))

    last_name / first_name

}

#' summarize the correct scores by last and first names
#'
#' @param resondent_race "Black," "White," "AAPI," "Latino," "Other," "Indigenous"
#' @param name_race "Black or African American," "White," "Asian or Pacific Islander," "Hispanic,"
#' @return
#' @importFrom
#' @export

sum_last_first_names <- function(respondent_race, name_race) {

    last_name <- study23 %>%
        filter(str_detect(res.race, respondent_race) &
                   str_detect(identity, name_race)) %>%
        group_by(last) %>%
        summarize(pct_correct = mean(correct)) %>%
        mutate(mean_correct = mean(pct_correct),
               se_correct = se(pct_correct))

    first_name <- study23 %>%
        filter(str_detect(res.race, respondent_race) &
                   str_detect(identity, name_race)) %>%
        group_by(first) %>%
        summarize(pct_correct = mean(correct)) %>%
        mutate(mean_correct = mean(pct_correct),
               se_correct = se(pct_correct))

    joined <- full_join(last_name, first_name) %>%
        select(last, first, mean_correct, se_correct)

    out <- joined %>% pivot_longer(c("last", "first"),
                        "Name type") %>%
        filter(!is.na(value)) %>%
        mutate(`Name type` = recode(`Name type`,
                                    "last" = "Last name",
                                    "first" = "First name"),
               res.race = respondent_race,
               identity = if_else(length(unique(res.race)) == 1, name_race, respondent_race)) %>%
        select(-value) %>%
        distinct() %>%

    return(out)
}

diff_summ <- function(category, category_name) {

    study123 %>%
        group_by(identity, w.asian, name, correct) %>%
        summarize(category_name = mean(get(category))) %>%
        pivot_wider(names_from = correct,
                    values_from = category_name) %>%
        mutate(diff = `1` - `0`,
               covariate = category_name) %>%
        select(-c(`1`, `0`))

}


dum_mean_se <- function(group) {

    dum_sum_ext %>%
        pivot_longer(contains(group),
                     "type",
                     "estimate") %>%
        mutate(type = str_replace_all(type, glue("{group}_"), "")) %>%
        select(c(1, 10, 11)) %>%
        pivot_wider(names_from = type,
                    values_from = value)

}