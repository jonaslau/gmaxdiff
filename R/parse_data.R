#' Format headers of data file
#'
#' @param md_define A list containing a sublist of data
#' @export
#' @returns A list containing the data and processed data
#' @examples
#' process_attribute_text = function(md_define)
process_attribute_text = function(md_define = NULL){
  # check number of attributes at the end
  n_attributes = 0

  # check md_define
  if(is.null(md_define) | is.null(md_define$data$header)){
    warning("No data frame to process!")
  }

  # retrieve info for process
  dat_header = md_define$data$header
  n_attributes = md_define$n$attributes

  # clean up the raw data header
  dat = md_define$data$raw

  # for merger header
  dat_header_merger = tibble(
    name_old = names(dat)
  )
  dat_header_merger = dat_header_merger %>%
    left_join(dat_header %>% select(name_old = name, name_new),
              by = "name_old")
  # replace names of maxdiff columns with
  # (digit)_md_(digit) format
  # cooresponds to loop, md_identifier, attribute
  names(dat) = if_else(!is.na(dat_header_merger$name_new),
                       dat_header_merger$name_new, names(dat), NA_character_)

  # save to data
  md_define$data$raw = dat

  # return object
  return(md_define)
}

#' Internal helper function: Check incomplete response per respondent
#'
#' @param md_define A list containing a sublist of data
#' @returns A list containing the data and processed data
check_response_number <- function(md_define = NULL) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process!")
  }

  # retrieve data for process
  dat <- md_define$data$raw
  dat_maxdiff <- md_define$data$maxdiff

  # read data from md_define for processing
  n_trials <- md_define$n$trials

  dat_respondents <- dat %>%
    select(ResponseId, matches("\\d+_md_")) %>%
    pivot_longer(
      cols = -ResponseId,
      names_pattern = "(\\d+)_md_(.+)",
      names_to = c("loop", "option"),
      values_to = "response"
    ) %>%
    filter(!is.na(response))

  # count responses per respondent
  dat_respondents <- dat_respondents %>%
    filter(option != "DO") %>%
    group_by(ResponseId) %>%
    count(response) %>%
    pivot_wider(
      id_cols = ResponseId,
      names_from = response,
      names_prefix = "resp_",
      values_from = n
    )

  # filter out people with incomplete responses
  dat_respondents_incomplete <- dat_respondents %>%
    filter(resp_1 != n_trials | resp_2 != n_trials) %>%
    pull(ResponseId)

  if (length(dat_respondents_incomplete) > 0) {
    message(paste0(
      "list of ResponseId with inconsistent number of trials:\n",
      paste(dat_respondents_incomplete, collapse = ", ")
    ))

    # update respondent list
    dat_respondents <- dat_respondents %>%
      filter(!ResponseId %in% dat_respondents_incomplete)

    # update maxdiff format file
    dat_maxdiff <- dat_maxdiff %>%
      filter(!ResponseId %in% dat_respondents_incomplete)
  }

  # update number of complete responses
  n_respondents <- length(unique(dat_respondents$ResponseId))

  # save to data
  md_define$n$respondents <- n_respondents

  # return object
  return(md_define)
}

#' Internal helper function: Check if there attribute coding matches
#'
#' @param md_define A list containing a sublist of data
#' @returns NULL. Throws a warning if not matched
check_displaymatrix <- function(md_define = NULL) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  n_attributes <- md_define$n$attributes
  n_shown <- md_define$n$shown
  dat <- md_define$data$raw

  # gather the display order info
  dat_displayorder <- dat %>%
    select(ResponseId, matches("_md_DO$")) %>%
    pivot_longer(
      cols = -ResponseId,
      names_pattern = "(\\d+)_(md_DO)",
      names_to = c("loop", ".value")
    ) %>%
    mutate(n_items = str_count(md_DO, "\\|") + 1)

  # check if number of options per trials are consistent
  if (var(dat_displayorder$n_items) > 0) {
    warning("Some trials have fewer options than others!")
  }

  # check if length of option codes are consistent
  dat_displayorder <- dat_displayorder %>%
    separate(md_DO, into = paste0("c", 1:n_shown))

  # number of each attributes showing up
  dat_displayorder <- dat_displayorder %>%
    pivot_longer(
      cols = matches("^c\\d+"),
      names_to = "option_order",
      values_to = "attribute"
    ) %>%
    count(attribute, name = "n") %>%
    mutate(attribute = as.integer(attribute)) %>%
    arrange(attribute)

  # check if coding in DO column matches number of attributes
  if (max(dat_displayorder$attribute) != n_attributes) {
    warning("Coding in Design Matrix is inconsistent.")
  }
}

#' parse maxdiff question format
#'
#' @param md_define A list containing a sublist of data
#' @param signchange Boolean, whether the scale should flip. Default: FALSE
#' @returns A list containing the data and processed data
#' @import tidyr
#' @export
#' @examples
#' process_maxdiff_format(md_define, signchange = FALSE)
process_maxdiff_question_format <- function(md_define = NULL, signchange = FALSE) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  n_attributes <- md_define$n$attributes
  n_shown <- md_define$n$shown
  dat <- md_define$data$raw

  # column names
  dat_maxdiff <- dat %>%
    select(ResponseId, matches("\\d+_(.+)_\\d+$|\\d+_(.+)_DO$")) %>%
    pivot_longer(
      cols = -ResponseId,
      names_pattern = "(\\d+)_(.+)_(\\d+$|DO$)",
      names_to = c("trial", "trial_attribute", ".value")
    )

  # number of attribute shown per trial
  dat_maxdiff <- dat_maxdiff %>%
    mutate(n_shown = str_count(DO, "\\|") + 1)

  if (var(dat_maxdiff$n_shown) > 0) {
    warning("Inconsistent number of attributes shown.")
  }

  # record number of attributes shown per trial
  n_shown <- median(dat_maxdiff$n_shown)
  dat_maxdiff <- dat_maxdiff %>%
    select(-n_shown)

  # break design matrix into columns
  dat_maxdiff <- dat_maxdiff %>%
    separate(col = DO, sep = "\\|", into = paste0("c__", 1:n_shown))

  # trial choice options matrix
  # convert choice to a long data frame: c__ for choice positions
  dat_DO <- dat_maxdiff %>%
    select(ResponseId, trial, trial_attribute, starts_with("c__")) %>%
    pivot_longer(
      cols = starts_with("c__"),
      names_to = "choice_order",
      values_to = "choice_attribute"
    ) %>%
    mutate(attribute = choice_attribute) %>%
    # convert design matrix back to a wide format: a__ for attribute
    mutate(dummy = 1L) %>%
    pivot_wider(
      id_cols = everything(),
      names_from = choice_attribute,
      names_prefix = "a__",
      values_from = dummy,
      values_fill = 0L
    ) %>%
    mutate(choice_order = as.integer(str_replace(choice_order, "c__", ""))) %>%
    select(
      ResponseId, trial, trial_attribute, choice_order, attribute,
      paste0("a__", 1:n_attributes)
    )

  # response matrix
  dat_choice <- dat_maxdiff %>%
    # select(ResponseId, trial, trial_attribute, starts_with("\\d+")) %>%
    select(-starts_with("c__")) %>%
    pivot_longer(
      cols = -c(ResponseId, trial, trial_attribute),
      names_to = "chosen_attribute",
      values_to = "polarity"
    ) %>%
    filter(!is.na(polarity))

  # combine choice and attribute matrix
  dat_maxdiff <- dat_choice %>%
    left_join(dat_DO, by = c("ResponseId", "trial", "trial_attribute")) %>%
    mutate(chosen = as.integer(attribute == chosen_attribute))

  rm(dat_DO, dat_choice)

  # standardize polarity coding
  polarity_code <- sort(unique(dat_maxdiff$polarity))

  dat_maxdiff <- dat_maxdiff %>%
    mutate(polarity = factor(polarity,
                             levels = polarity_code,
                             labels = c(1, 2), ordered = TRUE
    ))

  # if sign changes
  if (signchange) {
    polarity_code_rev <- c(0, 0)
    polarity_code_rev[1] <- polarity_code[2]
    polarity_code_rev[2] <- polarity_code[1]
    polarity_code <- polarity_code_rev

    dat_maxdiff <- dat_maxdiff %>%
      # mutate(polarity = -polarity + 3) %>%
      mutate(polarity = factor(polarity,
                               levels = polarity_code,
                               labels = c(1, 2), ordered = TRUE
      ))
  }

  # process best and worst choices
  dat_maxdiff <- dat_maxdiff %>%
    mutate_at(
      vars(matches("^a__")), # 1:n_attributes
      function(x) {
        x * ((as.integer(dat_maxdiff$polarity) - 1.5) * 2)
      }
    )

  # save to data
  md_define$data$maxdiff <- dat_maxdiff
  md_define$n$shown <- n_shown
  md_define$polarity_code <- polarity_code

  # check number of responses
  md_define <- check_response_number(md_define = md_define)

  # check display matrix
  # throw an error if not match
  check_displaymatrix(md_define)

  # return object
  return(md_define)
}

#' Parse maxdiff module format data (instead of maxdiff question format)
#'
#' @param md_define A list containing a sublist of data
#' @param signchange Boolean, whether the scale should flip. Default: FALSE
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' process_maxdiff_module_format(md_define, signchange = FALSE)
process_maxdiff_module_format <- function(md_define = NULL, signchange = FALSE) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  dat <- md_define$data$raw
  dat_header <- md_define$data$header

  # response
  dat_md_resp <- dat %>%
    select(ResponseId, matches("C\\d+_\\d+$"))

  # long format
  dat_md_resp <- dat_md_resp %>%
    pivot_longer(
      cols = -ResponseId,
      names_pattern = "C(\\d+)_(\\d+$)",
      names_to = c("trial", "choice_order"),
      values_to = "resp"
    )

  # display order
  dat_md_do <- dat %>%
    select(ResponseId, matches("\\d+_\\d+_MAXDIFF$"))

  # long format
  dat_md_do <- dat_md_do %>%
    pivot_longer(
      cols = -ResponseId,
      names_pattern = "(\\d+)_(\\d+)_MAXDIFF",
      names_to = c("trial", "choice_order"),
      values_to = "attribute"
    )

  # attribute list
  dat_attribute <- sort(unique(dat_md_do$attribute))

  # recode display order
  dat_md_do <- dat_md_do %>%
    mutate(attribute = as.integer(factor(attribute,
                                         dat_attribute,
                                         1:length(dat_attribute),
                                         ordered = TRUE
    )))

  # maxdiff format
  dat_md <- dat_md_resp %>%
    left_join(dat_md_do, by = c("ResponseId", "trial", "choice_order")) %>%
    mutate(attribute2 = attribute) %>%
    mutate(dummy = 1) %>%
    pivot_wider(
      id_cols = everything(),
      names_from = attribute2,
      names_prefix = "a__",
      values_from = dummy
    )

  # polarity coding
  polarity_code <- dat_md %>%
    filter(!is.na(resp)) %>%
    pull(resp)

  polarity_code <- sort(unique(polarity_code))

  if (!signchange) {
    # make new rows that's worst and best
    dat_md <- dat_md %>%
      mutate(polarity = 1L) %>%
      bind_rows(dat_md %>%
                  mutate(polarity = 2L)) %>%
      mutate(resp = factor(resp, polarity_code, c(1L, 2L), NA_integer_))
  } else {
    polarity_code <- sort(unique(polarity_code), decreasing = TRUE)
    dat_md <- dat_md %>%
      mutate(polarity = 1L) %>%
      bind_rows(dat_md %>%
                  mutate(polarity = 2L)) %>%
      mutate(resp = factor(resp, polarity_code, c(1L, 2L), NA_integer_))
  }
  # mark options as chosen
  dat_md <- dat_md %>%
    mutate(chosen = if_else(resp == polarity, 1L, 0L, 0L))

  # arrange the columns
  dat_md <- dat_md %>%
    select(
      ResponseId, trial, polarity,
      choice_order, attribute,
      paste0("a__", 1:length(dat_attribute)), chosen
    )

  # change negative values to -1
  dat_md <- dat_md %>%
    mutate_at(
      paste0("a__", 1:length(dat_attribute)),
      function(x) {
        x * ((dat_md$polarity - 1) * 2 - 1)
      }
    )

  n_trial <- max(as.integer(dat_md$trial))

  # change trial numbers
  dat_md <- dat_md %>%
    mutate(trial = as.integer(trial) + (polarity - 1) * n_trial) %>%
    mutate(choice_order = as.integer(choice_order))

  # fill in the NA values
  dat_md[is.na(dat_md)] <- 0

  # question
  question = dat_header$describe[which(dat_header$name == "C1_1")]
  question = str_replace_all(question, "\\[Field-\\d+\\.\\d+_MAXDIFF\\]", "")

  # save to data
  md_define$data$maxdiff <- dat_md
  md_define$n$shown <- max(as.integer(dat_md$choice_order))
  md_define$n$attributes <- length(dat_attribute)
  md_define$n$trials <- max(dat_md$trial)
  md_define$n$respondents <- length(unique(dat_md$ResponseId))
  md_define$attributes <- dat_attribute
  md_define$polarity_code <- polarity_code
  md_define$question <- question

  # return object
  return(md_define)
}
