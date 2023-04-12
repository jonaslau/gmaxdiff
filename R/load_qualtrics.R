#' Read in a csv file
#'
#' @param md_define A list containing a sublist of data
#' @param file_name A string denoting a csv file, e.g., "data.csv"
#' @param folder A string denoting the path of where the data file is.
#' @returns A list containing the data and processed data
#' @import readr
#' @import stringr
#' @export
#' @examples
#' read_qualtrics_data(file_name = "data.csv", folder = "/project/", md_define = md_define)
read_qualtrics_data <- function(file_name = NULL, folder = NULL, md_define = NULL) {
  # change directory
  if (!is.null(folder)) setwd(folder)

  # save to md_define
  if (is.null(md_define)) {
    md_define <- list()
  }

  # read data from csv
  dat <- read_csv(file_name,
    col_names = names(read_csv(file_name, n_max = 0))
  )

  # if there's no ResponseId column
  if (!"ResponseId" %in% names(dat)) {
    dat <- dat %>%
      mutate(ResponseId = paste0("R_", 1:nrow(dat)))
  }

  # remove header lines
  dat_header <- dat[1:3, ]

  # retain responses
  dat <- dat[4:nrow(dat), ]

  # format column names
  # get rid of spaces and special characters
  names(dat) <- str_replace_all(names(dat), "\\W+", "_")
  names(dat) <- str_replace_all(names(dat), "^_+", "")
  names(dat) <- str_replace_all(names(dat), "_+$", "")


  # initial processing of header
  dat_header <- as.data.frame(t(dat_header))

  # column names
  colnames(dat_header) <- c("name", "describe", "importId")

  # align names in header with names in dat
  # change row names
  rownames(dat_header) <- 1:nrow(dat_header)
  dat_header$name <- names(dat)

  # save to data
  md_define$data$raw <- dat
  md_define$data$header <- dat_header

  # return object
  return(md_define)
}

#' Process Qualtrics dataset headers
#'
#' @param md_define A list containing the data and processed data
#' @param n_word_match An integer. Number of words match for the maxdiff question
#' @returns A list containing the data and processed data
#' @export
#' @import dplyr
#' @import forcats
#' @examples
#' read_qualtrics_header(md_define = md_define)
read_qualtrics_header <- function(md_define = NULL, n_word_match = 10) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process!")
  }

  # read in header for processing
  dat_header <- md_define$data$header

  # extract first X words of the descriptions
  # count number of other variables have the same descriptions
  n_extractwords <- n_word_match
  dat_header <- dat_header %>%
    # trim extra spaces
    mutate(describe_extract = str_trim(describe)) %>%
    mutate(describe_extract = str_replace_all(describe_extract, "[:space:]+", " ")) %>%
    # mutate(describe_extract = str_extract(
    #   describe_extract,
    #   paste0(rep("\\w+", n_extractwords),
    #     c(rep("[:space:]", n_extractwords - 1), ""),
    #     collapse = ""
    #   )
    # )
    mutate(describe_extract = str_split_fixed(describe_extract, " - ",2)[,1]) %>%
    mutate(describe_extract = str_extract_all(describe_extract, "\\w+")
    )

  dat_header$describe_extract <-
    lapply(dat_header$describe_extract, function(x){paste0(unlist(x), collapse = "|")})
  dat_header <- dat_header %>%
    mutate(describe_extract = str_extract(describe_extract,
                                          paste0(rep("\\w+", n_extractwords), collapse = "\\|")))

  # summarize number of "likely" maxdiff questions
  md_q_describe <- dat_header %>%
    count(describe_extract, name = "n") %>%
    filter(!is.na(describe_extract)) %>%
    arrange(desc(n))

  # check if there are enough "repeated" questions + option combinations
  if (nrow(md_q_describe) == 0 | md_q_describe$n[1] <= 5) {
    message("No MaxDiff columns found! Check if your MaxDiff questions are consistent.")
  }

  # extract the descriptions of maxdiff questions
  md_q_describe <- md_q_describe %>%
    slice(1) %>%
    pull(describe_extract)

  # label all maxdiff questions
  dat_header <- dat_header %>%
    mutate(label = if_else(str_count(describe_extract, md_q_describe) >
                             str_count(md_q_describe, "\\|"),
                           "md", "", NA_character_))

  # question text
  question <- dat_header %>%
    filter(str_detect(describe, "Display Order")) %>%
    mutate(loop = 1:n()) %>%
    mutate(q_text = str_replace(describe, "Display Order$", ""))

  # filter only maxdiff questions
  dat_header <- dat_header %>%
    select(-describe_extract) %>%
    filter(label == "md")

  # message for question checking
  if (length(unique(question$q_text)) == 1) {
    message(paste("Detected MaxDiff question text:\n", question$q_text[1]))
  } else {
    message(paste(
      "Detected multiple versions of MaxDiff question text:\n",
      paste(question$q_text, sep = "\n")
    ))
  }

  # extract attributes
  for (i in unique(question$q_text)) {
    dat_header <- dat_header %>%
      mutate(text = sub(
        pattern = i, replacement = "",
        x = dat_header$describe, fixed = TRUE
      ))
  }

  # convert options to number
  dat_header <- dat_header %>%
    mutate(id = as.integer(fct_inorder(factor(text))))

  # save number of attributes, not counting the DO columns
  n_attributes <- as.integer(max(as.integer(dat_header$id))) - 1

  # change display order label code to DO
  dat_header <- dat_header %>%
    mutate(id = if_else(str_detect(text, "Display Order"), "DO", as.character(id), NA_character_))

  dat_trials <- dat_header %>%
    count(id, name = "n")

  if (var(dat_trials$n) > 0) {
    warning("Some of the options show up more than others!")
  }

  n_trials <- median(dat_trials$n)

  # label loop numbers
  dat_header <- dat_header %>%
    mutate(loop = rep(1:n_trials, each = (n_attributes + 1))) %>%
    mutate(name_new = paste(loop, label, id, sep = "_"))

  # attributes
  dat_attributes <- dat_header %>%
    filter(loop == 1) %>%
    filter(id != "DO") %>%
    select(id, text) %>%
    pull(text)

  # simplify header
  dat_header <- dat_header %>%
    select(name, describe, importId, text, name_new)

  # message
  message(paste0("Number of attributes: ", n_attributes))
  message(paste0("Number of trials: ", n_trials))

  # save to data
  md_define$data$header <- dat_header
  md_define$n$attributes <- n_attributes
  md_define$n$trials <- n_trials
  md_define$question <- question$q_text[1]
  md_define$attributes <- dat_attributes

  # return object
  return(md_define)
}
