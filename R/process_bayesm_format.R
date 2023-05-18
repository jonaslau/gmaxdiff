#' Convert data to bayesm format
#'
#' @param md_define A list containing a sublist of data
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
process_bayesm_format <- function(md_define = NULL) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  dat_maxdiff <- md_define$data$maxdiff
  n_attributes <- md_define$n$attributes

  # get a list of respondents
  respondent_id <- unique(dat_maxdiff$ResponseId)

  dat_bayesm <- vector("list", length(respondent_id))

  # creating some visuals for progress
  cat("Converting data into bayesm format:\n")
  cat("0%\t\t\t\t\t100%\n")
  cat("|", sep = "")

  # Loop over each respondent to create a list
  for (i in 1:length(respondent_id)) {
    # progress visual, capture the moment of cross each X%
    if (((i / length(respondent_id) * 100) %% 5) >
      (((i + 1) / length(respondent_id) * 100) %% 5)) {
      cat(paste0(rep("=", times = 2)), sep = "")
    }

    # create a filtered data frame for each respondent
    dat_tmp <- dat_maxdiff %>%
      # only the best case included if uncommented
      # filter(polarity == 2L) %>%
      filter(ResponseId == respondent_id[i])

    # log the ResponseId
    dat_bayesm[[i]]$ResponseId <- respondent_id[i]

    # Pull all the responses, in the attribute code
    dat_bayesm[[i]]$y <- dat_tmp %>%
      select(choice_order, chosen) %>%
      filter(chosen == 1L) %>%
      pull(choice_order)

    # the X matrix: attribute data frame
    dat_bayesm[[i]]$X <- dat_tmp %>%
      select(paste0("a__", 1:(n_attributes - 1))) %>%
      as.matrix()
  }
  cat("|\n")

  # remove intermediate variable
  rm(dat_tmp)

  # save to data
  md_define$data$bayesm <- dat_bayesm

  # return object
  return(md_define)
}
