#' process aggregate betas
#'
#' @param md_define A list containing a sublist of data
#' @param burn_in burn in period before the simulated data are used
#' @returns A list containing the data and processed data
#' @import ggplot2
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
#' md_define <- run_bayesm_simulation(md_define, R = 5e2)
#' md_define <- process_aggregate_betas(md_define = md_define)
process_aggregate_betas <- function(md_define = NULL, burn_in = .5) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # retrieve data
  n_attributes <- md_define$n$attributes

  # retrieve beta weights
  dat_estbetas <- md_define$output$bayesm$betadraw
  # remove X% as burn-in period
  n_keep <- dim(dat_estbetas)[3]
  dat_estbetas <- dat_estbetas[, , (floor(dim(dat_estbetas)[3] * burn_in):dim(dat_estbetas)[3])]

  # Post-bayesian sampling analysis
  # retrieve beta weights: taking the median instead of mean
  dat_estbetas <- apply(dat_estbetas, c(1, 2), median) # median

  # attach a column of 0 for the reference column
  dat_estbetas <- bind_cols(dat_estbetas, 0)
  names(dat_estbetas) <- paste0("a__", 1:n_attributes)

  # convert to empirical vs expected percentages
  # dat_prct = exp(dat_estbetas)/apply(exp(dat_estbetas), MARGIN = 1, FUN = sum)
  # round(colMeans(dat_prct)*100, 1)

  # 95% sampling distributions
  dat_plot <- dat_estbetas %>%
    # select(starts_with("V")) %>%
    apply(MARGIN = 2, FUN = function(x) {
      tibble(mean = mean(x), se = sd(x) / sqrt(length(x)))
    }) %>%
    bind_rows() %>%
    mutate(variable = names(dat_estbetas %>% select(starts_with("a__"))))

  # save data
  md_define$results$aggregate_betas <- dat_plot
  return(md_define)
}

#' Aggregate plot and save data
#'
#' @param md_define A list containing a sublist of data
#' @param label_width An integer. Capping label length
#' @param point_color A string denoting the color of the plotted data
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
#' md_define <- run_bayesm_simulation(md_define, R = 5e2)
#' md_define <- process_aggregate_betas(md_define)
#' md_define <- plot_aggregate_betas(md_define, label_width = 30)
plot_aggregate_betas <- function(md_define = NULL, label_width = 30, point_color = NULL) {
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  # check input arguments
  if (is.null(point_color)) point_color <- google_color[1]

  # retrieve data for processing
  dat_plot <- md_define$results$aggregate_betas
  dat_attribute <- md_define$attributes
  n_attributes <- md_define$n$attributes

  # process attributes order
  dat_plot <- dat_plot %>%
    mutate(variable = fct_reorder(factor(variable, ordered = TRUE), .x = mean))

  # aggregate plot
  fig <- dat_plot %>%
    ggplot(aes(x = variable, y = mean)) + # , colour=supp
    geom_errorbar(aes(ymin = 0, ymax = mean),
      width = 0,
      color = "grey90", linetype = "dotted"
    ) +
    geom_errorbar(aes(ymin = mean - se * 1.96, ymax = mean + se * 1.96), # 95% CI
      width = 0,
      color = "grey75"
    ) +
    geom_point(color = google_color[1], size = 3) +
    coord_flip() +
    geom_hline(yintercept = 0, color = google_color[1], linetype = 2) + # 1/n_attributes
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) +
    scale_x_discrete(
      name = "",
      breaks = paste0("a__", 1:n_attributes),
      labels = str_trim(
        str_trunc(
          dat_attribute,
          width = label_width, ellipsis = "..."))
    ) +
    scale_y_continuous(
      name = "Aggregate beta weights",
      breaks = seq(-100, 100, 1),
      limits = c(
        floor((min(dat_plot$mean - dat_plot$se * 1.96) - .15) * 1000) / 1000,
        ceiling((max(dat_plot$mean + dat_plot$se * 1.96) + .15) * 1000) / 1000
      ),
      # labels = function(x){x*100},
      expand = c(0, 0)
    )

  # save output
  print(fig)
  md_define$plots$aggregate_betas <- fig

  return(md_define)
}

#' Post simulation analysis: Percentage
#'
#' @param md_define A list containing a sublist of data
#' @param burn_in burn in period before the simulated data are used
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
#' md_define <- run_bayesm_simulation(md_define, R = 5e2)
#' md_define <- process_aggregate_prct(md_define)
process_aggregate_prct <- function(md_define = NULL, burn_in = .5) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # retrieve data
  n_attributes <- md_define$n$attributes

  # retrieve beta weights
  dat_estbetas <- md_define$output$bayesm$betadraw
  # remove 10% as burn-in period
  n_keep <- dim(dat_estbetas)[3]
  dat_estbetas <- dat_estbetas[, , (floor(dim(dat_estbetas)[3] * burn_in):dim(dat_estbetas)[3])]

  # Post-bayesian sampling analysis
  # retrieve beta weights: taking the median instead of mean
  dat_estbetas <- apply(dat_estbetas, c(1, 2), median)

  # attach a column of 0 for the reference column
  dat_estbetas <- bind_cols(dat_estbetas, 0)
  names(dat_estbetas) <- paste0("a__", 1:n_attributes)

  # convert to empirical vs expected percentages
  dat_prct <- exp(dat_estbetas) / apply(exp(dat_estbetas), MARGIN = 1, FUN = sum)
  # round(colMeans(dat_prct)*100, 1)

  # 95% sampling distributions
  dat_plot <- dat_prct %>%
    # select(starts_with("V")) %>%
    apply(MARGIN = 2, FUN = function(x) {
      tibble(mean = mean(x), se = sd(x) / sqrt(length(x)))
    }) %>%
    bind_rows() %>%
    mutate(variable = names(dat_prct %>% select(starts_with("a__"))))

  # save data
  md_define$results$aggregate_prct <- dat_plot
  return(md_define)
}

#' Aggregate plot and save data
#'
#' @param md_define A list containing a sublist of data
#' @param label_width An integer. Capping label length
#' @param point_color A string denoting the color of the plotted data
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
#' md_define <- run_bayesm_simulation(md_define, R = 5e2)
#' md_define <- process_aggregate_prct(md_define)
#' md_define <- plot_aggregate_prct(md_define, label_width = 30)
plot_aggregate_prct <- function(md_define = NULL, label_width = 30, point_color = NULL) {
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  # check input arguments
  if (is.null(point_color)) point_color <- google_color[1]

  # retrieve data for processing
  dat_plot <- md_define$results$aggregate_prct
  dat_attribute <- md_define$attributes
  n_attributes <- md_define$n$attributes

  # process attributes order
  dat_plot <- dat_plot %>%
    mutate(variable = fct_reorder(factor(variable, ordered = TRUE), .x = mean))

  # aggregate plot
  fig <- dat_plot %>%
    mutate(
      ci_lower = mean - se * 1.96,
      ci_lower = if_else(ci_lower < 0, 0, ci_lower),
      ci_upper = mean + se * 1.96
    ) %>%
    ggplot(aes(x = variable, y = mean)) + # , colour=supp
    geom_errorbar(aes(ymin = 0, ymax = mean),
      width = 0,
      color = "grey90", linetype = "dotted"
    ) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
      width = 0,
      color = "grey75"
    ) +
    geom_point(color = google_color[1], size = 3) +
    coord_flip(ylim = c(0, ceiling((max(dat_plot$mean + dat_plot$se * 1.96) + .001) * 1000) / 1000)) + # ylim = c(0, 1)
    geom_hline(yintercept = 1 / n_attributes, color = google_color[1], linetype = 2) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) +
    scale_x_discrete(
      name = "",
      breaks = paste0("a__", 1:n_attributes),
      labels = str_trim(
        str_trunc(
          dat_attribute,
          width = label_width, ellipsis = "..."))
    ) +
    scale_y_continuous(
      name = "Percentage Share (%)",
      breaks = seq(0, 1, .05),
      limits = c(0, 1),
      expand = c(0, 0),
      labels = function(x) {
        x * 100
      }
    )

  # save output
  print(fig)
  md_define$plots$aggregate_prct <- fig

  return(md_define)
}
