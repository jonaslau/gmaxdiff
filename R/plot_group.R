#' Group data processing
#'
#' @param md_define A list containing a sublist of data
#' @param group_var A string denoting the variable in the data set header
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
#' md_define <- run_bayesm_simulation(md_define, R = 2e3)
#' md_define <- process_group_prct(md_define, group_var = "Q18")
process_group_prct <- function(md_define = NULL, group_var = NULL, burn_in = .5) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # retrieve beta weights
  dat <- md_define$data$raw
  dat_bayesm <- md_define$data$bayesm
  dat_estbetas <- md_define$output$bayesm$betadraw
  n_attributes <- md_define$n$attributes
  n_respondents <- md_define$n$respondents

  # get a list of respondents
  dat_responseId <- NULL
  for (i in 1:length(dat_bayesm)) {
    dat_tmp <- tibble(
      order = i,
      id = dat_bayesm[[i]]$ResponseId
    )
    dat_responseId <- bind_rows(dat_responseId, dat_tmp)
  }

  # remove X% as burn-in period
  # n_keep = dim(dat_estbetas)[3]
  dat_estbetas <- dat_estbetas[, , (floor(dim(dat_estbetas)[3] * burn_in):dim(dat_estbetas)[3])]

  group <- NULL
  group_random <- NULL
  if (is.null(group_var)) {
    # generate some random groups for testing
    group <- sample(1:3, size = n_respondents, replace = TRUE)
    group_random <- TRUE
  } else {
    dat_rawgroup <- dat %>%
      select(ResponseId, group = matches(paste0("^", group_var, "$")))
    # merge with appearance order
    group <- dat_responseId %>%
      left_join(dat_rawgroup, by = c("id" = "ResponseId")) %>%
      pull(group)
    group_random <- FALSE
  }

  # unique groups
  unique_groups <- tibble(
    group = sort(unique(as.character(group))),
    group_id = as.integer(factor(group))
  )

  # Post-bayesian sampling analysis
  # retrieve beta weights: taking the median instead of mean
  dat_estbetas <- apply(dat_estbetas, c(1, 2), median)

  # attach a column of 0 for the reference column
  dat_estbetas <- bind_cols(dat_estbetas, 0)
  names(dat_estbetas) <- paste0("a__", 1:n_attributes)

  # convert to empirical vs expected percentages
  dat_prct <- exp(dat_estbetas) / apply(exp(dat_estbetas), MARGIN = 1, FUN = sum)

  # attach group membership
  dat_prct <- dat_prct %>%
    mutate(group = group)

  # Plot by group
  dat_plot_group <- NULL

  # calculate percentages based on group
  for (i in unique_groups$group) {
    # data for each group
    dat_plot_tmp <- dat_prct %>%
      filter(group == i) %>%
      select(-group) %>%
      apply(MARGIN = 2, FUN = function(x) {
        tibble(mean = mean(x), se = sd(x) / sqrt(length(x)))
      }) %>%
      bind_rows() %>%
      mutate(group = i, variable = names(dat_prct %>% select(starts_with("a__"))))

    # dat_plot_tmp = dat_plot_tmp %>%
    #   mutate(variable = as.integer(str_replace(variable, "a__", "")))

    dat_plot_group <- bind_rows(
      dat_plot_group,
      dat_plot_tmp %>%
        select(group, variable, everything())
    )
  }

  # recode group variables
  dat_plot_group <- dat_plot_group %>%
    mutate(group = as.integer(factor(group, unique_groups$group,
      unique_groups$group_id,
      ordered = TRUE
    )))

  # save data
  md_define$results$group_prct <- dat_plot_group
  md_define$results$unique_groups <- unique_groups
  md_define$group_random <- group_random
  return(md_define)
}

#' Group plot and save data
#'
#' @param md_define A list containing a sublist of data
#' @param label_width maximum width of text labels
#' @param include The groups to include. Must match exact wording in data.
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
#' md_define <- read_qualtrics_data(file_path)
#' md_define <- read_qualtrics_header(md_define = md_define)
#' md_define <- process_attribute_text(md_define = md_define)
#' md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
#' md_define <- process_bayesm_format(md_define = md_define)
#' md_define <- run_bayesm_simulation(md_define, R = 2e3)
#' md_define <- process_aggregate_prct(md_define = md_define)
#' md_define <- process_group_prct(md_define, group_var = "Q18")
#' md_define <- plot_group_prct(md_define, label_width = 30, include = c(1,2))
plot_group_prct <- function(md_define, label_width = 30, include = NULL) {
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  # retrieve data for processing
  unique_groups <- md_define$results$unique_groups
  group_random <- md_define$group_random
  dat_plot_group <- md_define$results$group_prct
  dat_attribute <- md_define$attributes
  n_attributes <- md_define$n$attributes

  # exclude groups if any
  if(!is.null(include)){
    dat_plot_group <- dat_plot_group %>%
      filter(group %in% include)
  }


  # aggregate
  dat_plot <- md_define$results$aggregate_prct

  # check order
  dat_plot_group <- dat_plot %>%
    mutate(group = 0L) %>%
    bind_rows(dat_plot_group)

  # attribute order
  attribute_order <- dat_plot %>%
    arrange(mean) %>%
    pull(variable)

  # sort attribute order
  dat_plot_group <- dat_plot_group %>%
    mutate(variable = factor(variable, attribute_order, ordered = TRUE))

  # arrange group order
  unique_groups <- unique_groups %>%
    bind_rows(tibble(
      group = "Total",
      group_id = 0L
    )) %>%
    arrange(group_id)

  # sort group order
  dat_plot_group <- dat_plot_group %>%
    mutate(group = factor(group, unique_groups$group_id, ordered = TRUE))

  pd <- position_dodge(.5) # move them .05 to the left and right

  fig <- dat_plot_group %>%
    mutate(
      ci_lower = mean - se * 1.96,
      ci_lower = if_else(ci_lower < 0, 0, ci_lower),
      ci_upper = mean + se * 1.96
    ) %>%
    # mutate(text = factor(text, dat_agg_option, ordered = TRUE), mean) %>%
    # mutate(group = factor(as.character(group), ordered = TRUE)) %>%
    ggplot(aes(x = variable, y = mean, color = group, alpha = group)) +
    geom_errorbar(
      data = dat_plot_group %>% filter(group == 0L), aes(ymin = 0, ymax = mean),
      width = 0,
      color = "grey75", linetype = "dotted"
    ) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
      width = 0,
      color = "grey90", position = pd
    ) +
    geom_point(size = 3, position = pd) +
    coord_flip() +
    geom_hline(yintercept = 1 / n_attributes, color = google_color[1], linetype = 2) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.justification = c(.95, .05),
      legend.position = c(.95, .05)
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
      limits = c(0, min(round(max(dat_plot_group$mean + dat_plot_group$se * 1.96) + .01, 2), 1)),
      expand = c(0, 0),
      labels = function(x) {
        x * 100
      }
    ) +
    scale_alpha_manual(
      name = "", # manual
      breaks = unique_groups$group_id,
      labels = unique_groups$group,
      values = c(.25, rep(1, nrow(unique_groups)))
    )

  if (nrow(unique_groups) <= 4) {
    fig <- fig +
      scale_color_manual(
        name = "", # manual
        breaks = unique_groups$group_id,
        labels = unique_groups$group,
        values = google_color[1:nrow(unique_groups)]
      )
  } else {
    fig <- fig +
      scale_color_discrete(
        name = "", # manual
        breaks = unique_groups$group_id,
        labels = unique_groups$group # , values = google_color[1:4]
      )
  }

  # warning for random groups
  if (group_random) {
    fig <- fig +
      annotate(
        "text",
        label = "Random groups\nfor illustration",
        x = 1, y = 0, hjust = 0, vjust = 0, size = 8, colour = "red"
      )
  }

  # save data
  print(fig)
  md_define$plots$group_prct <- fig

  return(md_define)
}
