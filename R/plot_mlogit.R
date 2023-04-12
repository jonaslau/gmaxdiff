#' Wrapper for `mlogit` for data checking
#'
#' @param md_define A list containing a sublist of data
#' @returns A list containing the data and processed data
#' @import mlogit
#' @import dfidx
#' @export
#' @examples
#' run_mlogit(md_define)
run_mlogit <- function(md_define) {
  dat_maxdiff <- md_define$data$maxdiff
  n_attributes <- md_define$n$attributes
  n_trials <- md_define$n$trials
  dat_attribute <- md_define$attributes

  vector_attribute <- dat_attribute
  names(vector_attribute) <- paste0("a__", 1:length(vector_attribute))

  # retain only relevant columns for mlogit
  dat_maxdiff <- dat_maxdiff %>%
    mutate(ResponseId = fct_inorder(factor(ResponseId))) %>%
    mutate(trial = (as.integer(ResponseId) - 1) * n_trials * 2 +
      as.integer(trial) + n_trials * (as.integer(polarity) - 1)) %>%
    select(ResponseId, trial, choice_order, starts_with("a__"), chosen) %>%
    mutate(chosen = as.logical(chosen)) %>%
    arrange(ResponseId, trial, choice_order)

  dat_mlogit <- dfidx(dat_maxdiff,
    idx = c("trial", "choice_order"), # "ResponseId",
    choice = "chosen", shape = "long"
  )

  # writ a more generic form of the formula
  f <- as.formula(paste0(
    "chosen ~ ",
    paste(paste0("a__", 1:(n_attributes - 1)), collapse = " + "),
    " | 0"
  ))
  model <- mlogit(formula = f, data = dat_mlogit)

  md_define$results$mlogit <- summary(model)
  return(md_define)
}

#' Plotting `mlogit` results for checking
#'
#' @param md_define A list containing a sublist of data
#' @param label_width An integer. Capping label length
#' @param point_color A string denoting the color of the plotted data
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_mlogit_betas(md_define, label_width = 30, point_color = c("red", "blue"))
plot_mlogit_betas <- function(md_define, label_width = 30, point_color = NULL) {
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  # # check input arguments
  # if(is.null(point_color)) point_color = google_color[1]
  #
  # # retrieve data for processing
  # dat_plot = md_define$results$dat_aggregate_betas
  dat_attribute <- md_define$attributes
  n_attributes <- md_define$n$attributes

  dat_mlogit_result <- as.data.frame(md_define$results$mlogit$CoefTable)
  dat_mlogit_result$variable <- rownames(dat_mlogit_result)
  # names(dat_mlogit_result) = c("estimate", "se", "z_value", "prob", "variable")
  names(dat_mlogit_result) <- str_replace_all(names(dat_mlogit_result), "\\W+", "_")
  names(dat_mlogit_result) <- str_replace_all(names(dat_mlogit_result), "_$", "")

  # process attributes order
  dat_mlogit_result <- dat_mlogit_result %>%
    # mutate(variable = fct_reorder(factor(variable, ordered = TRUE), .x = Estimate)) %>%
    arrange(Estimate) %>%
    mutate(variable = fct_inorder(factor(variable, ordered = TRUE)))

  # aggregate plot
  fig <- dat_mlogit_result %>%
    ggplot(aes(x = variable, y = Estimate)) + # , colour=supp
    geom_errorbar(aes(ymin = 0, ymax = Estimate),
      width = 0,
      color = "grey90", linetype = "dotted"
    ) +
    geom_errorbar(aes(ymin = Estimate - Std_Error * 1.96, ymax = Estimate + Std_Error * 1.96), # 95% CI
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
        floor((min(dat_mlogit_result$Estimate - dat_mlogit_result$Std_Error * 1.96) - .15) * 1000) / 1000,
        ceiling((max(dat_mlogit_result$Estimate + dat_mlogit_result$Std_Error * 1.96) + .15) * 1000) / 1000
      ),
      # labels = function(x){x*100},
      expand = c(0, 0)
    )

  # save output
  print(fig)
  md_define$plots$mlogit_betas <- fig

  return(md_define)
}
