#' Individual estimation
#'
#' @param md_define A list containing a sublist of data
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_individual_prct(md_define)
process_individual_prct <- function(md_define = NULL) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # burn in period
  burn_in <- .5

  # retrieve beta weights
  dat_estbetas <- md_define$output$bayesm$betadraw
  n_attributes <- md_define$n$attributes

  # remove X% as burn-in period
  # n_keep = dim(dat_estbetas)[3]
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
  # dat_plot = dat_prct %>%
  #   # select(starts_with("V")) %>%
  #   apply(MARGIN = 2, FUN = function(x){
  #     tibble(mean = mean(x), se = sd(x)/sqrt(length(x)))
  #   }) %>%
  #   bind_rows() %>%
  #   mutate(variable = names(dat_prct %>% select(starts_with("a__"))))

  # save data
  md_define$results$individual_prct <- dat_prct
  return(md_define)
}

#' Plot individual distribution
#'
#' @param md_define A list containing a sublist of data
#' @param label_with An integer. Capping label length
#' @param ncol Number of column in the plots
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_attribute_distribution(md_define)
plot_attribute_distribution <- function(md_define, label_width = 30, ncol = 2) {
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  dat_plot <- md_define$results$individual_prct
  dat_attribute <- md_define$attribute
  n_attributes <- md_define$n$attributes

  dat_plot <- dat_plot %>%
    mutate(ResponseId = 1:n())

  dat_plot <- dat_plot %>%
    pivot_longer(
      cols = -ResponseId,
      names_to = "attribute",
      values_to = "prct"
    )

  # format attribute to be just numbers
  dat_plot <- dat_plot %>%
    mutate(attribute = as.integer(str_replace(attribute, "a__", "")))

  strip_label <- str_trim(str_replace(
    str_trunc(
      dat_attribute,
      width = label_width, ellipsis = "<trunc>"
    ),
    "([:space:]\\w+)<trunc>$|([:space:])<trunc>$", "..."
  ))
  names(strip_label) <- paste0("a__", 1:n_attributes)

  fig <- vector("list", ceiling(n_attributes / 6))
  for (i in 1:ceiling(n_attributes / 6)) {
    fig[[i]] <- local({
      # list of ggplots
      # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
      i <- i
      tmp_fig <- dat_plot %>%
        filter(attribute %in% ((i - 1) * 6 + 1):min(((i - 1) * 6 + 6), n_attributes)) %>%
        # mutate(column = attribute %% 6) %>%
        # mutate(attribute = factor(attribute, dat_attribute$id,
        #                           str_trim(
        #                             str_replace(str_trunc(
        #                               dat_attribute$text, width = label_width, ellipsis = "<trunc>"),
        #                               "([:space:]\\w+)<trunc>$|([:space:])<trunc>$", "..."),
        #                             ), ordered = TRUE)) %>%
        mutate(attribute = paste0("a__", attribute)) %>%
        ggplot(aes(x = prct)) +
        geom_histogram(binwidth = .01, fill = "#F4B400") +
        coord_cartesian(xlim = c(0, .3)) +
        scale_x_continuous(
          name = "Percentage",
          # limits = c(0, .30),
          breaks = seq(0, 1, .05),
          expand = c(0, 0),
          labels = function(x) {
            x * 100
          }
        ) +
        facet_wrap(vars(attribute), scales = "free_y", ncol = 2, nrow = 3, labeller = labeller(attribute = strip_label)) +
        geom_vline(xintercept = 1 / n_attributes, color = google_color[1], linetype = 2) +
        theme_minimal() +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank()
        )
      # print(tmp_fig)
    })
  }

  # save data
  md_define$plots$individual_distribution <- fig
  return(md_define)
}
