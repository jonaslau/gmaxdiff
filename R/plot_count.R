#' Count plot
#'
#' @param md_define A list containing a sublist of data
#' @param label_width maximum width of text labels
#' @returns A list containing the data and processed data
#' @export
#' @import cowplot
#' @examples
#' plot_aggregate_betas(md_define)
plot_count <- function(md_define = NULL, label_width = 30) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  # google colors
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  # retrieve info for processing
  dat_mdformat <- md_define$data$maxdiff
  n_attributes <- md_define$n$attributes
  dat_attribute <- md_define$attributes
  n_shown <- md_define$n$shown

  dat_appear <- dat_mdformat %>%
    mutate(attribute = as.integer(attribute)) %>%
    select(polarity, attribute) %>%
    group_by(polarity) %>%
    count(attribute, name = "n_appear")
  # %>%
  # filter(polarity == 1) %>%
  # ungroup() %>%
  # select(-polarity)

  dat_selected <- dat_mdformat %>%
    filter(chosen == 1L) %>%
    mutate(attribute = as.integer(attribute)) %>%
    select(polarity, attribute) %>%
    group_by(polarity) %>%
    count(attribute, name = "n_selected")
  # %>%
  # pivot_wider(id_cols = attribute,
  #             names_from = polarity,
  #             names_prefix = "c__",
  #             values_from = n_selected)

  dat <- dat_appear %>%
    left_join(dat_selected, by = c("attribute", "polarity")) %>%
    mutate(prct = n_selected / n_appear) # %>%
  # ungroup() %>%
  # mutate(prct = prct / n_shown)

  dat[is.na(dat)] <- 0

  dat_diff <- dat %>%
    # pivot_wider(id_cols = "attribute",
    #             names_from = "polarity",
    #             names_prefix = "c__",
    #             values_from = "prct") %>%
    # mutate(diff = c__2 - c__1)
    group_by(attribute) %>%
    summarise(prct = diff(prct)) %>%
    arrange(prct)

  dat <- dat %>%
    mutate(panel = 2) %>%
    bind_rows(dat_diff %>% mutate(panel = 1))

  dat_order <- dat_diff %>%
    # filter(polarity == 2) %>%
    # mutate(attribute = fct_reorder(factor(attribute, 1:n_attributes, ordered = TRUE), prct))
    arrange(prct)

  dat <- dat %>%
    mutate(attribute = factor(attribute, dat_order$attribute, ordered = TRUE))

  fig1 <- dat %>%
    filter(panel == 1) %>%
    # mutate(ci_lower = mean - se * 1.96,
    #        ci_lower = if_else(ci_lower < 0, 0, ci_lower),
    #        ci_upper = mean + se * 1.96) %>%
    # mutate(text = factor(text, dat_agg_option, ordered = TRUE), mean) %>%
    # mutate(group = factor(as.character(group), ordered = TRUE)) %>%
    ggplot(aes(x = attribute, y = prct, color = polarity)) + # , alpha = group
    # geom_errorbar(data = dat_plot_group %>% filter(group == 0L), aes(ymin=0, ymax=mean),
    #               width=0,
    #               color = "grey90", linetype = "dotted") +
    # geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper),
    #               width=0,
    #               color = "grey75", position = pd) +
    # geom_point(size = 3) + # , position = pd
    geom_bar(stat = "identity", fill = "grey75", color = NA) +
    coord_flip() +
    geom_hline(yintercept = 0, color = google_color[1], linetype = 2) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.justification = c(.01, .99),
      legend.position = c(.01, .99),
      strip.background = element_blank()
    ) +
    scale_x_discrete(
      name = NULL,
      breaks = paste0(1:n_attributes),
      labels = str_trim(
        str_trunc(
          dat_attribute,
          width = label_width, ellipsis = "..."))
    ) +
    scale_y_continuous(
      name = "Difference (%)",
      breaks = seq(-1, 1, .1),
      limits = c((floor(min(dat$prct) * 100) - 1) / 100, (ceiling(max(dat$prct) * 100) + 1) / 100),
      expand = c(0, 0),
      labels = function(x) {
        x * 100
      }
    )

  fig2 <- dat %>%
    filter(panel == 2) %>%
    # mutate(ci_lower = mean - se * 1.96,
    #        ci_lower = if_else(ci_lower < 0, 0, ci_lower),
    #        ci_upper = mean + se * 1.96) %>%
    # mutate(text = factor(text, dat_agg_option, ordered = TRUE), mean) %>%
    # mutate(group = factor(as.character(group), ordered = TRUE)) %>%
    ggplot(aes(x = attribute, y = prct, color = as.factor(polarity))) + # , alpha = group
    # geom_errorbar(data = dat_plot_group %>% filter(group == 0L), aes(ymin=0, ymax=mean),
    #               width=0,
    #               color = "grey90", linetype = "dotted") +
    # geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper),
    #               width=0,
    #               color = "grey75", position = pd) +
    geom_point(size = 3) + # , position = pd
    coord_flip() +
    # geom_hline(yintercept = 0, color = google_color[1], linetype = 2) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      # legend.justification = c(.99, .5),
      legend.position = "right",
      strip.background = element_blank(),
      axis.text.y = element_blank()
    ) +
    scale_x_discrete(
      name = NULL,
      breaks = paste0(1:n_attributes),
      labels = str_trim(
        str_replace(
          str_trunc(
            dat_attribute,
            width = label_width, ellipsis = ""
          ),
          "\\W+$|[:space:]\\w+$", "..."
        ),
      )
    ) +
    scale_y_continuous(
      name = "Percentage",
      breaks = seq(0, 1, .05),
      limits = c(0, (ceiling(max(dat$prct) * 100) + 1) / 100),
      expand = c(0, 0),
      labels = function(x) {
        x * 100
      }
    ) +
    scale_color_manual(
      name = "", # manual
      breaks = c(2, 1),
      labels = c("Positive", "Negative"),
      values = c(google_color[1], google_color[2])
    )

  fig <- plot_grid(fig1, fig2, rel_widths = c(1.2, 1), align = "h")

  # save to data
  print(fig)
  md_define$plots$count <- fig

  # return object
  return(md_define)
}
