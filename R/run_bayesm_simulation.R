#' `bayesm` wrapper: Run `rhierMnlRwMixture` simulation with the parsed format
#'
#' @param md_define A list containing a sublist of data
#' @param R An integer. Number of MCMC draws
#' @param keep An integer. Keeping every `keep`-th draw. Default: keeping 2000 draws.
#' @param seed An integer. Provide a seed for results reproduction
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' run_bayesm_simulation(md_define, R = 1e4, keep = 10, seed = NULL)
run_bayesm_simulation <- function(md_define = md_define, R = 1e4, keep = 10, seed = NULL) {
  # set parameters
  if (!is.null(seed)) {
    set.seed(seed)
  } # 19980904
  else {
    set.seed(runif(min = 1e4, max = 1e5))
  }

  # retrieve from md_define
  n_shown <- md_define$n$shown

  # put some constraints
  nvar <- md_define$n$attributes
  ncomp <- 1
  a <- rep(1, ncomp)
  # R   <- 1e4 # control by wrapper parameter
  keep_feed_to_bayesm <- min(keep, max(c(floor(R / 2e3), 1L))) # keeping every X-th of R to make up 2000 samples

  data <- list(lgtdata = md_define$data$bayesm, p = n_shown)
  prior <- list(ncomp = ncomp)
  mcmc <- list(R = R, nprint = 1e3, keep = keep_feed_to_bayesm)

  # load output
  # run the analysis
  bayesm.output <- bayesm::rhierMnlRwMixture(Data = data, Prior = prior, Mcmc = mcmc)

  # save to data
  md_define$output$bayesm <- bayesm.output

  # return object
  return(md_define)
}

#' Making trace plot for the MCMC draws
#'
#' @param md_define A list containing a sublist of data
#' @param label_width An integer. Capping label length
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_trace(md_define)
plot_trace <- function(md_define = NULL, label_width = 30) {
  # save to md_define
  if (is.null(md_define)) {
    warning("No data frame to process! ")
  }

  # load data from md_define for processing
  dat_bayesm_compdraw <- md_define$output$bayesm$nmix$compdraw
  n_attributes <- md_define$n$attributes
  dat_attribute <- md_define$attributes

  # looping over the iterations
  dat_compdraw <- NULL
  for (i in 1:length(dat_bayesm_compdraw)) {
    dat_tmp <- dat_bayesm_compdraw[[i]][[1]]$mu
    names(dat_tmp) <- c(paste0("V", 1:length(dat_tmp)))
    dat_compdraw <- bind_rows(dat_compdraw, dat_tmp)
  }

  dat_compdraw <- dat_compdraw %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(
      cols = -iteration,
      names_to = "variable",
      values_to = "beta"
    )

  (fig <- dat_compdraw %>%
    ggplot(aes(x = iteration, y = beta, color = variable)) +
    # geom_point() +
    geom_line(alpha = .5) +
    scale_x_continuous(name = "Iterations after thinning", expand = c(0, 0)) +
    ylab("Average beta coefficients") +
    scale_color_discrete(
      name = NULL,
      breaks = paste0("V", 1:(n_attributes - 1)),
      labels = str_trim(
        str_trunc(
          dat_attribute[1:(n_attributes - 1)],
          width = label_width, ellipsis = "..."))
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey90"),
      panel.background = element_blank(),
      plot.background = element_blank()
    ))

  # save to data
  print(fig)
  md_define$plots$trace <- fig

  # return object
  return(md_define)
}
