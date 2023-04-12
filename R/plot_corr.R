#' process data to plot attribute correlations
#'
#' @param md_define A list containing a sublist of data
#' @param label_width An integer. Capping label length
#' @param burn_in burn in period before the simulated data are used
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_corr(md_define)
process_corr <- function(md_define = NULL, label_width = 30, burn_in = .5) {
  # retrieve beta weights
  attributes <- md_define$attributes
  dat_estbetas <- md_define$output$bayesm$betadraw
  n_attributes <- md_define$n$attributes

  # remove 10% as burn-in period
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

  # For notebook display
  corrMatrix <- cor(dat_prct)
  colnames(corrMatrix) <- paste0("(", str_extract(colnames(corrMatrix), "\\d+"), ")")
  rownames(corrMatrix) <- paste0(
    paste0("(", str_extract(colnames(corrMatrix), "\\d+"), ")"), " ",
    str_trunc(
      attributes[
        as.integer(str_extract(
          colnames(corrMatrix),
          "\\d+"
        ))
      ],
      width = label_width
    )
  )

  md_define$results$corrMatrix <- corrMatrix

  return(md_define)
}

#' plot attribute correlations
#'
#' @param md_define A list containing a sublist of data
#' @param n_clusters An integer. Capping label length
#' @returns A list containing the data and processed data
#' @export
#' @examples
#' plot_corr(md_define, n_clusters = 3)
plot_corr <- function(md_define = NULL, n_clusters = 1) {
  # google colors
  google_color <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

  dat_corrMatrix <- md_define$results$corrMatrix

  corrplot::corrplot(dat_corrMatrix,
    order = "hclust", # order="hclust", hclust.method = "centroid", addrect = 9,
    addgrid.col = NA,
    bg = element_blank(),
    col = c(
      colorRampPalette(c(google_color[2], "white"))(20),
      colorRampPalette(c("white", google_color[1]))(20)
    ),
    # col = colorRampPalette(c(google_color[2], google_color[1]))( 10 ),
    hclust.method = c("ward.D"),
    tl.col = "grey25", tl.cex = 0.8,
    # tl.pos="ld",
    tl.srt = 90, tl.offset = .5,
    addrect = n_clusters, # If order = "hclust", number of cluster rectangles
    rect.col = "grey75", # Color of the rectangles
    rect.lwd = 1,
    type = "full", cl.pos = "r", cl.cex = 0.5
  ) +
    theme_minimal(base_size = 5)
}
