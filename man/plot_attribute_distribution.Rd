% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_individual.R
\name{plot_attribute_distribution}
\alias{plot_attribute_distribution}
\title{Plot individual distribution}
\usage{
plot_attribute_distribution(md_define = NULL, label_width = 30, ncol = 2)
}
\arguments{
\item{md_define}{A list containing a sublist of data}

\item{label_width}{An integer. Capping label length}

\item{ncol}{Number of column in the plots}
}
\value{
A list containing the data and processed data
}
\description{
Plot individual distribution
}
\examples{
file_path <- system.file("extdata", "icecream.csv", package = "gmaxdiff")
md_define <- read_qualtrics_data(file_path)
md_define <- read_qualtrics_header(md_define = md_define)
md_define <- process_attribute_text(md_define = md_define)
md_define <- process_maxdiff_question_format(md_define = md_define, signchange = FALSE)
md_define <- process_bayesm_format(md_define = md_define)
md_define <- run_bayesm_simulation(md_define, R = 2e3)
md_define <- process_individual_prct(md_define)
md_define <- plot_attribute_distribution(md_define)
}
