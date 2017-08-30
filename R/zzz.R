.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()
  packageStartupMessage("Happy Visualizing!",
                        "We recommend the great book Mutliple Correspondence Analysis by Brigitte Le Roux and Henry Roueant (2010).",
                        "https://us.sagepub.com/en-us/nam/multiple-correspondence-analysis/book233035")
}
