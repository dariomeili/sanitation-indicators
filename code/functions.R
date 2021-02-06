# Date: 6.2.2021
# R Script defining all function to run analysis.R
#
######################
# Confidence intervals
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
#
######################
# robust standard errors
elm <-  function(model, ...) {
  extract.lm_robust(model,
                    include.ci = F,
                    include.rmse = F)
}

######################
# function for linebreaks in facet labels
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}
