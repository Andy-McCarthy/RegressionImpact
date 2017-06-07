#' @title Visualize Regression Variables
#'
#' @description Function to visually display the significance of each predictor variable in
#' a regression. The function takes an lm() object as input and outputs a ggplot2-labeled
#' figure.
#'
#' @param model an lm() object
#' @param measure use P-values (P), coefficients (C), or percent change (T) to construct plot
#'
#' @return a ggplot2 object
#'
#' @examples
#' X <- runif(500,1,5)
#' Y <- runif(500,6,10)
#' Z <- runif(500,-5,0)
#'
#' W <- rnorm(500,X+2*Y,5)
#'
#' model <- lm(W~X+Y+Z)
#'
#' visualize(model)
#' visualize(model, measure = "C")
#' visualize(model, "T")
#'
#' @export

visualize <- function(model, measure = "P") {

  # Extract variable names
  var.names <- attr(model$model, "names")[2:model$rank]

  # Extract P-values
  Pvals <- summary(model)$coefficients[2:model$rank,4]

  # initialize impact
  imp <- rep(NA,(model$rank-1))

  # depending on whether the user wants P-values or coefficients,
  # determine what is to be plotted
  if (measure == "P") {
    for (i in 1:(model$rank-1)) {
      # determine if positive or negative
      if (model$coefficients[i+1] > 0) {
        imp[i] <- 1 - Pvals[i]
      } else {
        imp[i] <- -(1 - Pvals[i])
      }
    }
  } else if (measure == "C") {
    imp <- model$coefficients[2:model$rank]
  } else if (measure == "T") {
    imp <- coef(model)[2:model$rank]/mean(model$fitted.values)
  } else {
    message("For the measure argument, please enter 'P' for P-values, 'C' for coefficients,")
    message("or 'T' for percent change.")
    return()
  }

  # construct data frame
  pct <- data.frame(
    num = 1:(model$rank-1),
    name = var.names,
    impact = imp
  )

  # plot results
  gg.p <- ggplot2::ggplot(pct, ggplot2::aes(x = impact, y = num)) +
    ggplot2::geom_text(ggplot2::aes(label=pct$name, size = 14)) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(-1,-max(abs(pct$impact))),
                                                        to = max(1,max(abs(pct$impact)), by = 0.2)),
                                limits = c(min(-1,-max(abs(pct$impact))-.1),
                                           max(1,max(abs(pct$impact))+.1))) +
    ggplot2::scale_y_discrete(limits = c(0,model$rank)) +
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0, color = "red")) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = -1, color = "blue")) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 1, color = "blue"))

  # return plot
  return(gg.p)

}
