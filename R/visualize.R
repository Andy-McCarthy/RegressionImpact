#' @title Visualize Regression Variables
#'
#' @description Function to visually display the significance of each predictor
#' variable in a regression. The function takes an lm() object as input and
#' outputs a ggplot2-labeled figure.
#'
#' @param model an lm() object
#' @param measure use P-values (P) or coefficients (C) to construct plot
#' @param signif significance level for P-values
#' @param sorted whether the variables should be ordered by significance
#'
#' @return a ggplot2 object
#'
#' @examples
#' X <- runif(500,1,5)
#' Y <- runif(500,6,10)
#' Z <- runif(500,-5,0)
#'
#' W <- rnorm(500,2*Y-X,5)
#'
#' model <- lm(W~X+Y+Z)
#'
#' visualize(model)
#' visualize(model, signif = 0.1)
#' visualize(model, measure = "C")
#' visualize(model, measure = "C", sorted = F)
#'
#' @export

visualize <- function(model, measure = "P", signif = 0.05, sorted = TRUE) {

  # extract variable names
  var.names <- attr(model$model, "names")[2:model$rank]

  # initialize impact
  imp <- rep(NA,(model$rank-1))

  # depending on whether the user wants P-values or standardized coefficients,
  # determine what is to be plotted
  if (measure == "P") {
    imp <- summary(model)$coefficients[2:model$rank,4]
  } else if (measure == "C") {
    # standardize coefficients
    coeff <- summary(model)$coef[-1, 1]
    stdev <- rep(NA,model$rank)
    for (i in 1:ncol(model$model)) {
      stdev[i] <- sd(model$model[,i])
    }
    imp <- coeff * stdev[2:model$rank]/stdev[1]
  } else {
    message("For the measure argument, please enter 'P' for P-values or 'C'")
    message("for Standardized Coefficients.")
    return()
  }

  # construct data frame
  pct <- data.frame(
    name = var.names,
    impact = imp,
    order = 1:(model$rank-1)
  )

  # add the colors
  if (measure == "P") {
    pct$sig <- rep("black",nrow(pct))
    for (j in 1:nrow(pct)) {
      if (pct$impact[j] < signif) {pct$sig[j] <- "red"}
    }
  }

  # plot results-----------------------------------------------------
  if (sorted == T) {
  # IF THE PLOT SHOULD BE SORTED
  gg.p <- switch(measure,
    #P-value------------
    "P" = ggplot2::ggplot() +
      # plot the actual P-values
      ggplot2::geom_point(data = pct,
                          ggplot2::aes(x = impact, y = reorder(name,-impact)),
                          color = pct$sig) +
      # guidelines at 0, 1, and the significance level
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = "skyblue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = signif), color = "red") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), color = "skyblue") +
      # plot aesthetics
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                          labels = round(seq(from = 0, to = 1, by = 0.1),1)) +
      ggplot2::xlab("P-value") + ggplot2::ylab("Regression Variables") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     legend.position = "none"),

    #Coefficients-------
    "C" = ggplot2::ggplot() +
          ggplot2::geom_point(data = pct,
                   ggplot2::aes(x = impact, y = reorder(name,abs(impact)))) +
      # guidelines at -1, 0, and 1
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = "red") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = -1), color = "skyblue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), color = "skyblue") +
      # other plot aesthetics
      ggplot2::scale_x_continuous(limits = c(min(-1,-max(abs(pct$impact))-.1),
                                             max(1,max(abs(pct$impact))+.1))) +
      ggplot2::xlab("Standardized Coefficient") +
      ggplot2::ylab("Regression Variables") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     legend.position = "none")
  )
  # IF THE PLOT SHOULD NOT BE SORTED
  } else {
    gg.p <- switch(measure,
      #P-value------------
      "P" = ggplot2::ggplot() +
      # plot the actual P-values
      ggplot2::geom_point(data = pct,
                          ggplot2::aes(x = impact, y = reorder(name,-order)),
                          color = pct$sig) +
      # guidelines at 0, 1, and the significance level
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = "skyblue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = signif), color = "red") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), color = "skyblue") +
      # plot aesthetics
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                          labels = round(seq(from = 0, to = 1, by = 0.1),1)) +
      ggplot2::xlab("P-value") + ggplot2::ylab("Regression Variables") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     legend.position = "none"),

      #Coefficients-------
      "C" = ggplot2::ggplot() +
            ggplot2::geom_point(data = pct,
                                ggplot2::aes(x = impact, y = reorder(name,-order))) +
      # guidelines at -1, 0, and 1
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = "red") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = -1), color = "skyblue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), color = "skyblue") +
      # other plot aesthetics
      ggplot2::scale_x_continuous(limits = c(min(-1,-max(abs(pct$impact))-.1),
                                             max(1,max(abs(pct$impact))+.1))) +
      ggplot2::xlab("Standardized Coefficient") +
      ggplot2::ylab("Regression Variables") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     legend.position = "none")
    )
  }

  # return plot
  return(gg.p)

}
