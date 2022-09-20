#' Plot
#'
#' @param obj linreg object
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' data(iris)
#' linres <- linreg(formula=Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
#' plot(linres)
plot.linreg <- function(obj) {
  #----------------------------------------------------------------------------#
  #- Set data for plotting
  #----------------------------------------------------------------------------#
  df <- data.frame(cbind(pred(obj), residuals.linreg(obj)))

  #----------------------------------------------------------------------------#
  #- Data management for plotting
  #----------------------------------------------------------------------------#
  #- Standardized residuals
  resid_std <- residuals.linreg(obj)/sqrt(obj$residual_variance)
  resid_std_abs_sqrt <- sqrt(abs(resid_std))
  #- Square root of the absolute value of standardized residuals
  df$resid_std_abs_sqrt <- resid_std_abs_sqrt
  df$ij <- row.names(df)
  names(df) <- c("pred","resid","resid_std_abs_sqrt","ij")

  #- Find top third value
  third_topval <- resid_std_abs_sqrt[order(resid_std_abs_sqrt, decreasing = TRUE)][3]

  #----------------------------------------------------------------------------#
  #- Make plots
  #----------------------------------------------------------------------------#

  ggplot2::theme_set(ggplot2::theme_bw())

  #- Common plot parameters
  cpt <- paste0("linreg(",format(obj$formula),")")
  x <- "pred"
  xlab <- "Fitted values"
  cutoff_y_var <- "resid_std_abs_sqrt"
  cutoff_y_val <- third_topval
  id <- "ij"

  #- Plot 1
  title1 <- "Residuals Vs Fitted"
  y1 <- "resid"
  ylab1 <- "Residuals"

  ####
  title <- title1
  y <- y1
  ylab <- ylab1
  pl1 <- make_plot(df=df, title=title1, cpt=cpt, x=x, y=y1, xlab=xlab,  ylab=ylab1,
                   cutoff_y_var=cutoff_y_var, cutoff_y_val=cutoff_y_val, id=id)

  plot(pl1)

  #- Plot 2
  title2 <- "Scale-Location"
  y2 <- "resid_std_abs_sqrt"
  ylab2 <- expression(sqrt("|standardised residuals|"))

  pl2 <- make_plot(df=df, title=title2, cpt=cpt, x=x, y=y2, xlab=xlab,  ylab=ylab2,
                   cutoff_y_var=cutoff_y_var, cutoff_y_val=cutoff_y_val, id=id)

  plot(pl2)

}

make_plot <- function(df, title, cpt, x, y, xlab, ylab, cutoff_y_var,
                      cutoff_y_val, id) {

  resid_plot <- df[,y]
  m <- mean(resid_plot)
  pred_plot <- df[,x]
  id_plot <- df[,id]
  cutoff_y_var <- df[,cutoff_y_var]
  ylim_min <- min(0, round(min(resid_plot),2))
  ylim_max <- round(max(resid_plot),2)

  #- Calculate summary statistics
  y_means <- stats::aggregate(resid_plot, list(pred_plot), FUN=mean)
  names(y_means) <- c("pred_plot","resid_plot")
  y_medians <- stats::aggregate(resid_plot, list(pred_plot), FUN=stats::median)
  names(y_medians) <- c("pred_plot","resid_plot")

  #d <- ifelse((cutoff_y_var>=cutoff_y_val),as.character(id_plot),'')
  #print(d)
  #print(length(d))

  pl <-
    ggplot2::ggplot(data = df) +
    ggplot2::aes(x=pred_plot, y=resid_plot) +
    ggplot2::geom_point(shape = 1) +

    ggplot2::ylim(ylim_min, ylim_max) +

    #ggplot2::geom_text(ggplot2::aes(label=ifelse((cutoff_y_var>=cutoff_y_val),
    #                                             as.character(id_plot),'')),
    #                   hjust="inward, vjust="-0.2"inward, check_overlap = FALSE,
    #                   position = position_dodge(width = 1), size=3) +

    ggplot2::geom_text(ggplot2::aes(label=ifelse((cutoff_y_var>=cutoff_y_val),
                                                 as.character(id_plot),'')),
                       hjust=-0.2, vjust=-0.2,
                       check_overlap = TRUE, size=3) +

    #- Add mean and medians line plots
    ggplot2::geom_line(data=y_means, ggplot2::aes(color="mean")) +
    ggplot2::geom_line(data=y_medians, ggplot2::aes(color="median")) +
    ggplot2::scale_color_manual(name = "", values = c("mean" = "red",
                                                      "median" = "darkblue")) +

    #- Title, axes and caption
    ggplot2::labs(title=title, y=ylab, x=xlab, caption=paste0(cpt)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.caption = ggplot2::element_text(hjust = 0.5))

  return(pl)
}