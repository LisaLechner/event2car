#' Plots cumulative abnormal returns over time
#'
#' plot method for the class "event2car_range".
#'
#' @param x an object of class \code{event2car_range}.
#' @param ... other arguments ignored (for compatibility with generic)
#' @param event_color a character object indicating the color of the vertical lines at the \code{event_date}. Default is "red".
#' @param event_linetype a character object indicating the linetype of the veritical line at the \code{event_date}. Default is "twodash".
#' @param event_size a numeric value indicating the size of the vertical line at the \code{event_date}. Default is 1.
#' @param event_label_size a numeric value indicating the size of the event label(s). Default is 3.
#' @param line_color a character object indicating the color of the loess curve (see \code{stats::loess}) of all
#' cumulative abnormal returns. Default is "black".
#' @param background_color a character value indicating the color of the area above zero. Default is "gray".
#' @param background_alpha a numeric value indicating the transparency of the area above zero. Default is 0.5.
#' @param point_size a numeric value specifying the size of the points representing
#' cumulative abnormal returns. Default is 0.2.
#' @param point_alpha a numeric value specifying the transparency of the points
#' representing cumulative abnormal returns. Default is 0.1.
#' @param axis_text_size a numeric value indicating the size of the axis text. Default is 8.
#' @param axis_title_size a numeric value indicating the size of the axis titles. Default is 10.
#' @param show_method a logical value indicating whether the information on the applied method
#' (\code{mean_adj}, \code{mrkt_adj_out}, or \code{mrkt_adj_within}) should be included in the plot.
#' @keywords internal
#' @method plot event2car_range
#' @export
#' @importFrom stats reshape qnorm sd
#' @importFrom ggplot2 ggplot geom_rect aes geom_point geom_smooth
#'  geom_vline geom_label scale_color_grey labs theme_minimal theme scale_x_date
#'  element_text element_blank
#' @importFrom scales date_format
#' @examples
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms <- tech_returns[,2:19]
#' return_indx <- tech_returns[,1]
#' effect_trump <- event2car_range(returns=returns_firms,regressor=return_indx,
#'                                 imputation_returns="mean",
#'                                 event_date=trumpelection,method="mean_adj")
#' plot(effect_trump)
plot.event2car_range <- function(x,...,
                                 event_color = "red",
                                 event_linetype = "twodash",
                                 event_size = 1,
                                 event_label_size = 3,
                                 line_color = "black",
                                 background_color = "gray",
                                 background_alpha = 0.5,
                                 point_size = 0.2,
                                 point_alpha = 0.1,
                                 axis_text_size = 8,
                                 axis_title_size = 10,
                                 show_method = TRUE){

  if (!class(x) == "event2car_range") {
    stop("Plot works for event2car_range xs only.")
  }

  if (class(x$car_timeseries) == "zoo") {
    use <- data.frame(x$car_timeseries)
    use$date <- row.names(use)
    use <- stats::reshape(use, idvar="date",
                          varying = 1:(ncol(use)-1),
                          v.names = "car",
                          direction="long")
    row.names(use) <- NULL
    names(use) <- c("date","firm","car")
    use$date <- as.Date(use$date)
  }
  if (class(x$car_timeseries) == "data.frame") {
    use <- x$car_timeseries
    row.names(use) <- NULL
  }
  use$firm <- as.character(use$firm)
  use$event <- ifelse(use$date %in% x$event_date,1,0)
  use$label <- ifelse(use$event == 1 & use$firm %in% unique(use$firm)[1], as.character(use$date),NA)

  if (show_method==TRUE) {
    if (x$method == "mean_adj") {
      method = "Mean-adjusted model"
    }
    if (x$method == "mrkt_adj_within") {
      method = "Market-adjusted model\n(within estimation)"
    }
    if (x$method == "mrkt_adj_out") {
      method = "Market-adjusted model\n(out-of-sample estimation)"
    }
  } else {method = ""}

  p <- ggplot2::ggplot(use,aes(date,car,label = label))+
    geom_rect(mapping=aes(xmin=min(use$date), xmax=max(use$date), ymin=0, ymax=Inf), color=background_color,
              alpha=background_alpha,fill=background_color)+
    geom_point(aes(color=firm),alpha=0.1)+
    geom_smooth(aes(color=firm),method="loess",alpha=point_alpha,size=point_size)+
    geom_smooth(color=line_color,method="loess")+
    geom_vline(xintercept = unique(use$date[use$event==1]),
               color=event_color,linetype=event_linetype,size=event_size)+
    geom_label(aes(y=max(use$car)),size=event_label_size,na.rm=TRUE) +
    scale_color_grey(start = 0.9, end = 0.1) +
    scale_x_date(labels = date_format("%Y-%m"))+
    labs(y="Cumulative Abnormal Return",x="",
         color="",
         title = method)+
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text = element_text(size=axis_text_size),
          axis.title = element_text(size=axis_title_size))
  return(p)
}
