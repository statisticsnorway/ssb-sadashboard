#' Interactive plots of seasonally adjusted time series.
#'
#' A function that creates a list of interactive plots of time series seasonally adjusted with RJDemetra.
#' Interactive plots are made with Plotly's R graphing library.
#' Interactive plots show unadjusted series, seasonally adjusted series and trend.
#'
#' @param models_in A list of output objects from the x13()-function.
#' @param series_now Names of series to be plotted.
#' @param plot_start Start date of time axis in plot.
#' @param linearized Linearized times series to be shown in plots? Default is FALSE.
#' @param cal_adjust Calender adjusted times series to be shown in plots? Default is FALSE.
#' @param ma_filter 1x3 asymmetric MA-filter to be shown in plots? Default is FALSE.
#' @return List of plotly objects.
#' @export
#' @examples
#'
#' # Kommer noen eksempler her




make_plot_fig  <-function(models_in,series_now,plot_start = NULL,linearized=F,cal_adjust=F,ma_filter=F){

  model_names <- names(models_in)

  plots_now <- list()

  for(i in 1:length(series_now)){
    model_now <- models_in[[which(series_now[i]==model_names)]]

    ts_df <- make_ts_df(model_now,linearized,cal_adjust,ma_filter)

    if(!is.null(plot_start)){
      ts_df <- dplyr::filter(ts_df,date>=plot_start)
    }

    plot_now <- plotly::plot_ly(ts_df,type= 'scatter',mode='lines')|>
      plotly::layout(xaxis=list(title=""),yaxis=list(title=""))|>
      plotly::add_trace(x = ~date, y = ~Ujust,name="Ujust",line=list(color="lightslategrey", width=1))
    if(isTRUE(cal_adjust)){
      plot_now <- plot_now|>
        plotly::add_trace(x = ~date, y = ~cal_adjust,name="Cal_adjust",line=list(color="forestgreen",  width=1))
    }
    if(isTRUE(linearized)){
      plot_now <- plot_now|>
        plotly::add_trace(x = ~date, y = ~B1,name="Linearized",line=list(dash = "dot",color="grey",  width=1))
    }
    plot_now <- plot_now|>
      plotly::add_trace(x = ~date, y = ~SA,name="SA",line=list(color="darkblue"))|>
      plotly::add_trace(x = ~date, y = ~Trend,name="Trend",line=list(color="tomato"))
    if(isTRUE(ma_filter)){
      plot_now <- plot_now|>
        plotly::add_trace(x = ~date, y = ~MA_filter,name="MA_filter",line=list(color="orange",dash="dot"))
    }

    plots_now[[i]] <- plot_now
  }
  return(plots_now)

}


make_taglist <- function(group_plots_now){

  tag_plot_now <- list()

  for(i in 1:length(group_plots_now)){
    tag_plot_now[[i]] <- htmltools::tagList(group_plots_now[[i]])
  }

  return(tag_plot_now)
}
