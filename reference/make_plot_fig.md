# Interactive plots of seasonally adjusted time series.

A function that creates a list of interactive plots of time series
seasonally adjusted with RJDemetra. Interactive plots are made with
Plotly's R graphing library. Interactive plots show unadjusted series,
seasonally adjusted series and trend.

## Usage

``` r
make_plot_fig(
  models_in,
  series_now,
  plot_start = NULL,
  linearized = F,
  cal_adjust = F,
  ma_filter = F
)
```

## Arguments

- models_in:

  A list of output objects from the x13()-function.

- series_now:

  Names of series to be plotted.

- plot_start:

  Start date of time axis in plot.

- linearized:

  Linearized times series to be shown in plots? Default is FALSE.

- cal_adjust:

  Calender adjusted times series to be shown in plots? Default is FALSE.

- ma_filter:

  1x3 MA-filter to be shown in plots? Default is FALSE.

## Value

List of plotly objects.
