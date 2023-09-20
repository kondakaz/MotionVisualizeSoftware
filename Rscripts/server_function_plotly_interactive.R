# ggplot2 をもっとカンタンに plotly 化する
# https://blog.atusy.net/2019/03/22/ggplotly-asif-layer/

pacman::p_load(purrr, plotly)

gginteractive <- function(interactive = TRUE, ...) {
  structure(
    if(interactive) {
      purrr::partial(plotly::ggplotly, ...)
    } else {
      identity
    },
    class = c("gginteractive", "function")
  )
}

ggplot_add.gginteractive <- function (object, plot, object_name) object(plot)