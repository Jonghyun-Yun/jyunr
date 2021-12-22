tab20c_palette <- c(
  "#3182bd", "#6baed6", "#9ecae1", "#c6dbef",
  "#e6550d", "#fd8d3c", "#fdae6b", "#fdd0a2",
  "#31a354", "#74c476", "#a1d99b", "#c7e9c0",
  "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb",
  "#636363", "#969696", "#bdbdbd", "#d9d9d9"
)

tab20c_all_pal <- function() {
  scales::manual_pal(tab20c_palette)
}
scale_color_tab20c_all <- function(...) {
  ggplot2::discrete_scale("color", "tab20c_all", tab20c_all_pal(), ...)
}
scale_colour_tab20c_all <- scale_color_tab20c_all
scale_fill_tab20c_all <- function(...) {
  ggplot2::discrete_scale("fill", "tab20c_all", tab20c_all_pal(), ...)
}

tab20c_mat <- array(tab20c_palette, dim = c(4, 4))
tab20c_pick_pal <- function(light = 1) {
  tab20c_pal <- function() {
    scales::manual_pal(c(tab20c_mat[light, ]))
  }
  return(tab20c_pal)
}
scale_color_tab20c <- function(light = 0, ...) {
  ggplot2::discrete_scale("color", "tab20c", tab20c_pick_pal(light)(), ...)
}
scale_colour_tab20c <- scale_color_tab20c
scale_fill_tab20c <- function(light = 0, ...) {
  ggplot2::discrete_scale("fill", "tab20c", tab20c_pick_pal(light)(), ...)
}

tab20_palette <- c(
  "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78",
  "#2ca02c", "#98df8a", "#d62728", "#ff9896",
  "#9467bd", "#c5b0d5", "#8c564b", "#c49c94",
  "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7",
  "#bcbd22", "#dbdb8d", "#17becf", "#9edae5"
)

tab20_mat <- array(tab20_palette, dim = c(2, 10))
tab20_pick_pal <- function(light = 1) {
  tab20_pal <- function() {
    scales::manual_pal(c(tab20_mat[light, ]))
  }
  return(tab20_pal)
}
scale_color_tab20 <- function(light = 0, ...) {
  ggplot2::discrete_scale("color", "tab20", tab20_pick_pal(light)(), ...)
}
scale_colour_tab20 <- scale_color_tab20
scale_fill_tab20 <- function(light = 0, ...) {
  ggplot2::discrete_scale("fill", "tab20", tab20_pick_pal(light)(), ...)
}
