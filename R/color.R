plt_palette = c('#3182bd', '#6baed6', '#9ecae1', '#c6dbef', '#e6550d', '#fd8d3c', '#fdae6b', '#fdd0a2', '#31a354', '#74c476', '#a1d99b', '#c7e9c0', '#756bb1', '#9e9ac8', '#bcbddc', '#dadaeb', '#636363', '#969696', '#bdbdbd', '#d9d9d9')

plt_pal <- function() {
  scales::manual_pal(plt_palette)
}

scale_color_plt <- function(...) { ggplot2::discrete_scale("color", "plt", plt_pal(), ...) }
scale_colour_plt <- scale_color_plt

scale_fill_plt <- function(...) { ggplot2::discrete_scale("fill", "plt", plt_pal(), ...) }
