jyun_boxplot = function(y, x, ylab = NULL, xlab = NULL) {
  ## side by side box plot
  dd <- data.frame(y = y, x = x)
  boxp <- ggplot(dd, aes(x = factor(x), y = y, fill = factor(x))) +
    geom_boxplot(outlier.size = 0.25) +
    boxp = boxp + labs(y = ylab, x = xlab) +
    theme_base() + theme(legend.position = "none")
  boxp
}

clust2long = function(df, ...) {
  if (is.null(df$prob)) {
    df_long <- tidyr::gather(df, key, value, -cluster)
  } else {
  df_long <- tidyr::gather(df, key, value, -prob, -cluster)
  }
  if (nargs() > 1)
    ## determine facet order & strip text
    df_long = df_long %>%
      dplyr::mutate(key = dplyr::recode_factor(key, ...))
    return(df_long)
}

box_clust <- function(df_long, nr = 1, strip.position = "top", ...) {
  gg <- ggplot(df_long, aes(
    x = cluster, y = value,
    color = factor(cluster), fill = factor(cluster),
    shape = factor(cluster)
  )) +
    geom_boxplot(
      width = 0.85, outlier.size = 0,
      outlier.alpha = 0,
      alpha = 0.5, color = "gray30"
    ) +
    geom_jitter(width = 0.3, size = 0.75, alpha = 0.6) +
    facet_wrap(vars(key), label = "label_parsed", nrow = nr,
               strip.position = strip.position) +
    theme_base(
      panel_spacing = grid::unit(0, "lines"),
      strip_text_hjust = 0.5, strip_text_size = 15, ...) +
    ## scale_color_futurama()+
    ## scale_fill_futurama()+
    theme(legend.position = "none")
  gg
}


plot2d <- function(x, cluster, myname = NULL, geom_size = 1,
                   xlim = NA, ylim = NA, symm = T, padding = 0.05, ...) {
  ## plot 2d array x
  ## mark points by groups specified by cluster
  cluster <- as.factor(cluster)
  position <- as.data.frame(x)
  ndim <- dim(x)[2]
  colnames(position) <- paste("position", 1:ndim, sep = "")

  if ((any(is.na(xlim))) && symm) {
    x1 <- -max(abs(position[, 1]))
    x2 <- -1 * x1
  } else {
    x1 <- xlim[1]
    x2 <- xlim[2]
  }
  if ((any(is.na(ylim))) && symm) {
    y1 <- -max(abs(position[, 2]))
    y2 <- -1 * y1
  } else {
    y1 <- ylim[1]
    y2 <- ylim[2]
  }

  pp <- ggplot(position, aes(x = position1, y = position2,
                             colour = cluster, fill = cluster)) +
    xlab(myname[1]) +
    ylab(myname[2]) +
    ## geom_hline(yintercept = 0, color = "gray70", linetype = 2) +
    ## geom_vline(xintercept = 0, color = "gray70", linetype = 2) +
    theme_base(
      plot_margin = margin(t = 0, r = 0, b = 0, l = 0), ...
    ) +
    geom_point(size = geom_size) +
    scale_color_tab20c(1) +
    scale_fill_tab20c(4)

  if (any(is.null(c(x1, x2, y1, y2)))) {
    pp <- pp +
      scale_x_continuous(expand = c(padding, padding)) +
      scale_y_continuous(expand = c(padding, padding))
  } else {
    pp <- pp +
      scale_x_continuous(limits = c(x1, x2), expand = c(padding, padding)) +
      scale_y_continuous(limits = c(y1, y2), expand = c(padding, padding))
  }
  ## pp +
  ##   geom_text(
  ##     data = subset(position, idx == "w"), aes(x = position1, y = position2, label = (1:n_w), colour = cluster_w),
  ##     ## segment.color = "grey50",
  ##     check_overlap = FALSE, show.legend = FALSE, size = 4
  ##   )
  pp
}
