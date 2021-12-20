jyun_boxplot = function(y, x, ylab = NULL, xlab = NULL) {
  ## side by side box plot
  dd <- data.frame(y = y, x = x)
  boxp <- ggplot(dd, aes(x = factor(x), y = y, fill = factor(x))) +
    geom_boxplot(outlier.size = 0.25) +
    boxp = boxp + labs(y = ylab, x = xlab) +
    theme_base() + theme(legend.position = "none")
  ## scale_x_continuous(breaks = c(1,max(dd$item)/2,max(dd$item))) + scale_y_continuous(limits=c(0,1), breaks = c(0,0.5,1)) +
  boxp
}

clust2long = function(df, ...) {
  if (is.null(df$prob)) {
    df_long <- tidyr::gather(tmp, key, value, -cluster)
  } else {
  df_long <- tidyr::gather(tmp, key, value, -prob, -cluster)
  }
  if (nargs() > 1)
    df_long = df_long %>% mutate(key = recode_factor(key, ...)) # determine facet order
  return(df_long)
}

box_clust <- function(df_long, nr = 1, strip.position = "top", ...) {
  gg <- ggplot(df_long, aes(
    x = cluster, y = value,
    color = factor(cluster), fill = factor(cluster),
    shape = factor(cluster)
  )) +
    geom_boxplot(
      width = 0.5, outlier.size = 0,
      outlier.alpha = 0,
      alpha = 0.5, color = "gray30"
    ) +
    geom_jitter(width = 0.35, size = 1, alpha = 0.7) +
    facet_wrap(vars(key), label = "label_parsed", nrow = nr, strip.position = strip.position) +
    theme_base(
      plot_margin = margin(0, 0, 0, 0),
      panel_spacing = grid::unit(.5, "lines"),
      strip_text_hjust = 0.5, strip_text_size = 15, ...) +
    ## theme_bw()+
    ## theme_minimal() +
    ## scale_color_futurama()+
    ## scale_fill_futurama()+
    theme(legend.position = "none")
  gg
}


cl_plot <- function(x, cl, myname = NULL, size_ = 1, xlim = NA, ylim = NA) {
  ## plot 2d array x
  ## mark points by groups specified by cl

  cl <- as.factor(cl)
  position <- as.data.frame(x)
  ndim <- dim(x)[2]

  colnames(position) <- paste("position", 1:ndim, sep = "")

  padding <- 1.05
  if (any(is.na(xlim))) {
    x1 <- -max(abs(position[, 1])) * padding
    x2 <- max(abs(position[, 1])) * padding
  } else {
    x1 <- xlim[1]
    x2 <- xlim[2]
  }
  if (any(is.na(ylim))) {
    y1 <- -max(abs(position[, 2])) * padding
    y2 <- max(abs(position[, 2])) * padding
  } else {
    y1 <- ylim[1]
    y2 <- ylim[2]
  }

  mytheme <- theme(
    axis.line = element_line(colour = "black"),
    ## panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ## panel.border = element_blank(),
    panel.background = element_blank()
  )

  ## plot
  pp <- ggplot(position, aes(x = position1, y = position2, colour = cl)) +
    ## theme(text=element_text(size=20)) +
    ## geom_point()+
    xlim(x1, x2) +
    ylim(y1, y2) +
    xlab(myname[1]) +
    ylab(myname[2]) +
    ## xlab("Position 1") + ylab("Position 2") +
    geom_hline(yintercept = 0, color = "gray70", linetype = 2) +
    geom_vline(xintercept = 0, color = "gray70", linetype = 2) +
    mytheme
  pp <- pp + geom_point(size = size_)
  ## pp +
  ##   geom_text(
  ##     data = subset(position, idx == "w"), aes(x = position1, y = position2, label = (1:n_w), colour = cl_w),
  ##     ## segment.color = "grey50",
  ##     check_overlap = FALSE, show.legend = FALSE, size = 4
  ##   )
}
