theme_legend <- function(title_size = 10, text_size = 10, key_size = 1,
                         legend_pos = c(1,1), legend_dir = "vertical",
                         legend_just = c(1,1),
                         legend_background = T, key_background = F, ...) {
  ret = theme(
    # Legend title and text labels
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Title font color size and face
    legend.title = element_text(size = title_size),
    # Title alignment. Number from 0 (left) to 1 (right)
    legend.title.align = NULL,
    # Text label font color size and face
    legend.text = element_text(size = text_size),
    # Text label alignment. Number from 0 (left) to 1 (right)
    legend.text.align = NULL)
  ret = ret + theme(
    ## Legend position, margin and background
    ## :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## Legend position: right, left, bottom, top, none
    legend.position = legend_pos,
    ## Margin around each legend
    legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  if (legend_background == TRUE) {
  ret = ret + theme(
    ## Legend background
    legend.background = element_rect(size = 0.1, color = "gray70")
    )}
  ret = ret + theme(
    # Legend direction and justification
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Layout of items in legends ("horizontal" or "vertical")
    legend.direction = legend_dir,
    # Positioning legend inside or outside plot
    # ("center" or two-element numeric vector)
    legend.justification = legend_just)
  ret = ret + theme(
    # Background underneath legend keys
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    legend.key.size = unit(key_size, "lines"),    # key size (unit)
    legend.key.height = NULL,                # key height (unit)
    legend.key.width = NULL                 # key width (unit)
    )
  if (key_background == TRUE) {
    ret = ret + theme(legend.key = element_rect(color = "gray70")  # Key background
                      )}
    ## # Spacing between legends.
    ## #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## legend.spacing = unit(0.4, "cm"),
    ## legend.spacing.x = NULL,                 # Horizontal spacing
    ## legend.spacing.y = NULL,                 # Vertical spacing
    ## #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## # Legend box
    ## #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ## # Arrangement of multiple legends ("horizontal" or "vertical")
    ## legend.box = NULL,
    ## # Margins around the full legend area
    ## legend.box.margin = margin(0, 0, 0, 0, "cm"),
    ## # Background of legend area: element_rect()
    ## legend.box.background = element_blank(),
    ## # The spacing between the plotting area and the legend box
    ## legend.box.spacing = unit(0.4, "cm")
  ret = ret + theme(...)
  ret
}
