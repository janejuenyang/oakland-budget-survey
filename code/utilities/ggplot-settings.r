################################################################################
# purpose: define defaults for visualizations
# last edited: jan 19, 2025
################################################################################

# set ggplot theme
theme_set(theme_minimal()) 

# customize to further remove unnecessary visual elements
theme_update(
    # don't include axis titles (will be in graph title instead)
    axis.title = element_blank(),
    # grey axes labels
    axis.text = element_text(color = "grey40"),
    # set axis font size, margin, and color
    axis.text.x = element_text(size = 12, margin = margin(t = 5)),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),
    axis.ticks = element_line(color = "grey91", linewidth = .5),
    axis.ticks.length.x = unit(1.3, "lines"),
    axis.ticks.length.y = unit(.7, "lines"),
    # remove unneeded visual elements
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.border = element_blank(),
    legend.position = "none",
    # customize plot margin (top, right, bottom, left)
    plot.margin = margin(5, 5, 5, 5),
    # customize plot labels
    plot.title = element_text(
        color = "grey10", 
        size = 20, 
        face = "bold",
        margin = margin(t = 15)
    ),
    plot.subtitle = element_markdown(
        color = "grey30", 
        size = 16,
        lineheight = 1.35,
        margin = margin(t = 15, b = 15)
    ),
    plot.caption = element_text(
        color = "grey30", 
        size = 12,
        lineheight = 1.2, 
        hjust = 0,
        margin = margin(t = 15) # Large margin on the top of the caption.
    ),
    # align title and caption
    plot.title.position = "plot",
    plot.caption.position = "plot",
)