library(ggplot2)
library(gridExtra)
library(grid)

theme_mine <- function (base_size = 12, color = "brown", base_family = "helvetica") 
{
    colorhex <- ggthemes_data$wsj$bg[color]
    (theme_foundation(base_size = base_size, base_family = base_family) + 
        theme(line = element_line(linetype = 1, colour = "black"), 
            rect = element_rect(fill = colorhex, linetype = 0, 
                colour = NA), text = element_text(colour = "black"), 
            title = element_text(family = base_family, size = rel(1.5)), 
            axis.title = element_text(face = "bold",size = rel(.75)),
            axis.text = element_text(face = "bold", 
                size = rel(1)), axis.text.x = element_text(colour = NULL),               
#           axis.title.y = element_text(angle=90,vjust =2),
#           axis.title.x = element_text(vjust = -0.2),
            axis.text.y = element_text(colour = NULL), axis.ticks = element_line(colour = NULL), 
            axis.ticks.y = element_blank(), axis.ticks.x = element_line(colour = NULL), 
            axis.line = element_line(), axis.line.y = element_blank(), 
            legend.background = element_rect(), legend.position = "right", 
#           legend.direction = "horizontal", legend.box = "vertical", 
            legend.title = element_blank(),
            legend.direction = "vertical", legend.box = "vertical",
            panel.grid = element_line(colour = NULL, linetype = 0), 
            panel.grid.major = element_line(colour = "black"), 
            panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
            plot.title = element_text(hjust = 0, face = "bold"), 
            plot.margin = unit(c(10,5,5,5),"mm"), strip.background = element_rect()))
}


theme_mine_legend_bottom <- function (base_size = 12, color = "brown", base_family = "helvetica") 
{
    colorhex <- ggthemes_data$wsj$bg[color]
    (theme_foundation(base_size = base_size, base_family = base_family) + 
        theme(line = element_line(linetype = 1, colour = "black"), 
            rect = element_rect(fill = colorhex, linetype = 0, 
                colour = NA), text = element_text(colour = "black"), 
            title = element_text(family = base_family, size = rel(1.5)), 
            axis.title = element_text(face = "bold",size = rel(.75)),
            axis.text = element_text(face = "bold", 
                size = rel(1)), axis.text.x = element_text(colour = NULL),               
#           axis.title.y = element_text(angle=90,vjust =2),
#           axis.title.x = element_text(vjust = -0.2),
            axis.text.y = element_text(colour = NULL), axis.ticks = element_line(colour = NULL), 
            axis.ticks.y = element_blank(), axis.ticks.x = element_line(colour = NULL), 
            axis.line = element_line(), axis.line.y = element_blank(), 
            legend.background = element_rect(), legend.position = "right", 
            legend.direction = "horizontal", legend.box = "vertical", 
            legend.title = element_blank(),
#           legend.direction = "vertical", legend.box = "vertical",
            panel.grid = element_line(colour = NULL, linetype = 0), 
            panel.grid.major = element_line(colour = "black"), 
            panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
            plot.title = element_text(hjust = 0, face = "bold"), 
            plot.margin = unit(c(10,5,5,5),"mm"), strip.background = element_rect()))
}


theme_mine_no_legend <- function (base_size = 12, color = "brown", base_family = "helvetica") 
{
    colorhex <- ggthemes_data$wsj$bg[color]
    (theme_foundation(base_size = base_size, base_family = base_family) + 
        theme(line = element_line(linetype = 1, colour = "black"), 
            rect = element_rect(fill = colorhex, linetype = 0, 
                colour = NA), text = element_text(colour = "black"), 
            title = element_text(family = base_family, size = rel(1.5)), 
            axis.title = element_blank(),
            axis.text = element_blank(), axis.text.x = element_text(colour = NULL),               
#           axis.title.y = element_text(angle=90,vjust =2),
#           axis.title.x = element_text(vjust = -0.2),
            axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.line = element_line(), axis.line.y = element_blank(), 
            legend.title = element_blank(),
            legend.position = "none",
#           legend.background = element_rect(), legend.position = "right", 
#           legend.direction = "horizontal", legend.box = "vertical", 
#           legend.title = element_blank(),
#           legend.direction = "vertical", legend.box = "vertical",
            panel.grid = element_line(colour = NULL, linetype = 0), 
            panel.grid.major = element_line(colour = "black"), 
            panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
#           plot.title = element_text(hjust = 0, vjust = 1, face = "bold"), 
            plot.margin = unit(c(20,5,20,5),"mm"), strip.background = element_rect()))
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "mm"), lheight/3)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)

}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
