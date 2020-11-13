#' @title Define Segments in y-Axis for `ggplot2`
#' @description Easy to define segments in y-axis for `ggplot2`.
#' @param plot A `ggplot2` plot.
#' @param ylim The y-axis limits.
#' @param segments The interval of a segment. If more than one intervals
#'   are given, please use `list()` to concatenate them.
#' @param tick_width One or more numbers for each segmented y-axis.
#' @param rel_heights Numerical vector of relative segmented y-axis and
#'   segments heights, default is 1 and 0.
#' @param vjust Vertical justification. Default = 0 (baseline at y).
#' @param margin Margins around the text.
#' @param ... Arguments will be handed to `plot_grid()` in `cowplot`.
#' @importFrom ggplot2 coord_cartesian theme scale_y_continuous ylab
#'   element_blank element_line unit
#' @importFrom cowplot plot_grid draw_label
#' @return A segmented picture.
#' @export
#'
#' @examples
#' data(mtcars)
#' library(ggplot2)
#' p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
#'     geom_bar() +
#'     ggtitle("Number of Cars by Gear") +
#'     xlab("Gears")
#'
#' #single segments and missing tick_width
#' gggap(plot=p,
#'        segments=c(5,10),
#'        ylim=c(0,50))
#' #tick_width can be one or more numbers
#' gggap(plot=p,
#'        segments=c(5,10),
#'        tick_width = c(1,10),
#'        ylim=c(0,50))
#' #segments list cantains more than one number vectors
#' gggap(plot=p,
#'        segments=list(c(2.5,4),c(5,10)),
#'        tick_width = c(1,0.5,10),
#'        ylim=c(0,50))
#' #rel_heights can set the relative height for segments and segmented y-axis
#' gggap(plot=p,
#'        segments=list(c(2.5,4),c(5,10)),
#'        tick_width = c(1,0.5,10),
#'        rel_heights=c(0.2,0,0.2,0,1),
#'        ylim=c(0,50))
#' #reversed y-axis
#' p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
#'     geom_bar() +
#'     ggtitle("Number of Cars by Gear") +
#'     xlab("Gears")+
#'     scale_y_continuous(trans = 'reverse')
#' #single segments and missing tick_width
#' gggap(plot=p,
#'        segments=c(10,5),
#'        ylim=c(15,0))
gggap <- function(plot, ylim, segments, tick_width, rel_heights,
                  vjust = 0,
                  margin = c(top = 1, right = 2, bottom = 1, left = 1),
                  ...) {
  # `segments` must be a list
  if (!is.list(segments)) {
    segments <- list(segments)
  }

  ylim <- get_validated_ylim(ylim, plot)
  ascending_ylim <- ylim[1] < ylim[2]
  trans <- get_desired_transform(plot)

  if (segments_ordered_like_ylim(segments, ascending_ylim) &&
     segment_pairs_ordered(segments, ascending_ylim) &&
      segments_within_ylim(segments, ylim, ascending_ylim) &&
      desired_transform_valid(trans, ascending_ylim, ylim)) {
    thick_width <- compute_thick_width(thick_width, ylim, segments)
    seg_heights <- compute_seg_heights(segments)
    y_heights <- compute_y_heights(segments)
  }

  # plotting must be done in three stages
  # one for each segment from bottom to top
  p_segment <- plot_bottom(plot, ascending_ylim, ylim[1],
                           unlist(segments[1])[1],
                           tick_width[1], trans)
  rel_height <- c(y_heights[1], seg_heights[1])

  for (i in 2:length(segments) - 1) {
    p_segment <- plot_midd(plot, i, ascending_ylim, ylim[1],
                          unlist(segments[i])[1], tick_width[i],
                          segments, trans, p_segment)
    rel_height <- c(rel_height, y_heights[i], seg_heights[i])
  }

  p_segment <- plot_top(plot, length(segments), ascending_ylimit, ylim[2],
                        unlist(segments[length(segments)])[2],
                        tick_width[length(segments) + 1], segments,
                        trans, p_segment)
  rel_heigh <- c(rel_heigh, y_heights[length(segments)])

  # this does not seem to be used `num_parts <- length(p_segment)`
  # main fix start
  sbt <- p_segment[[1]]$labels$subtitle #fix
  p_segment <- purrr::map(p_segment,
  ~ if (is.ggplot(.)) {
      . + labs(subtitle = NULL)
    } else {
      NULL
    }) # fix

  # reverse order
  p_segment <- rev(p_segment)

  p_segment[[1]]$labels$subtitle <- sbt # fix
  # main fix ends
  if (missing(rel_heights)) {
    rel_heights <- rev(rel_heigh)
  } else {
    rel_heights <- rev(rel_heights)
  }
  if (is.null(plot$theme$axis.title.y$angle)) {
    angle <- 90
  } else {
    angle <- plot$theme$axis.title.y$angle
  }
  plot_grid(plotlist = p_segment, ncol = 1, align = "v",
            rel_heights = rel_heights) +
  theme(plot.margin = unit(margin, "cm")) +
  draw_label(label = plot$labels$y,
             x = 0,
             hjust = plot$theme$axis.title.y$hjust,
             vjust = vjust,
             fontfamily = plot$theme$axis.title.y$family,
             fontface = plot$theme$axis.title.y$face,
             size = plot$theme$axis.title.y$size,
             angle = angle,
             lineheight = plot$theme$axis.title.y$lineheight,
             colour = plot$theme$axis.title.y$colour)
}

plot_bottom <- function(plot, ascending_ylimit, ylim, gap, tick_width, trans) {
  if (ascending_ylimit) {
    breaks <- seq(ylim, gap, by = tick_width)
  } else if (!ascending_ylimit) {
    breaks <- seq(gap, ylim, by = tick_width)
  }
  p_segment_i <- plot +
  coord_cartesian(ylim = c(ylim, gap)) +
  theme(panel.border = element_blank()) +
  theme(axis.line.y = element_line(),
        axis.line.x.bottom = element_line(),
        plot.title = element_blank(),
        legend.position = "none") +
  scale_y_continuous(expand = c(0, 0),
                      breaks = breaks,
                      trans = trans) +
  ylab(label = NULL)
  p_segment <- list(p_segment_i)
  names(p_segment)[length(p_segment)] <- 1
  return(p_segment)
}

plot_midd <- function(plot, i, ascending_ylimit, ylim, gap, tick_width,
  segments, trans, p_segment) {
  if (ascending_ylimit) {
    breaks <- seq(ylim, gap, by = tick_width)
  } else if (!ascending_ylimit) {
    breaks <- seq(gap, ylim, by = tick_width)
  }
  p_segment_i <- plot +
  coord_cartesian(ylim = c(unlist(segments[i - 1])[2], gap)) +
  theme(panel.border = element_blank()) +
  theme(axis.line.y = element_line(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0, 0),
                      breaks = breaks,
                      trans = trans) +
  ylab(label = NULL)
  # add y label in the middle median part
  p_segment <- c(p_segment, list(NULL), list(p_segment_i))
  names(p_segment)[length(p_segment)] <- i
  return(p_segment)
}

plot_top <- function(plot, i, ascending_ylimit, ylim, gap, tick_width,
  segments, trans, p_segment) {
  if (ascending_ylimit) {
    breaks <- seq(gap, ylim, by = tick_width)
  } else if (!ascending_ylimit) {
    breaks <- seq(ylim, gap, by = tick_width)
  }
  p_segment_i <- plot +
  coord_cartesian(ylim = c(gap, ylim)) +
  theme(panel.border = element_blank()) +
  theme(axis.line.y = element_line(),
        axis.line.x.top = element_line(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0, 0),
                      breaks = breaks,
                      trans = trans) +
  ylab(label = NULL)
  p_segment <- c(p_segment, list(NULL), list(p_segment_i))
  names(p_segment)[length(p_segment)] <- i + 1
  return(p_segment)
}

get_validated_ylim <- function(ylim, plot) {
  # `ylim` must be defined and must have different values
  if (all(missing(ylim), is.null(plot$coordinates$limits$y))) {
    stop("ylim is undefined")
  } else if (ylim[1] == ylim[2]) {
    stop("ylim values must be different")
  } else if (missing(ylim)) {
    return(plot$coordinates$limits$y)
  }
}

segments_ordered_like_ylim <- function(segments, ascending_ylimit) {
  # `segments` must be ordered, either ascending or descending order is valid
  # and must match `ylim` ordering
  for (j in seq_len(length(segments))) {
    seg1 <- segments[[j]][1]
    seg2 <- segments[[j]][2]
    if (seg1 > seg2) {
      if (ascending_ylimit) {
        stop(paste0("No.", j, " segment: c(", seg1, ",", seg2,
                    ") is wrong. It should be ", "c(", seg2, ",", seg1, ")"))
      }
    } else if (seg1 < seg2) {
      if (!ascending_ylimit) {
        stop(paste0("No.", j, " segment: c(", seg1, ",", seg2,
                    ") is wrong. It should be ", "c(", seg2, ",", seg1, ")"))
      }
    } else {
      stop(paste0("No.", j, " segment: c(", seg1, ",", seg2,
                  ") is wrong. They must not be the same"))
    }
  }
  return(TRUE)
}

segment_pairs_ordered <- function(segments, ascending_ylimit) {
  # the paired sequence of `segments` must follow to the ordering of `ylims`
  if (length(segments) >= 2) {
    if (ascending_ylimit) {
      for (k in 2:length(segments)) {
        # the second element of the previous segment cannot be larger than
        # the first element of the current segment
        if (segments[[k - 1]][2] > segments[[k]][1]) {
          pre <- paste0("c(", segments[[k - 1]][1], ",", segments[[k - 1]][2],
                        ")")
          suf <- paste0("c(", segments[[k]][1], ",", segments[[k]][2], ")")
          stop(paste0("Segments ", k - 1, " and ", k, ": ", pre, ",", suf,
                      " are wrong. They should be ", suf, ",", pre))
        }
      }
      return(TRUE)
    } else if (!ascending_ylimit) {
      for (k in 2:length(segments)) {
        # the second element of the previous segment cannot be smaller than
        # the first element of the current segment
        if (segments[[k - 1]][2] < segments[[k]][1]) {
          pre <- paste0("c(", segments[[k - 1]][1], ",", segments[[k - 1]][2],
                        ")")
          suf <- paste0("c(", segments[[k]][1], ",", segments[[k]][2], ")")
          stop(paste0("Segments ", k - 1, " and ", k, ": ", pre, ",", suf,
                      " are wrong. They should be ", suf, ",", pre))
        }
      }
      return(TRUE)
    }
  }
}

segments_within_ylim <- function(segments, ylim, ascending_ylim) {
  # the range of values in `segments` must be within the values in `ylim`
  if (ascending_ylimit) {
    # `ylim` is in ascending order
    if (min(unlist(segments)) <= ylim[1])
      stop("the minimum of segments must be larger than the minium of ylim")
    if (max(unlist(segments)) > ylim[2])
      stop("the maximum of segments must be smaller than maximum of ylim")
  } else if (!ascending_ylimit) {
    # `ylim` is in descending order
    if (min(unlist(segments)) <= ylim[2])
      stop("the minimum of segments must be larger than the minium of ylim")
    if (max(unlist(segments)) > ylim[1])
      stop("the maximum of segments must be smaller than maximum of ylim")
  }
  return(TRUE)
}

compute_thick_width <- function(thick_width, ylim, segments) {
  # `tick_width` must be computed if not specified
  if (missing(tick_width)) {
    tick_width <- rep(abs(ylim[2] - ylim[1]) / 10, length(segments) + 1)
  } else {
    # `tick_width` must have 1 more element than `segments`
    if ((length(tick_width) - length(segments)) < 1) {
      int_len <- length(tick_width)
      for (m in (int_len + 1):(length(segments) + 1)) {
        tick_width[m] <- tick_width[int_len]
      }
    }
  }
  return(thick_width)
}

compute_seg_heights <- function(segments) {
  # `seg_heights` cannot have less elements than `segments`
  seg_heights <- 0
  if (length(seg_heights) < length(segments)) {
    seg_heights_len <- length(seg_heights)
    for (m in (seg_heights_len + 1):length(segments)) {
      seg_heights[m] <- seg_heights[seg_heights_len]
    }
  }
  return(seg_heights)
}

compute_y_heights <- function(segments) {
  # `y_heights` cannot have less elements than `segments`+1
  y_heights <- 1
  if (length(y_heights) < (length(segments) + 1)) {
    y_heights_len <- length(y_heights)
    for (m in (y_heights_len + 1):(length(segments) + 1)) {
      y_heights[m] <- y_heights[y_heights_len]
    }
  }
  return(y_heights)
}

get_desired_transform <- function(plot) {
  # identity is the default transformation
  if (length(plot$scales$scales) == 0) {
    return("identity")
  } else if ("trans" %in% names(plot$scales$scales[[1]])) {
    return(plot$scales$scales[[1]]$trans)
  } else {
    return("identity")
  }
}

desired_transform_valid <- function(trans, ascending_ylimit, ylim) {
  # `ylim` cannot be in ascending order if reverse transform is desired
  if ("reverse" %in% trans) {
    if (ascending_ylimit) {
      stop(paste0("ylim: ", "c(", ylim[1], ",", ylim[2], ")",
                  " is wrong. It should be ", "c(", ylim[2], ",", ylim[1], ")"))
    }
  }
  # `ylim` cannot be in descending order if identity transform is desired
  if ("identity" %in% trans) {
    if (!ascending_ylimit) {
      stop(paste0("ylim: ", "c(", ylim[1], ",", ylim[2], ")",
                  " is wrong. It should be ", "c(", ylim[2], ",", ylim[1], ")"))
    }
  }
  return(TRUE)
}