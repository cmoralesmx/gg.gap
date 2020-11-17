# gggap

Streamlined creation of segments on the y-axis of 'ggplot2' plots  

This project builds on top of [gg.gap v1.4](https://github.com/ChrisLou-bioinfo/gg.gap) by Jiacheng Lou, et al.  

What changed with respect to upstream?  
- Improved handling of subtitles and captions. These were duplicated and placed in the gap between the new segments of the plot.  
- Legends are now supported by the core functionality.  
- The code was heavily refactored following the [tidyverse style guide](https://style.tidyverse.org/) and Clean Code principles.  
- The project has a new name after lossing the punctuation mark from the former name as per the recommendations of the style guide.  

Future work:  
- Support the creation of gaps on the x-axis  
- Introduce testing  
- Revise the examples provided  

[![](https://cranlogs.r-pkg.org/badges/gggap?color=orange)](https://cran.r-project.org/package=gggap)

## Installation

You can install `gggap` from CRAN:

``` r
install.packages("gggap")
```

A development version can be retrieved from Github:

``` r
# install.packages("devtools")
devtools::install_github("cmoralesmx/gggap", ref="dev")
```

## How to use gggap

``` r
data(mtcars)
library(ggplot2)
p <- ggplot(data = mtcars, aes(x = gear, fill = gear)) +
  geom_bar() +
  ggtitle("Number of Cars by Gears") +
  xlab("Gears")

# single segments and missing `tick_width`
gggap(plot = p, segments = c(5, 10), ylim = c(0, 50))

# `tick_width` can be one or more numbers
gggap(
  plot = p,
  segments = c(5, 10),
  tick_width = c(1, 10),
  ylim = c(0, 50)
)

# segments list cantains more than one number vectors
gggap(
  plot = p,
  segments = list(c(2.5, 4),c(5, 10)),
  tick_width = c(1, 0.5, 10),
  ylim = c(0, 50))

# `rel_heights` can set the relative height for segments and segmented y-axis
gggap(
  plot = p,
  segments = list(c(2.5, 4),c(5, 10)),
  tick_width = c(1, 0.5, 10),
  rel_heights = c(0.2, 0, 0.2, 0,1),
  ylim = c(0, 50)
)

# reversed y-axis
p <- ggplot(
  data = mtcars,
  aes(x = gear, fill = gear)
) +
geom_bar() +
ggtitle("Number of Cars by Gears") +
xlab("Gears")+
scale_y_continuous(trans = 'reverse')
# single segments and missing tick_width
gggap(plot = p, segments = c(10, 5), ylim = c(15, 0))

# for facet()
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
p1 <- p + facet_wrap(~cyl, scales = "free")
gggap(plot = p1, ylim = c(60, 200), segments = c(100, 120))
```

