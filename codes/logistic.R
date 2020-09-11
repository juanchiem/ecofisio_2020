# https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve/#fn:reparameter
# https://rpubs.com/erobinson95/nonlinearsoybeangrowth
# https://rpubs.com/angelov/growthcurver
# https://rpubs.com/venkatritch/333327

library(tidyverse)
theme_set(theme_minimal())

points <- tibble(
  dias = c(38, 45, 52, 61, 80, 74), 
  qq = c(14, 24, 57, 74, 84, 74))

colors <- list(
  data = "#41414550",
  fit = "#414145")

ggplot(points) + 
  aes(x = dias, y = qq) + 
  geom_point(size = 3.5, color = colors$data) +
  scale_x_continuous(
    name = "Dias", 
    limits = c(0, 96), 
    # Because age is in months, I want breaks to land on multiples
    # of 12. The `Q` in `extended_breaks()` are "nice" numbers to use
    # for axis breaks.
    breaks = scales::extended_breaks(Q = c(24, 12))) + 
  scale_y_continuous(
    name = "qq",
    limits = c(0, NA))#,
    # labels = scales::percent_format(accuracy = 1))

xs <- seq(0, 96, length.out = 80)

# Create the curve from the equation parameters
trend <- tibble(
  dias = xs,
  asymptote = 80,
  scale = .2,
  midpoint = 48,
  qq = asymptote / (1 + exp((midpoint - dias) * scale)))

ggplot(points) + 
  aes(x = dias, y = qq) + 
  geom_line(data = trend, color = colors$fit) +
  geom_point(size = 3.5, color = colors$data) +
  scale_x_continuous(
    name = "Age in months", 
    limits = c(0, 96), 
    breaks = scales::extended_breaks(Q = c(24, 12))) + 
  scale_y_continuous(
    name = "Intelligibility",
    limits = c(0, NA))

colors$asym <- "#E7552C"
colors$mid <- "#3B7B9E"
colors$scale <- "#1FA35C"

p <- ggplot(points) +
  aes(x = dias, y = qq) +
  annotate(
    "segment",
    color = colors$mid,
    x = 48, xend = 48,
    y = 0, yend = 40,
    linetype = "dashed") +
  annotate(
    "segment",
    color = colors$asym,
    x = 20, xend = Inf,
    y = 80, yend = 80,
    linetype = "dashed") +
  geom_line(data = trend, size = 1, color = colors$fit) +
  geom_point(size = 3.5, color = colors$data) +
  annotate(
    "text",
    label = "plateau de crecimiento en la asintota",
    x = 20, y = 84,
    # horizontal justification = 0 sets x position to left edge of text
    hjust = 0,
    color = colors$asym) +
  annotate(
    "text",
    label = "maximo crecimiento en el punto central",
    x = 49, y = 5,
    hjust = 0,
    color = colors$mid) +
  scale_x_continuous(
    name = "Dias", 
    limits = c(0, 96), 
    breaks = scales::extended_breaks(Q = c(24, 12))) + 
  scale_y_continuous(
    name = "qq",
    limits = c(0, NA))
p

# Compute endpoints for segment with given slope in middle
slope <- (.2 / 4) * 80
x_step <- 2.5
y1 <- 40 + slope * -x_step
y2 <- 40 + slope * x_step

p <- p +
  geom_segment(
    x = 48 - x_step, xend = 48 + x_step,
    y = y1, yend = y2,
    size = 1.2,
    color = colors$scale,
    arrow = arrow(ends = "both", length = unit(.1, "in"))) +
  annotate(
    "text",
    label = "La escala controla la pendiente de la curva",
    x = 49, y = 38, 
    color = colors$scale, hjust = 0)
p

# Define logistic fn and it's first derivative.
f1 <- function(x) 1 / (exp(-x)+1)  
f2 <- function(x) f1(x)*(1-f1(x))


# generate plot
ggplot(data.frame(x = c(-10, 10)), aes(x)) + 
  stat_function(fun = f1, aes(colour="y=f(x)"))+ 
  stat_function(fun = f2, aes(colour="y=f'(x)")) + 
  ggtitle("Curva logística de crecimiento y su derivada") + 
  xlab("x") + 
  ylab("y") 

