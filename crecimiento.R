# https://link.springer.com/article/10.1007/s40502-018-0400-x
# https://rpubs.com/venkatritch/333327

asymptote / (1 + exp((midpoint - age) * scale)))
f1 <- function(x) 8000 / (1 + exp((70 - x) * 0.1))
f1_deriv <- function(x) 8000 * (exp((70 - x) * 0.1) * 0.1)/(1 + exp((70 - x) * 0.1))^2
optimize(f1_deriv, interval=c(30, 60), maximum=TRUE)

f1_deriv2 <- function(x) -(8000 * (exp((70 - x) * 0.1) * 0.1 * 0.1)/(1 + exp((70 - x) * 
                                                                               0.1))^2 - 8000 * (exp((70 - x) * 0.1) * 0.1) * (2 * (exp((70 - 
                                                                                                                                           x) * 0.1) * 0.1 * (1 + exp((70 - x) * 0.1))))/((1 + exp((70 - 
                                                                                                                                                                                                      x) * 0.1))^2)^2)
  
f1_expre = expression(8000 / (1 + exp((70 - x) * 0.1)))
f2_express <- D(f1_expre, "x")
f3_express <- D(D(f1_expre,'x'),'x')

integrate(f1_deriv,0,120)
integrate(f1_deriv,0,30)
integrate(f1_deriv,30,60)
integrate(f1_deriv,60,90)

library(tidyverse)

theme_set(theme_minimal())

p1 <- ggplot(data.frame(x = c(0, 120)), aes(x)) + 
  stat_function(fun = f1, col = "red3")+ 
  labs(x="Dias desde emergencia", y = "kg/ha",
       title = "Curva de acumulación de MS",  
       subtitle = "y=f(x)") +
  scale_x_continuous(
    name = "Dias", 
    limits = c(0, 120), 
    breaks = scales::extended_breaks(Q = c(30, 10)))  

p1

p2 <- ggplot(data.frame(x = c(0, 120)), aes(x)) + 
  stat_function(fun = f1_deriv, col= "steelblue") +
  labs(x="Dias desde emergencia", y = "kg/ha/dia", 
       title= "Tasa instantánea de crecimiento",  
       subtitle = "y=f'(x)") +
  scale_x_continuous(
    name = "Dias", 
    limits = c(0, 120), 
    breaks = scales::extended_breaks(Q = c(30, 10)))#+
p2

p3 <- ggplot(data.frame(x = c(0, 120)), aes(x)) + 
  stat_function(fun = f1_deriv2, col= "green3") +
  labs(x="Dias desde emergencia", y = "", 
       title= "Segunda derivada",  
       subtitle = "y=f''(x)") +
  scale_x_continuous(
    name = "Dias", 
    limits = c(0, 120), 
    breaks = scales::extended_breaks(Q = c(30, 10))) 
p3

library(patchwork)  
p1 / p2
ggsave(last_plot(), file = "figs/MS_deriv.png", w =4, h = 5)

(p1+geom_vline(xintercept = 70, linetype = "dashed"))/
  (p2+geom_vline(xintercept = 70, linetype = "dashed"))/
  (p3+geom_vline(xintercept = 70, linetype = "dashed")+
     geom_hline(yintercept = 0, col = "grey30"))

ggsave(last_plot(), file = "figs/MS_deriv2.png", w =4, h = 6)

df = data.frame(x = seq(1, 120, length.out = 120))
df$y = f1_deriv(df$x)

int_30_60 <- integrate(f1_deriv,30,60)

ggplot(df, aes(x, y)) + 
  geom_line()+
  # stat_function(fun = f1_deriv, aes(colour="y=f'(x)")) +
  labs(x="Dias desde emergencia", y = "kg/ha/dia",  
       title = "Tasa instantanea de crecimiento", 
       subtitle = "Integral de y=f'(x) entre t=30 y 60") +
  scale_x_continuous(
    limits = c(0, 120), 
    breaks = scales::extended_breaks(Q = c(30, 10)))+
  geom_area(data = df[df$x > 30 & df$x < 60,], aes(x = x, y = y),
            fill = "red", alpha = 0.5)+
  annotate("text", x = 45, y = 25, 
           label = paste0("2007 kg/ha"))
ggsave(last_plot(), file = "figs/integral.png", w =5, h = 4)
