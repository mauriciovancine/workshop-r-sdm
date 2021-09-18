
library(tidyverse)

setwd("/home/mude/data/github/workshop-r-sdm/01_slides/img")

set.seed(123)
da <- tibble::tibble(x = c(rnorm(50, -1, 1), rnorm(50, 1, 1)),
                     y = c(rbinom(50, 0, .2), rbinom(50, 1, .8)))
da

glm(y ~ x, data = da, family = "binomial")

ggplot(data = da, aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(y)), size = 5, alpha = .7) +
  geom_smooth(color = "black", size = 2, method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = c("orange", "steelblue")) +
  labs(x = "Precipitação (padronizada)", y = "Pseudo-ausência/Presença") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
ggsave(filename = "sdm_metodos_glm.png", width = 20, height = 15, un = "cm", dpi = 300)

ggplot(data = da, aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(y)), size = 5, alpha = .7) +
  geom_smooth(color = "black", size = 2, method = "gam", method.args = list(family = "binomial")) +
  scale_color_manual(values = c("orange", "steelblue")) +
  labs(x = "Precipitação (padronizada)", y = "Pseudo-ausência/Presença") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
ggsave(filename = "sdm_metodos_gam.png", width = 20, height = 15, un = "cm", dpi = 300)


1/(1 + exp(1)^(-(-0.733+1.169*-2)))
1/(1 + exp(1)^(-(-0.733+1.169*-1)))
1/(1 + exp(1)^(-(-0.733+1.169*0)))
1/(1 + exp(1)^(-(-0.733+1.169*1)))
1/(1 + exp(1)^(-(-0.733+1.169*2)))

