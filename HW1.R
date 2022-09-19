library(tidyverse)
library(data.table)
library(MASS)

psfd_20 <- read_csv("/Users/cccc0423/Documents/111/C00377_1/psfd_rr2020_v202208_csv.csv")

psfd_20 <- psfd_20 %>%
  mutate(working = ifelse(w03 == 3, 0, 1),
         Age = 109 - a02a)

psfd_20 %>%
  group_by(Age) %>%
  summarise(Working_Rate = mean(working)) %>%
  ggplot(mapping = aes(y = Working_Rate, x = Age)) +
  geom_histogram(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(title = "Working Rate against Age",
       y = "Mean of Working Dummy",
       x = "Age") +
  theme(text = element_text(size = 15))

ggsave("/Users/cccc0423/Documents/111/C00377_1/Figures/Labor-Econ-hw1-f1.pdf")

#### Simulation
set.seed(7777)

mu0 <- 90
mu1 <- 100
s0 <- 5
s1 <- 5
s01 <- 2
c <- 10

s <- matrix(c(s0^2, s01, s01, s1^2), nrow = 2, byrow = TRUE)

df <- mvrnorm(n = 1e7,
              mu = c(0, 0), 
              Sigma = s) %>% as.data.table()

names(df) <- c("e0", "e1")

df <- df %>%
  mutate(w0 = mu0 + e0,
         w1 = mu1 + e1,
         i = ifelse(w1 >= w0 + c, 1, 0))

# Calculate E[w0 | I], E[w1 | I], Q0, and Q1
Exp.w0.I.est <- df[i == 1]$w0 %>% mean()
Exp.w1.I.est <- df[i == 1]$w1 %>% mean()
Q0.est <- df[i == 1]$e0 %>% mean()
Q1.est <- df[i == 1]$e1 %>% mean()

# RHS of (1) and (2)
snu <- (s0^2 + s1^2 - 2 * s01)**(1/2)
z <- (mu0 - mu1 + c) / snu
rho <- s01 / (s0 * s1)
lambda <- dnorm(z) / (1 - pnorm(-z))

Exp.w0.I <- mu0 + ((s0 * s1) / snu) * (rho - s0/s1) * lambda
Exp.w1.I <- mu1 + ((s0 * s1) / snu) * (s1/s0 - rho) * lambda




