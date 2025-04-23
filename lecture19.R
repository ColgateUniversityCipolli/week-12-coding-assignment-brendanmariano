################################################################################
# LECTURE 19 R CODE
# CIPOLLI
# MATH 240 - SPRING 2025
################################################################################
library(tidyverse)
library(patchwork)

################################################################################
# Significant? 
################################################################################
# Suppose we aim to evaluate a t-test under the following conditions:
mu0 <- 3
xbar <- 3.01
s <- 1.3

# Note: for the effect size, we need to use the gamma function. On my computer
#       it stops working at n=345.
# >   gamma((344-1)/2)
# [1] 9.483368e+307
# > gamma((345-1)/2
# [1] inf

t.dat.small <- tibble(n=1:345)|>
  # compute the t-statistic and Cohen's d
  mutate(tn = (xbar-mu0)/(s/sqrt(n)),
         cd = abs(xbar-mu0)/s) |> 
  # compute the correction for hedges g (not calculatable for n<=2)
  mutate(g.correction = case_when(n <= 2 ~ NA_real_,
                                  TRUE ~ gamma((n-1)/2) / (sqrt((n-1)/2)*gamma((n-2)/2))))|>
  mutate(g = cd * g.correction)

# What does the correction do over the sample size?
ggplot(data=t.dat.small)+
  geom_line(aes(x=n, y=g.correction))+
  xlab("Sample Size")+
  ylab("Hedges' Correction (Muliplicative)")+
  geom_hline(yintercept=1, color="red", linetype="dotted", linewidth=1)+
  theme_bw()+
  labs(caption="As the sample size increases, the correction goes to identity (1).")

# What does the t-statistic and effect size encode?
ggplot(data=t.dat.small)+
  geom_line(aes(x=n, y=tn, color="T Statistic"),
            linewidth=1.5)+
  geom_line(aes(x=n, y=cd, color="Cohen's D (biased)"),
            linewidth=1.5)+
  geom_line(aes(x=n, y=g, color="Hedges' g (unbiased)"),
            linewidth=1.5)+
  theme_bw()+
  ylab("Statistic")+
  labs(color="")+
  labs(caption="As the sample size increases, Cohen's d and Hedges' g converge. T statistic increases with n.")+
  ggtitle("Test Statistic and Effect Sizes",
          subtitle=bquote("For minimal support for"~mu[X]>3*", where"~{bar(x)==3.01}~"and"~{mu[X]==3}))



# Compute the t-statistic and effect size over various sample sizes
t.dat <- tibble(n=seq(0, 500000, 1000))|>
  # compute the t-statistic and Cohen's d
  mutate(tn = (xbar-mu0)/(s/sqrt(n)),
         cd = abs(xbar-mu0)/s) |> 
  # compute the correction for hedges g (not calculatable for n<=2)
  mutate(g.correction = case_when(n <= 2 ~ NA_real_,
                                  n < 345 ~ gamma((n-1)/2) / (sqrt((n-1)/2)*gamma((n-2)/2)),
                                  TRUE ~ 1) # note g approaches 1 as n increases
  )|>
  mutate(g = cd * g.correction)

# What does the t-statistic and effect size encode?
ggplot(data=t.dat)+
  geom_line(aes(x=n, y=tn, color="T Statistic"),
            linewidth=1.5)+
  geom_line(aes(x=n, y=cd, color="Cohen's D (biased)"),
            linewidth=1.5)+
  geom_line(aes(x=n, y=g, color="Hedges' g (unbiased)"),
            linewidth=1.5)+
  theme_bw()+
  ylab("Statistic")+
  labs(color="")+
  labs(caption="We can find discernible support as long as the sample size is large. Still, the effect size is minuscule.")+
  ggtitle("Test Statistic and Effect Sizes",
          subtitle=bquote("For minimal support for"~mu[X]>3*", where"~{bar(x)==3.01}~"and"~{mu[X]==3}))