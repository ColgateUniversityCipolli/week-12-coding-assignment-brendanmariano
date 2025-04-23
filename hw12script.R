library(tidyverse)
library(VGAM)
# 1.a
(t.val = qt(.95, df = 19))
# 1.b
(t.val2 = qt(.95, df = 29)) 
# 1.c
type1.cnt = 0
for(i in 1:1000){
  dat = rlaplace(n = 30, scale = 4)
  p.20 = t.test(dat[1:20], alternative = "greater")
  p.30 = t.test(dat, alternative = "greater")
  if(p.20$statistic > t.val){
    type1.cnt = type1.cnt + 1
  }
  else if(p.30$statistic>t.val2)
  {
    type1.cnt = type1.cnt + 1
  }
}
proportion.type1 = type1.cnt/1000
#############
# Part 2
#2.a
actual.mean1 = 10/(10+2)
actual.mean2 = 2/(2+10)
actual.mean3 = 10/(10+10)
#Beta(10,2)
total.b1 = 0
for(i in 1:1000){
  dat.b1 = rbeta(n = 15, shape1 = 10, shape2 = 2)
  b1 = t.test(dat.b1, alternative = "less", mu = actual.mean1)
  if(b1$p.value < .05){ #Because we know that the null should't be rejected
    total.b1 = total.b1 + 1
  }
}
type1.error.b1 = total.b1/1000 #Proportion of type1 errors
#Beta(2,10)
total.b2 = 0
for(i in 1:1000){
  dat.b2 = rbeta(n = 15, shape1 = 2, shape2 = 10)
  b2 = t.test(dat.b2, alternative = "less", mu = actual.mean2)
  if(b2$p.value < .05){
    total.b2 = total.b2 + 1
  }
}
type1.error.b2 = total.b2/1000
#Beta(10,10)
total.b3 = 0
for(i in 1:1000){
  dat.b3 = rbeta(n = 15, shape1 = 10, shape2 = 10)
  b3 = t.test(dat.b3, alternative = "less", mu = actual.mean3)
  if(b3$p.value < .05){
    total.b3 = total.b3 + 1
  }
}
type1.error.b3 = total.b3/1000
#2.b


#Beta(10,2)
total.a1 = 0
for(i in 1:1000){
  dat.a1 = rbeta(n = 15, shape1 = 10, shape2 = 2)
  a1 = t.test(dat.a1, alternative = "greater", mu = actual.mean1)
  if(a1$p.value < .05){
    total.a1 = total.a1 + 1
  }
}
type1.error.a1 = total.a1/1000
#Beta(2,10)
total.a2 = 0
for(i in 1:1000){
  dat.a2 = rbeta(n = 15, shape1 = 2, shape2 = 10)
  a2 = t.test(dat.a2, alternative = "greater", mu = actual.mean2)
  if(a2$p.value < .05){
    total.a2 = total.a2 + 1
  }
}
type1.error.a2 = total.a2/1000
#Beta(10,10)
total.a3 = 0
for(i in 1:1000){
  dat.a2 = rbeta(n = 15, shape1 = 10, shape2 = 10)
  a3 = t.test(dat.a3, alternative = "greater", mu = actual.mean3)
  if(a3$p.value < .05){
    total.a3 = total.a3 + 1
  }
}
type1.error.a3 = total.a3/1000

#2.b

#Beta(10,2)
total.b1 = 0
for(i in 1:1000){
  dat.b1 = rbeta(n = 15, shape1 = 10, shape2 = 2)
  b1 = t.test(dat.b1, alternative = "less", mu = actual.mean1)
  if(b1$p.value < .05){
    total.b1 = total.b1 + 1
  }
}
type1.error.b1 = total.b1/1000
#Beta(2,10)
total.b2 = 0
for(i in 1:1000){
  dat.b2 = rbeta(n = 15, shape1 = 2, shape2 = 10)
  b2 = t.test(dat.b2, alternative = "less", mu = actual.mean2)
  if(b2$p.value < .05){
    total.b2 = total.b2 + 1
  }
}
type1.error.b2 = total.b2/1000
#Beta(10,10)
total.b3 = 0
for(i in 1:1000){
  dat.b3 = rbeta(n = 15, shape1 = 10, shape2 = 10)
  b3 = t.test(dat.b3, alternative = "less", mu = actual.mean3)
  if(b3$p.value < .05){
    total.b3 = total.b3 + 1
  }
}
type1.error.b3 = total.b3/1000
###########
#2.c

#Beta(10,2)
totalc1 = 0
for(i in 1:1000){
  datc1 = rbeta(n = 15, shape1 = 10, shape2 = 2)
  c1 = t.test(datc1, alternative = "two.sided", mu = actual.mean1)
  if(c1$p.value < .05){
    totalc1 = totalc1 + 1
  }
}
type1.errorc1 = totalc1/1000
#Beta(2,10)
total.c2 = 0
for(i in 1:1000){
  dat.c2 = rbeta(n = 15, shape1 = 2, shape2 = 10)
  c2 = t.test(dat.c2, alternative = "two.sided", mu = actual.mean2)
  if(c2$p.value < .05){
    total.c2 = total.c2 + 1
  }
}
type1.error.c2 = total.c2/1000
#Beta(10,10)
total.c3 = 0
for(i in 1:1000){
  dat.c3 = rbeta(n = 15, shape1 = 10, shape2 = 10)
  c3 = t.test(dat.c3, alternative = "two.sided", mu = actual.mean3)
  if(c3$p.value < .05){
    total.c3 = total.c3 + 1
  }
}
type1.error.c3 = total.c3/1000

dat5 = tibble(x=rbeta(n = 40, shape1 = 2, shape2 = 10))

ggplot() +
  geom_histogram(data = dat5, aes(x = x, y = after_stat(density)))