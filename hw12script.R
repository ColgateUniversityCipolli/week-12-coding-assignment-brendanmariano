library(tidyverse)
library(VGAM)
# 1.a

(t.val = qt(.95, df = 19))
# 1.b
(t.val2 = qt(.95, df = 29)) 
# 1.c
val3 = qt(.95,999)

type1.cnt = 0
for(i in 1:1000){
  dat = rlaplace(n = 30, scale = 4)
  if(t.val > quartile(dat, probs = .95)){
    type1.cnt = type1.cnt + 1
  }
}



