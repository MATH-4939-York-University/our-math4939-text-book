##
##  MATH 6627
##  November 26, 2014 
##  Interpreting Type I, II and III Sums of Squares
##      Hypotheses and Tests
##


library(yscs)
library(car)
library(latticeExtra)
library(magrittr)


# plot(c(0,10),c(0,10))
# XY <- locator(20)
# xy<-as.data.frame(XY)
#################################  REDO:
xy <- read.table(text="
1  0.662 7.702
2  1.567 7.105
3  1.394 5.836
4  1.242 4.791
5  1.853 3.634
6  2.484 3.858
7  1.853 5.052
8  1.537 2.664
9  2.769 2.141
10 2.351 4.156
11 4.000 1.730
12 4.733 9.643
13 5.049 8.001
14 5.364 6.881
15 5.823 5.985
16 5.792 7.740
17 6.189 5.724
18 5.884 3.261
19 4.927 6.508
20 5.894 6.694
")
names(xy) <- c('id','x','y')
xy$g <- factor( rep(c('F',"M") , each = 10))
xy
gd()
xyplot( y ~ x, xy, groups = g, xlim=c(0,8), ylim=c(0,10), pch = 18)
fit.full <- lm(y ~ x * g, xy)
summary(fit.full)
wald(fit.full, 'g')

gd(lwd = 2)
(pp <- xyplot( y ~ x, xy, groups = g, xlim=c(0,10), ylim=c(0,10)))


(pp <- pp +
  glayer( panel.lines( x=dell( x, y),y=NULL, ..., type = 'l',
                       lwd = 2)))

(pp <- pp +
  glayer( panel.lmline(..., lwd = 2)))
          
(pp <- pp +
   glayer( panel.abline(v=mean(x), ...,lwd = 1))+
   glayer( panel.abline(h=mean(y), ...,lwd = 1))
)

(pp <- pp +
   layer( panel.abline(v=mean(x), ...,lwd = 1))+
   layer( panel.abline(h=mean(y), ...,lwd = 1)))
   
fit.add <- lm( y ~ x + g, xy)

xy$y.add <- predict(fit.add, newdata = xy)
(pp <- pp +
    xyplot( y.add ~ x, xy, groups = g, type = 'n')+
   glayer( panel.lmline(...)))

summary(fit.full)
anova(fit.full)
Anova(fit.full)  # Type 2
Anova(fit.full,type = 3) # does not correspond to SAS or SPSS

# Using the Prestige example in car 

summary(Prestige)
fit <- lm( prestige ~ income*education*type, Prestige)
summary(fit)

anova(fit)   # Type I sequential
Anova(fit)   # Marginal but omitting higher-order interactions
             # that include each effect BUT including
             # unrelated higher-order interactions
Anova(fit, type = 3)
Anova





