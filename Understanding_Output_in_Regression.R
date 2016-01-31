#' ---
#' title: "MATH 6627: Understanding Output in Regression"
#' author: "Georges Monette"
#' date: "November 2014"
#' output:
#'   html_document:
#'     toc: true
#'     theme: united
#' ---
#' 
#' 
#' **Our theme:**<br> 
#'   **With complex regression models:**<br>
#'   **Most estimated coefficients are likely to be of little interest, and**<br>
#'   **the answers to important questions won't be in your estimated coefficients**
#' 
#' This R Markdown script is a discussion about understanding output in regression.  
#' Regression is at the conceptual core of statistics.  
#' Initially, we focus on linear regresion where our goal is to be able to 
#' - interpret every parameter estimated in regression outupt, focussing initially on regression coefficients
#' - formulate which parameters and functions of parameters are relevant to a subject-matter question
#' - estimate and test parameters and groups of parameters, by
#' - formulating linear hypotheses for regression coefficients and applying Wald tests, or
#' - reparametrizing the model and testing hypotheses for the reparametrized model, or 
#' - formulating and fitting null models followed by a likelihood ratio test to compare the full model with the null model
#' 
#' We want to understand the role and limitations of a number of tools:
#' - estimated regression coefficients
#' - Wald tests for linear hypotheses
#' - 'anova' for a likelihood ratio test of two models
#' - 'Anova' (in the 'car' package) sums of squares: Type I, Type II and Type III and their use and interpretation
#' 
#+ setup, include=FALSE
## read data and set up libraries
library(latticeExtra)
library(knitr)
opts_chunk$set(cache=FALSE,message=FALSE,comment="|  ")    # for information see http://yihui.name/knitr/options#chunk_options
if(!file.exists("Smoking3.csv")) download.file("http://blackwell.math.yorku.ca/Data/Smoking/Smoking3.csv","Smoking3.csv")
dall <- read.csv("Smoking3.csv")
dd <- subset( dall, sex == 'BTSX')   # subset of a data frame (combined sexex)
dd$LifeExp <- dd$lifeexp.Birth
dd$smoke <- dd$consumption.cigPC
dd$HE <- dd$HealthExpPC.Tot.ppp
dd$hiv <- dd$hiv_prev15_49
dd$special <- ifelse( 
        dd$country %in% c('Angola','Sierra Leone','Equatorial Guinea'), 
        1, 
        0)
library(yscs)
library(car)
#'
#'  
#' # Understanding Regression
#' 
#' Consider four approaches to regression: statistical theory, the mathematical formulation of models, computational expression of models in the language of a statistical package and the graphical representation of models. 
#' 
#' To master regression you need to know how to go from one representation to another and you need to know how to work within each representation to solve problems.
#' 
#' ### Statistical
#'  
#' $$
#' Y = X \beta + \epsilon,\: \epsilon \sim N(0,\sigma^2)
#' $$
#' and all the theory that follows, e.g.
#' $$
#' Var(\hat{\beta}) = \sigma^2 \left( X'X \right)^{-1}
#' $$
#' 
#' ### Mathematical
#' 
#' $$
#'   y_i = \beta_0 + \beta_1 x_i + \beta_2 x_i^2 + \epsilon_i \\
#' \frac{\partial E(y)}{\partial x} = \beta_1 + 2\,\beta_2 x
#' $$
#' 
#' ### Computing
library(car)
fit <- lm(income ~  education * type, data = Prestige)
summary(fit)
```

### Graphical: data space
```{r}
library(car)
xyplot(income ~ education | type, Prestige, type = c('p','r','smooth'))

```
### Graphical: beta space
**Figure:** Confidence ellipse for two parameters jointly. The blue ellipse has 95% coverage in 2 dimensions and its perpendicular shadows onto the vertical and horizontal axes form Scheffe 95% confidence intervals for testing in a space of dimension 2. The similar shadows of the red ellipse provide ordinary 95% confidence intervals.    
```{r,warning=FALSE,message=FALSE}
library(yscs)
library(latticeExtra)
fit <- lm(income ~  education + women, data = Prestige)
summary(fit)
plot(rbind(cell(fit),0),type= 'n',
     xlab = expression(beta[education]),
     ylab = expression(beta[women]))
lines(cell(fit,dfn=2), type = 'l', col = 'blue') 
lines(cell(fit,dfn=1), type = 'l', col = 'red') 
abline(h=0)
abline(v=0)
points(c(0,0), pch = 18)
```


## Interpreting Regression Coefficients: Smoking and Life Expectancy
  
With complex models: 
  
1. most regression coefficients are of little interest and 
2. most interesting questions are not answered with the regression coefficients.

Why do we pay attention to regression output? Because it may make some sense for very simple additive models -- but even then it is fraught with subtle traps most analysts do not understand.

We will illustrate these with the Smoking and Life Expectancy example.

```{r first_regression}
fit.hiv2 <- lm( LifeExp ~ log(HE) * (smoke + I(smoke^2)) + hiv+special, dd , 
                na.action = na.exclude) 
summary(fit.hiv2)
```

To interpret these coefficient, we consider the mathematical formula for the model:

Letting $\eta = E(y|HE,Smoke,HIV,Special)$
$$
\begin{aligned}
\eta = &\beta_0
+\beta_1 \times ln(HE) \\
& +\beta_2 \times Smoke \\
& +\beta_3 \times Smoke^2 \\
& +\beta_4 \times HIV \\
& +\beta_5 \times Special \\
& +\beta_6 \times ln(HE) \,Smoke \\
& +\beta_7 \times ln(HE) \,Smoke^2
\end{aligned}
$$
  
To understand the interpretation of the coefficients $\beta_i$, we differentiate $\eta$
with respect to each of the independent variables:

$$
\begin{aligned}
\frac{\partial \eta}{\partial HE} & = \beta_1 \frac{1}{HE}+\beta_6 \frac{Smoke}{HE}
+ \beta_7  \frac{Smoke^2}{HE} \\
\frac{\partial \eta}{\partial Smoke} & = \beta_2 + 2 \beta_3 Smoke +\beta_6 ln(HE)
+ 2 \beta_7 Smoke\, ln(HE) \\
\frac{\partial \eta}{\partial HIV} & = \beta_4 \\
\frac{\partial \eta}{\partial Special} & = \beta_5 \\
\frac{\partial^2 \eta}{\partial HE^2} & = \beta_1 \frac{-1}{HE^2}+\beta_6 \frac{-Smoke}{HE^2}
+ \beta_7  \frac{-Smoke^2}{HE^2} \\
\frac{\partial^2 \eta}{\partial Smoke^2} & =  2 \beta_3 \\
\frac{\partial^2 \eta}{\partial HE \, \partial Smoke} & = \beta_6 \frac{1}{HE}
+ 2 \beta_7  \frac{Smoke}{HE} \\
\end{aligned}   
$$
  
Thus $\beta_2$ is the **partial derivative** of $\eta$ with respect to $Smoke$ when $ln(HE) = Smoke = 0$. 

When $ln(HE) = 5$ and $Smoke = 4$, the partial derivative of  $\eta$ with respect to $Smoke$ is

$$
  \frac{\partial \eta}{\partial Smoke} = \beta_2 + 8 \beta_3 +5 \beta_6 
+ 40 \beta_7
$$
whose estimator is
$$
  \hat{\beta_2} + 8 \hat{\beta_3}  +5 \hat{\beta_6} 
+ 40 \hat{\beta_7}
$$
which we can express as a linear transformation of the $\hat{\beta}$ vector. 
Letting 
$$L = \left[ {\begin{array}{*{20}{c}}0&0&1&8&0&0&5&{40}\end{array}} \right]$$
we have:
\[\hat{\phi} = L \hat{\beta} = \left[ {\begin{array}{*{20}{c}}0&0&1&8&0&0&5&{40}\end{array}} \right]\widehat {\left[ {\begin{array}{*{20}{c}}{{\beta _0}}\\{{\beta _1}}\\{{\beta _2}}\\{{\beta _3}}\\{{\beta _4}}\\{{\beta _5}}\\{{\beta _6}}\\{{\beta _7}}\end{array}} \right]}\]

If we wish to simultaneously estimate the 'effect' of Smoke and the 'effect' of HE given values of HE and Smoke, we can form the $L$ matrix:
$$
L = \left[ {\begin{array}{*{20}{c}}
          0 & 0 & 1 & 2\, Smoke & 0 & 0 & ln(HE) & 2\, Smoke \, ln(HE) \\
          0 & 1 & 0 & 0       & 0 & 0 & \frac{Smoke}{HE} &\frac{Smoke^2}{HE}
          \end{array}} \right]  
$$
and
$$
\hat{\phi} = L \hat{\beta}
$$
In this case $\hat{\phi}$ is a column vector of length 2. 

In both cases, inference about $\phi$ uses the fact that
$$
Var(\hat{\phi}) = L Var(\hat{\beta}) L'
$$
and 
$$
Var(\hat{\beta}) = \sigma^2 (X'X)^{-1}
$$
With a normal linear model in which
$$
Y = X \beta + \epsilon, \:\epsilon \sim N(0,\sigma^2 \,I) 
$$
we have that
$$
\left( {\hat{\phi} - \phi} \right)'\left( {s^2 L(X'X)^{-1} L'} \right)^{-1} \left( {\hat{\phi} - \phi} \right)    \sim h \times F_{h,\nu}
$$
where $h$ is the number of rows of $L$ (assuming that $L$ is of full row rank) and $\nu = n - p$ where $n$ and $p$ are the number of rows and columns of $X$ respectively, again assuming that $X$ is of full  column rank.  

We can compute these quantities in R from a fitted model. 
```{r}
L <- evalq( rbind( 
c( 0,0,1, 2*smoke, 0 ,0 , log(HE), 2*smoke*log(HE)),
c( 0,1,0, 0      , 0,       0, smoke / HE, smoke^2/HE)), envir = list( smoke = 4, HE = exp(5)))
L
```
$\hat{\beta}$:
```{r}
coef(fit.hiv2)
```
$\hat{\phi} = L \hat{\beta}$:
```{r}
(phihat <- L %*% coef(fit.hiv2))
```
$s^2 (X'X)^{-1}$:
```{r}
vcov(fit.hiv2)
```
$\hat{Var}(\hat{\phi}) = L (s^2 (X'X)^{-1}) L'$:

```{r}
(Vphihat <- L %*% vcov(fit.hiv2) %*% t(L))
```
To test the hypothesis that $\phi = 0$, we have

$F = \hat{\phi}'\left({ \hat{Var}(\hat{\phi})}\right) ^{-1}\hat{\phi}/h$
```{r}
(Ftest <- (t(phihat) %*% solve(Vphihat) %*% phihat)/2)
1-pf(Ftest,2, fit.hiv2$df.residual)
pf(Ftest,2, fit.hiv2$df.residual, lower.tail = FALSE)
```

Functions to test linear hypotheses
-------------------------------
The functions 'lht' in the 'car' package and 'wald' in the 'spida' package can be used to test General Linear Hypotheses.

```{r}
require(car)
lht(fit.hiv2,L)
wald(fit.hiv2, L)
```
The 'lht' function can take a right-hand side to test hypotheses of the form $H_0 : \phi = \phi_0$. 'wald' can only test $H_0 : \phi = 0$.
The 'wald' function can handle $L$ matrices that are row rank deficient.  For example, in some uses of 'wald', the 'L' matrix is the whole design matrix.

The second argument of the 'wald' matrix can be a regular expression that is matched against the names of terms in the model.  All terms matched by the regular expression are simultaneously tested.  Thus one can test the 'overall' siginificance of an independent variable by testing whether all terms containing that variable are equal to 0. One can also use this approach to test higher-order interactions.
```{r}
wald(fit.hiv2, "smoke")   # is there statistical evidence that 'smoke' improves prediction?
wald(fit.hiv2, "HE")      # ditto for HE
wald(fit.hiv2, ":")       # ditto for interactions?
wald(fit.hiv2, "2)" )     # ditto for quadratic terms?
```
There are many strategies for potentially simplifying large models. One is to attack higher-order interactions and simplify the model by dropping groups of interactions that are not significant.  Results may depend on the precise strategy.  Another approach is to drop all terms for selected independent variables if they are not sufficiently significant in an overall test.  The end result will typically very much depend on the original approach selected.  The choice of approach should be guided by many factors: which null hypotheses are likely to be reasonable, the interpretive value of having a simple additive model versus the added validity of estimating conditional effects that are not averaged over levels of variables that may be important, etc. There's a good discussion of these problems in Snijders & Bosker.

Estimating effects over a grid
------------------------------

In a model with interactions and non-linear functions of some independent variables, it is often necessary to characterize how effects (partial derivatives) and inferences about effects vary over a range of predictors.

The 'effects' package by John Fox can help with this task.  It can also be done in a more laborious but possibly more flexible way with the 'Lfx' function in the 'spida' package.  The 'Lfx' function generates an expression which can then be edited to generate large $L$ matrices. The result of the wald test applied to this $L$ matrix can be transformed into a data frame for plotting.
```{r}
Lfx(fit.hiv2)
```
The expression generated by 'Lfx' can be edited to generate desired effects. The result is then fed back to 'Lfx' along with a data frame on which to evaluate the edited expression. Note that the 'M' functions preserved the shape of multi-term blocks in the design matrix so that multiplying them by 0 is a way of generating a block of 0s of the right dimension. In the following, we edit the expression to 
estimate the effect of smoking by differentiating with respect to 'smoke':
```{r}
pred <- expand.grid(HE = c(50,150,500, 1000, 1500, 5000), smoke = seq(10,2000,20), hiv = 0, special = 0)
head(pred)  # first 6 lines of 'pred'

L <- Lfx(fit.hiv2,
    list( 0,  
          0 * M(log(HE)),
          1 ,
          1 * M(I(2*smoke)),
          0 * hiv,
          0 * special,
          1 * M(log(HE)) * 1,
          1 * M(log(HE)) * M(I(2*smoke)) 
    ), pred)
dim(L)
head(L)
ww <- wald(fit.hiv2, L)
ww <- as.data.frame(ww)
head(ww)
xyplot( coef ~ smoke, ww, groups = HE, auto.key = list(columns = 3, lines = T, points = F),type = 'l')
```
With labels that are more informative:

**Figure 1:** Change in Life Expectancy associated with a increase in cigarette consumption of 1 cigarette per day per capita for different levels of health expenditures per capita per year (US$).
```{r,echo=FALSE,fig.align='left'}
library(latticeExtra)
gd(lwd=2)
xyplot( I(365*coef) ~ I(smoke/365), ww, groups = HE, type = 'l', 
        xlim = c(0,5.5),
        ylab = "Change in predicted LE per additional cigarette",
        xlab = "cigarettes per capita per day",
   auto.key = list( lines = T, points = F, space = 'right', 
                   title = "Health Exp.",cex.title = 1)) 

## with +/- 2 SE


#################################   HERE ####################################### 

gd(lwd=2, lty = 1)
# to get nice strip labels
ww$HE.o <- factor( as.character(ww$HE), levels= paste(as.character(unique(sort(ww$HE)))))
xyplot( I(365*coef)  ~ I(smoke/365) | HE.o, ww, type = 'n',
        #fit = 365*ww$coef,
        xlim = c(0,5.5),
        ylab = "Change in predicted LE per additional cigarette",
        xlab = "cigarettes per capita per day",
        groups = HE,
        fit = 365*(ww$coef),
        lower = 365*(ww$coef - ww$se),
        upper = 365*(ww$coef + ww$se),
        subscripts = T,
        strip = strip.custom(style=5),
        as.table = T) +
  glayer(gpanel.fit(...)) + layer( panel.abline( h = 0, lwd = 1))

## 'meaningful' coefficients: cigarettes per day
ww$coef2 <- 365 * ww$coef
ww$se2 <- 365 * ww$se
ww$smoke2 <- ww$smoke/365
ww$HE2 <- with(ww, reorder(factor(paste("HE =", HE)),HE))

## predicted life expectancy
ww$pred <- predict(fit.hiv2, ww)  # predicted value
td( col = brewer.pal(8,"Reds")[-(1:2)], lty = 1, lwd = 1) 
xyplot( pred ~ smoke2, ww, groups = HE, type = 'l',
   ylab = 'life expectancy',
   xlab = 'cigarettes/day',
   auto.key = list(columns = 6, lines = T, points = F, title = "health expenditures per capita"),
   scales = list(x=list(axs = 'i')),
   sub = "predicted life expectancy for countries with very low prevalence of HIV")+
layer_( {panel.abline(v=0:5,col='grey')
      panel.abline(h=seq(30,80,5),col='grey')})
# a caption should give more details: Health expenditures measures in what? ... etc.
## 'effect' of increasing smoking by 1 cigarette/day
td( lty = c(1,3,3), lwd = 1, col = 'blue')
xyplot( coef2 + I(coef2+2*se2) + I(coef2-2*se2) ~ smoke2 | HE2, ww,type = 'l', 
   ylab = "Change in years of LE predicted by extra cigarette/day",
   xlab = 'Cigarettes/day',
   ylim = c(-5,5),
   xlim = c(0,5.5),
   main = "Is this a predictive or a causal relationship?") +
layer_( panel.abline( h = 0, col = 'gray'))
# why do we not need to mention hiv here

## double checking our previous calculation:
ptest <- expand.grid(smoke = 4, HE = exp(5), hiv = 0, special = 0)

(L2 <- Lfx( fit.hiv2,
       list( 0,  
             0 * M(log(HE)),
             1 ,
             1 * M(I(2*smoke)),
             0 * hiv,
             0 * special,
             1 * M(log(HE)) * 1,
             1 * M(log(HE)) * M(I(2*smoke)) 
       ), ptest)) 
wald(fit.hiv2, list("At smoke = 4, HE = 148.4"=L2))

```
Exercises
---------
1. Carry out a similar process to estimate the 'effect' of health expenditures per capita.
2. Study the relative contribution of private versus public health expenditures on life expectancy.
3. Explore the 'effects' package and compare its functionality with 'Lfx'

Wald tests vs Likelihood Ratio Tests (LRT)
============================================
Let's consider a test for the need for a quadratic term in 'smoke'. There are two terms in the model that contain the quadratic term and a test to remove it involves more than one parameter.  We need a test of 
$$
H_0: \beta_3 = \beta_7 = 0
$$
We cannot simply test each hypothesis $H_0: \beta_3  = 0$ and $H_0: \beta_7 = 0$ separately. We will see many examples where individual hypotheses are not significant, yet a joint hypothesis is highly significant. This is not a example of this phenomenon since the p-value for each hypothesis is small. Nevertheless, a test of a joint hypothesis needs to be carried out correctly.  We consider two ways: a Wald test and a Likelihood Ratio Test executed with the 'anova' function, a clear misnomer.
```{r}
## Wald test using indices of coefficients
wald(fit.hiv2, c(4,8))
wald(fit.hiv2, list("Quadratic in smoke" =c(4,8)))

## Wald test using regular expression
wald(fit.hiv2, "2")

## Likelihood ratio test
# We need to fit the 'null' model
fit0 <- update(fit.hiv2, .~ log(HE)*smoke + hiv + special)
summary(fit0)

# Then compare the null model with the 'full' model:
#    By default, 'anova' uses an F distribution for the LRT 
#    taking advantage of the linear gaussian model
anova(fit0, fit.hiv2)  

# Using the general asymptotic distribution, chi-square, for the LRT gives a slightly
# different but very close result
anova(fit0, fit.hiv2, test="LRT")

```
The Wald test and the LRT using the F statistics give identical results.  This is the luxury of working with a Gaussian homoskedastic independent linear model. 
Exercises
-----------
1. Explore the pros and cons of Wald tests versus Likelihood Ratio Tests.

Interpreting sequential tests
==============================
```{r}

## Type I: sequential tests
anova(fit.hiv2)  # sequential - Type 1 tests

## Type 2: Each term added last except for higher-order interactions
require(car)
Anova(fit.hiv2)  # Type 2 is default

## Type 3: Each term added last
require(car)
Anova(fit.hiv2, type = 3)  

## Type 3 is identical to regression output except that it uses equivalent F tests
## and a single test for terms with multiple degrees of freedom
summary(fit.hiv2)
```


Working with factors
=======================================
Controlling for WHO regions provides a non-trivial example of the use of factors in regression.

When you create a data frame in R, non-numeric variables are automatically turned into **factors**.  Factors are both a strength of R and a frequent source of annoyance and confusion. See [traps and pitfalls with factors](http://scs.math.yorku.ca/index.php/R/Traps_and_pitfalls#Factors).

Let's create a small data frame to illustrate how factors work:
```{r}
set.seed(147)
sdf <- data.frame( x = c(1:7,6:10), 
              g = rep(c('a','b','c'),c(2,5,5)))
sdf
sdf$y <- with(sdf, x + c(1,0,2)[g]+.5* rnorm(12))
sdf
sdf$g
unclass(sdf$g)    # the innards of g  
# g is actually a numeric variable consisting of indices into a
# vector of 'levels'.
sfit <- lm( y ~ x + g, sdf, na.action = na.exclude)
summary(sfit)
```
- Note that there is no term called 'ga' although there are 3 levels: 'a', 'b' and 'c'
- The 'missing' level, 'a', is called the **reference level** 
- Each term shows a comparison with the reference level
To work out what the coefficients for 'gb' and 'gc' mean, you need to look at the X matrix:
```{r}
model.matrix(sfit, na.action = na.exclude)
model.matrix(~ x + g, sdf)
model.matrix(~ g, sdf)
```
If you work through the model:
$$
E(y|x,g) = \beta_0 + \beta_x x + \beta_{gb} gb + \beta_{gc} gc
$$
where $gb = 1$ if $g=b$ and 0 otherwise, and $gc = 1$ if $g=c$ and 0 otherwise, you see that 

1. $\beta_{gb}$ is the difference between the expected level for group 'b' versus the reference group 'a' keeping x constant and  
2. $\beta_{gc}$ is the same comparison for group 'c' compared with the reference group 'a'.
```{r}
# Plotting fits within groups and panels using latticeExtra:
# See more elegant but perhaps less flexible approaches in 
# the 'car' and in the 'effects' package by John Fox 
pred <- expand.grid( x = 0:13, g = levels( sdf$g))  
# the values over which we want to see predicted lines
# every combination of x and g
pred  <- merge( sdf, pred, all = T) # merge with data
pred$y1 <- predict(sfit, newdata = pred) # the predicted value
pred <- pred [ order(pred$x),] # order so lines won't be interrupted
require(latticeExtra)
require(spida)
td(cex=2, lty=1:3,        # this gives you optional control over line styles, colour, etc.
pch = 16:18, 
col = c('red','blue','magenta'), 
lwd =2)  # from spida
xyplot( y ~ x , pred, groups = g , auto.key = list(columns = 3,lines = T), 
   y1 = pred$y1, subscripts = T,
   sub = "compare adjusted and unadjusted differences between groups") +
glayer( panel.lines( x, y1[subscripts],...,type = 'l')) +
layer( panel.abline( v = c(0,3), col = 'grey')) +
glayer( panel.abline( h = mean(y,na.rm=T),...))
## an alternative ... but
xyplot( y ~ x, sdf, groups = g, type = c('p','r'))
```
### Exercise: 
1. Draw by hand the values of estimated coefficients in the plot.
2. How would you estimate the differences between horizontal lines?
Does 'g' matter?
------------------
```{r}
summary(sfit)  # p-values not significant
## But
wald(sfit, "g")  # simultaneous test that both are 0: different answer!
```
### Using the GLH (General Linear Hypothesis)
```{r}
Lmu.6 <-list( "at x = 6" = rbind( 
'g = a' = c(1,6,0,0),
'g = b' = c(1,6,1,0),
'g = c' = c(1,6,0,1)))
Lmu.6
wald(sfit, Lmu.6)
Ldiff <- rbind( 
'b - a' = c(0,0,1,0),
'c - a' = c(0,0,0,1),
'c - b' = c(0,0,-1,1))
Ldiff
wald(sfit,Ldiff)
wald(sfit, 'g')
```
This illustrated the crucial point that separate tests of 
$$
H_0: \beta_1 = 0
$$
and
$$
H_0: \beta_2 = 0
$$
can yield very different 'conclusions' that a test of the joint hypothesis:
$$
H_0: \beta_1 = \beta_2 = 0
$$
Later, we will see how the relationship between confidence ellipses(oids) and tests makes this clear.
### Reparametrization to answer different questions
```{r}
sdf$g2 <- relevel(sdf$g, 'b')  # makes 'b' the reference level
fitr <- lm( y ~ x + g2, sdf)
summary(fitr)
fitr2 <- lm( y ~ x + g2 -1 , sdf)   # dropping the intercept
summary(fitr2)
fitr3 <- lm( y ~ I(x-6) + g2 -1 , sdf)   # recentering x
summary(fitr3)   # compare with earlier
wald(sfit, Lmu.6)
```
### Equivalent models
What makes the last three models equivalent?
```{r}
summary(lm( model.matrix(fitr) ~ model.matrix(sfit)-1)) # note the Resid. SE
```
Each model matrix spans exactly the same linear space. Thus their columns are just different bases for the same space and the $\beta$s for one model are just a linear transformation of the $\betas$s for the other model.
### Exercises:
```{r}
## What do the coefficients estimate in each of the following?:
summary( fit1 <- lm( y ~ g - 1, sdf))  # an example where '=' would not work
model.matrix(fit1) 
summary( fit2 <- lm( y ~ x + g - 1, sdf))  # an example where '=' would not work
model.matrix(fit2)
```
## Factor with interaction:
```{r}
sfit2 <- lm( y ~ x * g, sdf)
summary(sfit2)
```
From which you might conclude that 'nothing is significant'!

This illustrates that it is often wrong *wrong* **wrong** to form conclusions on the basis of scanning p-values in regression output. 

Plotting the fitted model:
```{r}
pred$y2 <- predict( sfit2, newdata = pred)
xyplot( y ~ x, pred, groups = g, type = c('p','r'))
# or
xyplot( y ~ x, pred, groups = g, 
   subscripts = T, y2 = pred$y2, y1 = pred$y1) +
glayer( panel.xyplot( x, y2[subscripts], ..., type = 'l'))
# or
xyplot( y ~ x, pred, groups = g, 
   ylim = c(0,13), auto.key = T,
   subscripts = T, y2 = pred$y2, y1 = pred$y1) +
glayer( panel.xyplot( x, y2[subscripts], ..., type = 'l')) +
layer( panel.abline( v = c(0,6)))

```
The model is:
$$
\begin{aligned}
E(y|x,g) = & \beta_0 + \beta_x x + \beta_{gb} gb + \beta_{gc} gc\\
& + \beta_{x:gb} x \times gb + \beta_{x:gc} x \times gc \\
\end{aligned}
$$
Taking partial derivatives, we see that $\beta_{gb}$ is the difference between between group 'b' minus group 'a' when $x = 0$.  There might not be strong evidence of differences between groups outside the range of the data.

What would happen if we were to explore the difference between group 'b' and group 'c' when $x = 6$:
```{r}
L.bc.6 <- rbind( 'c - b|x=6'=
              c(0,0,-1,1,-6,6))
wald( sfit2, L.bc.6)
```
We could also do this by reparametrizing:
```{r}
sfit2.x6 <- lm( y ~ I(x-6) * relevel(g,'b'), sdf)
summary(sfit2.x6)
```
Here are some other ways of exploring the model:
```{r}
wald(sfit2, ":")
wald(sfit2, "g")
wald(sfit2, "x")
```
Using type 2 Anova gives you tests for 'g' and 'x' that assume that higher-order interactions involving 'g' and 'x' are all 0.  Note that the error term used is the error term for the full model including interactions.  This can lead to inconsistencies with tests based on a model in which interactions has been dropped. 
```{r}
Anova(sfit2)   # type 2 anova
Anova(sfit)    # here the gain in degrees of freedom outweighs the increase in SSE
```
## Using Lfx with factors
The 'M' function associated with 'Lfx' can generate code to test for differences between factor levels
```{r}
Lfx(sfit2)
```
The idea is to use the 'Lfx' expression to difference and then to apply to a data frame. 
```{r}
dpred <- expand.grid( x= 0:12, g = levels(sdf$g) , g0 = levels(sdf$g))
dim(dpred)
some(dpred)
# we don't need to compare g with g0 with the same levels 
# and we only need comparisons in one direction (perhaps not)
dpred <- subset( dpred, g0 < g)
dim(dpred)
some(dpred)
Lfx(sfit2)
# 'difference' g - g0, just like differentiating wrt to g
# except that 'M' function generates differences
Lmat <- Lfx( sfit2, 
        list( 0,
              0 * x,
              1 * M(g,g0),
              1 * x * M(g,g0) 
        ), dpred)
Lmat
wald(sfit2, Lmat)
ww <- as.data.frame(wald(sfit2, Lmat))
head(ww)
ww$gap <- with(ww, paste( g, '-', g0))
xyplot( coef ~ x | gap, ww)
td( col = c('black', 'blue','blue'), lty = 1, lwd = 2)
xyplot( coef +I(coef+2*se) + I(coef-2*se) ~ x | gap, ww, type = 'l') +
layer_( panel.abline( h = 0))
# or
xyplot( coef +I(coef+2*se) + I(coef-2*se) ~ x | gap, ww, type = 'l',
   ylab = "Estimated difference plus or minus 2 SEs",
   xlim = c(0,12))+
layer_( panel.abline( h = 0))
# or
xyplot( coef +I(coef+2*se) + I(coef-2*se) ~ x | gap, ww, type = 'l',
   ylab = list("Estimated difference plus or minus 2 SEs", cex = 1.3, font = 2),
   xlim = c(0,12))+
layer_( panel.abline( h = 0))
```
Note that if the significant gap between 'c' and 'b' around $x=6$ is a question inspired by the data and not a 'prior' hypothesis, then some adjustment should be made for **multiplicity**. 
### Exercises

1. Explore how to modify the appearance of the 'strips', i.e. where it says 'c - b'
2. Plot approximate '95% confidence bands' for each group with +/- 2 SEs
3. Plot approximate '95% prediction bands' for each group.
```{r}
# reset the random seed
set.seed(NULL) 
```
Using WHO regions as predictors of Life Expectancy
--------------------------------------------------
```{r}
fitr <- lm( LifeExp ~ (smoke + I(smoke^2)) * region + hiv + special, dd)
summary(fitr)
wald(fitr,":")
wald(fitr,"2):")

## Using data values for prediction instead of creating a separate prediction data frame
#    This can work with curvilinear models if the data is sufficiently dense

dd$yq <- predict( fitr, dd)
xyplot( LifeExp ~ smoke | region, dd)
# reorder for nice lines:
dd <- dd[order(dd$region,dd$smoke),]
xyplot( LifeExp ~ smoke | region, dd, subscripts = T, yq = dd$yq)  + 
layer( panel.xyplot( x, yq[subscripts],..., type = 'b', col = 'red'))
# presents a problem because of missing hiv values

# try again keeping non-missing data together to avoid interrupting lines
dd <- dd[order(is.na(dd$yq),dd$region,dd$smoke),]
xyplot( LifeExp ~ smoke | region, dd, subscripts = T, yq = dd$yq)  + 
layer( panel.xyplot( x, yq[subscripts],..., type = 'b', col = 'red'))
```
## Including Health Expenditure 
```{r}
fitrhe <- lm( LifeExp ~ (smoke + I(smoke^2)) * region * log(HE) + hiv + special, dd)
summary(fitrhe)
length(coef(fitrhe))
# should have > 380 observations using Harrell's rules of thumb for valid regression
wald(fitrhe, ":")   
wald(fitrhe, "2")   
wald(fitrhe, "2|:.*:") # quadratic terms and 3 and higher way interaction
fitr2 <- lm( LifeExp ~ (smoke + log(HE) + region)^2 + hiv + special, dd)
summary(fitr2)
wald(fitr2, ':')
wald(fitr2, 'HE):|:log')
fitr3 <- lm( LifeExp ~ region* smoke + log(HE)+ hiv + special, dd)
summary(fitr3)
wald(fitr3, ":")
wald(fitr3, 'region')
wald(fitr3, 'HE')
Anova(fitr3)
anova(fitr3)
wald(fitr3, 'smoke')
library(p3d)
Plot3d(LifeExp ~  smoke + HE | region, dd)
Fit3d( fitr3, other.vars=list(hiv=0,special=0))
#Id3d(par=2, labels = dd$country)
par3d(windowRect=c(10,10,700,700))
rgl.snapshot('regions.png')
```
![3d](regions.png)
### Exercise
Explore this data further producing informative graphs.
                                 