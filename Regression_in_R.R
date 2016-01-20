#'
#'
#' # Review of Regression in R version 4
#' #### MATH 6627
#'  
#' #### Week 2
#' 
#' #### First draft: September 17, 2014
#' 
#' #### Revised: September 24, 2014
#' 
#' 
#+ include=FALSE,eval=FALSE

# To run this file with R Markdown by clicking on File|Compile Notebook in RStudio
# you need to install some packages and download a data set:
if(FALSE){
  download.file("http://blackwell.math.yorku.ca/practicum/Smoking3.csv","Smoking3.csv")
  install.packages(c('car','Hmisc','latticeExtra','rgl','magrittr','vcd','RColorBrewer'))
  download.file("http://blackwell.math.yorku.ca/R/spida.zip","spida.zip")
  install.packages("spida.zip", repos = NULL)
  download.file("http://blackwell.math.yorku.ca/R/p3d.zip","p3d.zip")
  install.packages("p3d.zip", repos = NULL)
}
#'
#' To really understand regression, you need to be able to approach a problem from many
#' different angles. I can think of at least 8 representations that complement each other.
#' To master regression you need to know how to go from one representation to another and 
#' you need to know how to work within each representation to solve problems.
#'
#' #### 1. Statistical: the matrix formulation of a model
#' $$
#'   Y = X \beta + \epsilon,\: \epsilon \sim N(0,\sigma^2)
#' $$
#'   and all the theory that follows, e.g.
#' $$
#'   Var(\hat{\beta}) = \sigma^2 \left( X'X \right)^{-1}
#' $$
#' 
#' #### 2. Mathematical: the formula for the model
#' $$
#' y_i = \beta_0 + \beta_1 x_i + \beta_2 x_i^2 + \epsilon_i \\
#' \frac{\partial E(y)}{\partial x} = \beta_1 + 2\,\beta_2 x
#' $$
#' 
#' #### 3. Computing: commands and algorithms that fit the model
library(car)
fit <- lm(income ~  education * type, data = Prestige)
summary(fit)
#' 
#' #### 4. Graphical: data space
library(car)
library(lattice)
xyplot(income ~ education | type, Prestige, type = c('p','r','smooth'))
#'
#' #### 5. Graphical: beta space
#' 
library(spida)
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
#' **Figure:** Confidence ellipse for two parameters jointly. The blue ellipse has 95% coverage in 2 dimensions and its perpendicular shadows onto the vertical and horizontal axes form Scheffe 95% confidence intervals for testing in a space of dimension 2. The similar shadows of the red ellipse provide ordinary 95% confidence intervals.
#'   
#' #### 6. Graphical (different definition!): Graph (or 'path diagram') of variables
#' ![Figure 2-2](graph.PNG)  
#' 
#' #### 7. Geometric: Hilbert space representation of variables or 'variable space'
#' 
#' ![Figure 2-2](HilbertSpace.PNG)
#' 
#' #### 8. Most important: Real world interpretation
#' 
#' The most important representation is the interpretation of the model in the real world. Real world factors, such as the design, the nature of random assignment, the nature of random selection have a profound impact on the interpretation of the model and on the strategy for model development, selection and interpretation.
#' 
#' This is where you determine the nature of the data: observational or experimental, and the nature of the questions: predictive or causal. 
#' 

#' ## Review of the matrix formulation and the general linear hypothesis (GLH)
#' 
#' $$ Y = X \beta + \varepsilon $$
#' where
#' 
#' 1. $Y$ is a vector of length $n$ representing $n$ observations on a 'response' or 'dependent' variable,
#' 2. $X$ is a $n \times p$ matrix representing $n$ observations on each of $p$ 'predictor' or 'independent' variables.  The first column frequently consists of 1's.
#' 3. $\beta$ is a vector of $p$ parameters whose values are unknown and some aspect of which we wish to estimate. If the first column of $X$ consists of 1's it is customary to number the elements of $\beta$ starting from 0: $\beta = (\beta_0, \beta_1, ... , \beta_{p-1})'$.
#' 4. $\varepsilon$ is a vector of length $n$ representing 'errors' or 'residuals' that are not directly observed.
#' 
#' If $X$ is of full column rank (i.e. $rank(X) = p$) and if we assume that $\varepsilon \sim N_n( 0, \sigma^2 I)$ where $I$ is the $n \times n$ identity matrix, and if $rank(X) = p$, then the **UMVUE** (Uniformly minimum variance unbiased estimator) of $\beta$ is
#' $$ \hat{\beta} = (X'X)^{-1}X'Y $$
#' with $E(\hat{\beta}) = \beta$ and $Var(\hat{\beta}) = \sigma^2 (X'X)^{-1}$
#' 
#' ### Linear hypotheses
#' 
#' We can estimate or test hypotheses concerning one or more linear combinations 
#' of the $\beta$s
#' by forming a $h \times p$ hypothesis matrix $L$ and estimating the function of parameters:
#' $$
#' \eta = L \beta
#' $$
#' 
#' #### Example:
#' For a model with three parameters
#' $\beta = (\beta_0, \beta_1, \beta_2)'$ we can simultaneously estimate the sum and difference of $\beta_1$ and $\beta_2$ as follows. 
#' 
#' Letting
#' $$
#' L = \left[ \begin{array}{r} 0 & 1 & 1 \\ 0 & 1 & -1 \end{array} \right] 
#' $$ 
#' we get
#' $$
#' \begin{align}
#' \eta  = \left[\begin{array}{c} \eta_1 \\ \eta_ 2 \end{array} \right] 
#' &= L \beta \\
#' &= \left[ \begin{array}{r} 0 & 1 & 1 \\ 0 & 1 & -1 \end{array}\right] 
#'     \left[ \begin{array}{r} \beta_0 \\ \beta_1 \\ \beta_2 \end{array} \right] \\
#'  & = \left[ \begin{array}{c} \beta_1+\beta_2  \\ \beta_1 - \beta_2\end{array} \right]
#' \end{align}
#' $$
#' 
#' #### Estimation and tests
#' Letting
#' $$
#' \hat{\eta} = L \hat{\beta}
#' $$
#' we have 
#' $$
#' E(\hat{\eta}) = E(L \hat{\beta})= L E(\hat{\beta})= L \beta
#' $$
#' and
#' $$
#' Var(\hat{\eta}) = \sigma^2 L (X'X)^{-1} L'
#' $$
#' If $L$ is of full row rank $h$ and $X$ is of full column rank we can test the 
#' hypothesis 
#' $$
#' H_0: \eta = L \beta = 0
#' $$
#' against the alternative that $\eta \neq 0$ (i.e. that for at least one $i$, $\eta_i \neq 0$ by using the null distribution:
#' $$
#' \begin{align}
#' \hat{\eta}' \left( \widehat{Var} (\hat{\eta}) \right)^{-1} \hat{\eta} 
#' &=  \frac{\hat{\beta}'L' \left( L (X'X)^{-1} L' \right)^{-1} L \hat{\beta}}{s_e^2}\\
#' &\sim h \times F_{h,\nu}
#' \end{align}
#' $$
#' where $s_e$  is the 'residual standard error':
#' $$
#' s_e^2 = \frac{|| Y - X \hat{\beta} ||^2}{\nu}
#' $$
#' with $\nu = n - p$ the degrees of freedom for the estimate $s_e^2$ of $\sigma^2$
#' and 
#' $F_{h,\nu}$ is the $F$ distribution with $h$ and $\nu$ degrees of freedom.
#' 
#' #### Notes
#' 
#' 1. If $L$ is not of full row rank then an equivalent hypothesis can be formed by replacing
#' $L$ with a matrix whose rows form a basis of the row space of $L$. 
#' 2. Two $L$ matrices with the same row space test equivalent simultaneous hypotheses (**Could you prove this?**). For example, the hypothesis above is equivalent to the hypothesis that $\beta_1 = \beta_2 = 0$. **Why?**
#' 
#' ## Regression Using R
#' 
#' The following is an example of data analysis using regression in R. But it's very unrealistic.
#' 
#' Real data analysis does not start with a neat rectangular data set.
#'  
#' Hadley Wickham: "Data analysis is the process by which data becomes understanding, knowledge and insight"
#'  
#' The process involves much more than running a regression:
#' 
#' - subject matter understanding
#' - getting data
#' - tidying the data
#' - formulating research questions
#' - transforming data to variables for analysis
#' - exploratory visualization
#' - deciding on a starting model
#'   - not too big, not too small
#'   - includes key variables based on subject matter and questions
#' - modeling fitting
#' - model diagnostics
#' - refining the model: dropping some terms and adding others
#' - formulating parameter functions for estimation and testing
#' - interpreting results
#' - GO BACK and iterate a varying number of previous steps in various orders
#'
#' ## Jumping into R: extended example
#' 
#' We will use the "Smoking3" data set to illustrate both the practice and the principles of basic linear regression.
#'
#' To use this script you need to install two 'non-CRAN' packages
#' 
#' spida from scs.math.yorku.ca/index.php/spida
#' 
#' and
#' 
#' p3d from scs.math.yorku.ca/index.php/p3d
#' 
#' and you need to put the data set 'Smoking.csv' in the 
#' active directory for R
#' 
#'  For installation of spida or p3d on a Mac or for Linux, 
#'  see http://scs.math.yorku.ca/index.php/spida

#' Install some packages we will use frequently
#' 
#' In Windows:
#' Load some packages
library(p3d)
library(spida)
library(latticeExtra)
library(magrittr)
options(width=100) # 100 for 6in and 67 for 4in
# options(width=67) # 100 for 6in and 67 for 4in


#' make sure Smoking3.csv is in the same directory as the
#' active directory for R, otherwise move Smoking.csv 
#' or change the active directory


dd <- read.csv("Smoking3.csv")
head(dd)

#' #### Quick look:
summary(dd)
#' #### Quantile plots or barcharts
xqplot(dd)
tablemissing(dd)

#' #### Rename/define some variables

dd$cig <- dd$consumption.cigPC
dd$Cigarettes <- dd$consumption.cigPC
dd$pop <- dd$Pop.Total
dd$Life <- dd$lifeexp.Birth
dd$area <- tr(dd$region, 
              c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
              c("Africa","South Asia","Other")[c(1,3,3,3,2,3)])
tab(dd, ~ region + area)/3
dd$log.total <- log(dd$total)
dd$Health <- dd$total  # Note that total = govt + private health expenditures
dd$hiv <- dd$hiv_prev15_49

#' #### Use data for combined sexes

tab(dd, ~ sex)
ds <- subset( dd, sex == "BTSX")  # keep only data rows that combine sexes 
rownames(ds) <- ds$country
head(ds)

#' ### Interactive 3D


  Init3d(cex=1)
  
  Plot3d( Life ~ Cigarettes + Health |region,ds) 
  fg()
  spinto()
  Axes3d()
#  Id3d()
  fit <- lm( Life ~ Cigarettes, ds)
  summary(fit)
  wald(fit)
  Fit3d(fit, lwd = 3)
  fitsq <- lm( Life ~ Cigarettes+I(Cigarettes^2), ds)
  Fit3d(fitsq, lwd = 3, col = 'red')
  spin(0,0,0)
#  Id3d(pad=1)
  Id3d("Canada")
  Id3d("United States")
  par3d(windowRect=c(10,10,700,700))
  rgl.snapshot('quadsmoke.png')  
  Pop3d(2)

#' ![Fig-quadsmoke](quadsmoke.png)
#'
#' #### Controlling for Health
  
  spin(-90,0,0)
  
  fitlin <- lm( Life ~ Cigarettes + Health, ds)
  summary(fitlin)
  fith <- lm( Life ~ Cigarettes + Health + log( Health),ds)
  Fit3d(fitlin, col = 'pink')
  Fit3d(fith, col = 'red')
  Pop3d(2)
#'
#' ### A more interesting model?
#'
#' 1. Health Expenditures
#' 2. Proportion provided through government
#'   

ds$propGovt <- with(ds, govt/total)   # proportion of health exp. from Govt
Plot3d( Life ~ Health + propGovt |area, ds)
fg()
Axes3d()
spin(-10,15,0)
par3d(windowRect=c(10,10,700,700))
rgl.snapshot('health-pgovt.png')  
#'
#' ![Fig-health-pgovt](health-pgovt.png)
#'  
#' Try something that looks sensible:
#' 
#' The relationship between Life Expectance and 
#' Health Expentitures per capita and the proportion 
#' of health expenditures funelled through the government
#' 

ds$propGovt <- with(ds, govt/total)   # proportion of health exp. from Govt

fit <- lm( Life ~ (Health + log(Health) + propGovt)  * area , ds,
           na.action = na.exclude)   
summary(fit)
Plot3d( Life ~ Health + propGovt | area, ds)
Fit3d( fit)
spin(13,15,10)
par3d(windowRect=c(10,10,700,700))
rgl.snapshot('health-pgovt-fit.png')  
#'
#' ![hpfit](health-pgovt-fit.png)
#' 
#' **Question:** Is this model too big for the data?
#'
#' **Should we drop Health expenditures?**
#' 
#' None of the coefficients relating to Health are significant. 
#' 
#' Can we conclude that "Health" does not add to the predictive power of this
#' model?
#' 
#' #### Type II SS
#' 
Anova(fit)  # Type II: slightly less prone to massive misinterpretation

# #### Simultaneous tests of groups of coefficients:

wald(fit, "Health")  # note that overall evidence VERY strong although
# individual p-values not even significant

#' ### Note
#' 
#' I cannot sufficiently stress the importance of the principle this illustrates.
#' 
#' 100% of beginning graduates students in statistics programs will fall into the trap of 
#' mis-interpreting p-values in regression output. It's as bad a professional
#' error as a doctor amputating the wrong leg.
#' 
#' ### Two valid tests:
#' 
#' #### 1) Likelihood ratio test

#' Null model
fit0 <- lm( Life ~ propGovt  * area , ds,
            na.action = na.exclude)   
#' OR
fit0 <- update( fit, . ~ propGovt * area)
summary(fit0)
#' LRT:
anova( fit, fit0)

#'
#' #### 2) Wald test 
#' (doesn't require fitting a new model -- works for linear parameters, not necessarily so good for non-linear parameters)
#' 
#' Test simultaneous hypothesis that ALL coefficients are = to 0 simultaneously
#' -- which is equivalent to dropping Health entirely:
#' 
wald(fit, "Health")  # Note that F-values identical -- works in the case of OLS regression

#'
#' #### Explore interactions
#'  
#' There many approaches to simplifying a model. The most widespread is
#' to trim down non-significant interactions.
#'  
#' If you do this it is vital to never drop a group of terms unless you either:
#'  
#' 1. do it one term at a time making sure that you observe the principle of marginality as you go along, or
#' 2. you only drop groups of terms when you have tested them **as a group**.
#'  
#' We could refit and use LRTs or we can use Wald tests:
#'  
wald(fit, ":")  # Use REGULAR EXPRESSION matching to test whether there's any evidence of interaction
#' Explore how regular expressions work:
?regex
#'
#' Testing all interactions that involve 'Health'
wald(fit, "Health.*:")
#' Testing all interactions that involve 'Govt' (always to check to make sure that you captured exactly the right terms)
wald(fit, "Govt:")
#'
#' ### Some comments on reading a model
#' 
#' #### Table of coefficients and p-values:
#' 
summary(fit)
#' **Problems and limitations:**
#' 
#' 1. Except for very simple models this is generally misleading and not meaningful  
#' 2. Very few know how to interpret these correctly, even statisticians
#' 3. The p-value answers how much evidence is there that this term adds to the model when all other terms are already in the model.
#' 4. Only one degree of freedom per coefficient: never asks whether groups of terms are significant which is essential with categorical factors with 3 or more levels.
#' 5. Often it's meaningless to change one term keeping others constant
#' 6. Terms that are marginal to higher order interactions have a specific conditional interpretation that is generally just a very small and arbitrary part of the picture.
#' 7. The interpretation of tests does not respect the principle of marginality.
#' 
#' #### Type I (sequential) Tests and Sums of Squares
#' 
anova(fit)
#' The results depend on the order of the terms.
anova(update(fit, . ~ area * (propGovt + log(Health) + Health)  ))
#' **Notes:**
#' 
#' 1. Each p-values asks whether there's evidence that each terms adds **to the previous terms** listed in the model. 
#' 2. Interpretation of tests respects marginality since interactions are listed after their included main effects and sub-interactions.
#' 3. Factors with multiple degrees of freedom are tested jointly.
#'       
#' #### Type II Tests and Sums of Squares 
#'
Anova(fit) 
#' **Notes:**
#' 1. Terms within a 'level' are each added last respecting marginality. e.g. each main effect is added last among main effects but not including higher-order interactions that contain the effect.
#' 2. Main effects are interpretable as test of significance under assumption that there are no interactions.
#' 
#' #### Type III Tests and Sums of Squares
#' 
#' 1. Popularized by SAS and SPSS
#' 2. No universal definition so can be misleading
#' 3. With interactions, main effects are averages over levels of interacting variables which can be misleading if groups sizes are unequal.
#' 4. Loved by many researchers, deprecated by most statisticians ... like pie charts.
#' 
Anova(fit, type = 3)   # Type III Anova: Very popular but ....!
#'
#' #### Alternative -- or supplementary -- approaches
#' 
#' Answer specific questions. 
#' 
#' e.g. None of the above are equivalent to Wald test for 'OVERALL SIGNIFICANCE':
#'
wald(fit, "Health")
wald(fit, "area")
wald(fit, "propGovt")

#'
#' ## Regression diagnostics -- quick
#'

#' #### Traditional
plot(fit)
#' produces 4 plots
#' 
#' 1) resid ~ fit
#' 2) normal quantiles of residuals  -- Why would this matter??? GEQ. What can it mean if observed residuals are not normal?  Clue: Why would you expect them to be normal anyways?
#' 3) scale-location for heteroscedasticity
#' 4) Residual vs leverage plot -- will see deeper meaning of this plot
#' points with high Cook's distance might have strong influence on 
#' fitted vals 
#' 
#' Note: added-variable plots = partial residual leverage plots
#' 
avPlots(fit)   # look at these as if they were simple regression plots
avPlots(fit, id.n = 3)
avPlot(fit, 'propGovt', id.n = 10)
# avPlot(fit, 'propGovt', id.method = "identify")

#'
#' ### Visualize fit for diagnostics
#'

#'
#' In 3D

  Plot3d( Life ~ Health + propGovt | area, ds)
  Fit3d( fit , resid = T)
#  Id3d()  # outliers?

#'
#' 2D
#'  
summary(ds)
#'
#' Create a prediction data frame with values for which you want to predict
#' model with observed values for Health and area but 
#' controlling for propGovt
#'  

#' #### 3 ways:
#' 1. easiest: use data BUT need to control for propGovt
#' 2. Generate cartesian product of values of predictors: but hard to generate conditional ranges 
#' 3. Create prediction data set with original data augmented by extra points
#' 

#' #### 1. add predicted values to data frame

ds <- sortdf( ds, ~ Health)
pred1 <- rbind(ds,NA,ds,NA, ds,NA, ds,NA, ds, NA)  # to set five values for predicted propGovt
pred1$propGovt <- rep(seq(.1,.9,by=.2), each = nrow(ds)+1)

pred1$Life.fit <- predict(fit, newdata = pred1)

#'
#' In 'panels':
#' 
xyplot(Life ~ Health | area, ds)
gd() # ggplot2 look-alike

xyplot(Life ~ Health | area, ds)
xyplot(Life ~ Health | area, ds, layout = c(1,3))
xyplot(Life ~ Health | area, ds, layout = c(1,3)) +
  xyplot( Life.fit ~ Health | area, pred1, groups = propGovt,
          type = 'l')

gd(lwd = 2)
(p <- xyplot( Life.fit ~ Health | area, pred1, groups = propGovt,
              type = 'l', auto.key = list(space='right',lines = T, points =F,
                                          title = 'Prop. Govt', cex.title = 1)))
#' 
#' Some colour palettes you can choose from. 
display.brewer.all()
#' Note that the first group consists of 'progressive' palettes, the second group of categorical palettes (one is 'paired') and the third group of 'bipolar' palettes. Note that yellow often doesn't work for lines that blend into the background so you might have to avoid palettes that include yellow for some purposes.
gd(lwd = 2, col = brewer.pal(5,"Spectral"))
p  # replots with new parameters
gd(lwd = 2, col = brewer.pal(5,"Set1"))
p
gd(lty = 1)
p
p + xyplot(Life ~ Health | area, ds)

#' Probably better with log(Health)

(p <- xyplot( Life.fit ~ log(Health) | area, pred1, groups = propGovt,
              type = 'l', auto.key = list(space='right',lines = T, points =F,
                                          title = 'Prop. Govt', cex.title = 1)))

p + xyplot(Life ~ log(Health) | area, ds)

#' BUT NEVER NEVER USE axes that are not meaningful
#' 
#' Your work will be written off as incomprehensible!
#' 
#' Also: use meaningful labels

update(p, xlab = "Health expenditures per capita in $US (log scale)",
       ylab = "Life expectancy (both sexes)",
       layout = c(1,3),
       scales = list( x = list(
         at = log(c(30,100,300,1000,3000,8000)),
         labels = c(30,100,300,1000,3000,8000)))) +
  xyplot(Life ~ log(Health) | area, ds)


#'
#'  Dropping some observations: BEWARE
#'  
#'  Two ways:
#'  
#'  1) Drop from data set and refit
#'  2) Add parameters for dummy variables for observations to drop and refit   
#'  
#' Suppose we want to drop "Equatorial Guinea"
#'  
#' CAUTION: this needs good reflection BUT we often should approach this like
#' a sensitivity analysis: e.g. "would it make a big difference if I dropped
#' this point?"  
#'  

ds$EqG <- 1*(ds$country == "Equatorial Guinea")

fit2 <- lm( Life ~ (Health + log(Health) + propGovt)  * area + EqG  , ds,
            na.action = na.exclude)   

#' OR

fit2 <- update(fit, . ~ . + EqG)
summary(fit2)

Fit3d(fit2, other.vars = list( EqG = 0))
spin(-2,18,10)
par3d(windowRect=c(10,10,700,700))
rgl.snapshot('dropEqG.png')  
#' ![dropEqG](dropEqG.png)
#' 
#' **Figure X:** Note the change in the fitted surface for Africa when Equatorial Guinea is dropped from the model.
#' 
#' ## Asking questions:
#'  
#' ### Can we simplify the model?
#' Often this process focuses on the initial regression parameter and asks which
#' ones 'can we drop'? Often starting with highest order interactions and working in.
#' 
#' **BEWARE THE PRINCIPLE OF MARGINALITY**
#' 
#' **In general (i.e. 99.9% of the time)** DO NOT eliminate a term without also
#' eliminating all higher-order terms that are *marginal* to it.  e.g. 'Health' is marginal
#' to 'Health:area', and 'Health:area' would be marginal to 'Health:propGovt:area'. Otherwise the model loses
#' invariance with respect to changes of origins of interacting variables.
#' 
#' Here's a model that seems to tell a different story but it's perfectly equivalent
#' 

fit2.eq <- lm( Life ~ area/(Health + log(Health) + propGovt)  + EqG -1 , 
               ds,
               na.action = na.exclude)   
anova(fit2, fit2.eq)
AIC( fit2, fit2.eq)
summary(fit2)
summary(fit2.eq)  

Anova(fit2)
Anova(fit2.eq)

wald(fit2, "th:|th):")
#'
#' **Question:** Under what conditions would two seemingly different models produce exactly the same fit (i.e. predicted values of Y)?
#'  
#' ### Asking specific questions
#' 
#' **What question does each coefficient answer and how can
#' we get answers to the questions we want?**
#' 
#'  **PRINCIPLE** 
#'  Except with very simple models, raw regression output generally answers few meaningful questions AND
#'  most important questions are rarely answered by raw regression output 
#'  
#' **Interpreting $\beta$s:** Each term involving 'area' is a comparison with the **REFERENCE LEVEL**
#' when **ALL VARIABLES IN HIGHER ORDER INTERACTING TERMS are set to 0**. 
#' 
#' Each term involving 'area' is a comparison with the **REFERENCE LEVEL** (Africa because it's the level that isn't showing)
#' when **ALL VARIABLES IN HIGHER ORDER INTERACTING TERMS are set to 0**. 
#' 
#' The model is:
#' 
#' $$
#' \begin{aligned}
#' Y &= \beta_0 + \beta_1 Health + \beta_2 ln(Health) + \beta_3 propGovt\\ 
#' & + \beta_4 area_{Other} + \beta_5 area_{South Asia}\\
#' & + \beta_6 EqG \\
#' & + \beta_7 Health \times area_{Other} + \beta_8 Health \times area_{South Asia}\\
#' & + \beta_9 ln(Health)\times area_{Other} + \beta_{10} ln(Health)\times area_{South Asia} \\
#' & + \beta_{11} propGovt  \times area_{Other} + \beta_{12} propGovt \times area_{South Asia} \\
#' & + \varepsilon
#' \end{aligned}
#' $$
#' 
#' where $\varepsilon \sim N(0,\sigma^2)$ independently of predictors.
#' 
#' **Note:**  Here we encounter the vital difference between assumptions that can be checked and assumptions that can't be checked.  We are assuming that 1) errors are normal, 2) they have the same variance for each observation, and 3) they are independent of predictors.  The first two can be checked with diagnostics, the third, in general, cannot. In econometrics it's recognized as a key assumption that is related to the 'exogeneity' of the predictors and the 'causal interpretation' of the model. We will revisit this in *Methods Matter*.
#' 
#' ## Understanding coefficients
#' 
#' Q: What does $\beta_1$ mean?
#' 
#' A: It's the expected change in $Y$ when you change $Health$ by one unit keeping all other terms constant --- **BUT THAT'S IMPOSSIBLE**
#' 
#' We'll have more luck with $\beta_3$: It's the expected change in $Y$ when you change $progGovt$ by one unit keeping all other terms constant, i.e. when all variables that interact with $propGovt$ are equal to 0.
#' 
#' i.e. when $area_{Other} = area_{South Asia} = 0$
#' 
#' i.e. **in Africa**
#' 
#' So we have a clear interpretation: it's the expected change in Life Expectancy using our model to compare a hypothetical country where health expenditures are entirely supported by the government with a hypothetical country in which they are entirely private **in Africa**.
#' 
#' This is not obvious to a casual user of regression and most surely not to most clients and readers of academic journals.
#' 
#' ### How can we get answers to meaningful questions?
#' 
#' You're in luck. Calculus comes in handy. 
#' 
#' What's the 'effect' (a very misused word and I'm still looking for a better one) of increasing Health expenditures by 1 dollar?
#' 
#' $$
#' \begin{aligned}
#' \frac{\partial{E(Y)}}{\partial{Health}} &= 0 \times \beta_0 + \beta_1  + \beta_2 \frac{1}{Health} + 0 \beta_3 \\ 
#' & + 0 \times \beta_4  + 0 \times  \beta_5 \\
#' & + 0  \times \beta_6  \\
#' & + \beta_7  area_{Other} + \beta_8  area_{South Asia}\\
#' & + \beta_9 \frac{area_{Other}}{Health}  + \beta_{10} \frac{area_{South Asia}}{Health}  \\
#' & + 0 \times  \beta_{11}  + 0  \times \beta_{12} \\
#' &= L \beta
#' \end{aligned}
#' $$
#' where
#' $$
#' L= \left[ \begin{array}{c}
#' 0 & 1 & \frac{1}{Health} & 0 & 0 & 0 & 0 &
#'  area_{Other} & area_{South Asia}
#'  &  \frac{ area_{Other}}{Health} & \frac{area_{South Asia}}{Health}  &
#'  0 & 0  
#' \end{array} \right]
#' $$
#' and 
#' $$
#' \beta = \left[ \begin{array}{c}
#' \beta_0 \\  
#' \beta_1 \\  
#' \beta_2 \\  
#' \beta_3 \\  
#' \beta_4 \\  
#' \beta_5 \\  
#' \beta_6 \\  
#' \beta_7 \\  
#' \beta_8 \\  
#' \beta_9 \\  
#' \beta_{10} \\  
#' \beta_{11} \\  
#' \beta_{12} \\  
#' \end{array} \right]
#' $$
#' which we can estimate with 
#' $$
#' \hat{\eta} = L \hat{\beta}
#' $$
#' 
#' **NOTE:** This does not seem to depend on $propGovt$! Is this reasonable? What should we do about that?
#' 
#' **Exercise:** Explore what could be done with $propGovt$. 
#' 
#' $\hat{\beta}$ is obtained with:
coef(fit2)
#' To estimate the marginal effect of Health Expenditures in 'Other' when
#' Health expenditures = 100:
#'  
Lmat <- cbind( 0,1,1/100,0,0,0,0,   1, 0 , 1/100, 0,0,0)
Lmat
Lmat %*% coef(fit2)
#' We could write matrix expression to get the variance, F-test, p-values etc., but it's already been done with the 'wald' function in 'spida'. Other packages also have functions that do this, e.g. 'lht' in the 'car' package. 
wald(fit2, Lmat)
#' How could we mass produce this?
ex <- expression( cbind( 0,1,1/Health,0,0,0,0,   
                         area == "Other", area == "South Asia",
                         (area == "Other")/Health, (area == "South Asia")/Health,
                         0,0))
ex
with( list(Health=100, area = "South Asia"), eval(ex))

pred <- expand.grid( Health = seq(30,4000,10), 
                     area = levels(ds$area))
head(pred)
tail(pred)
dim(pred)
head(with(pred, eval(ex)))

ww <- wald(fit2, with(pred, eval(ex)))
str(ww)
head(as.data.frame(ww))
pred <- cbind( pred, as.data.frame(ww))
head(pred)
gd(lwd=2)
xyplot(coef ~ Health, pred, groups = area,type= 'l',
       auto.key=list(space = 'right'))

xyplot(coef ~ Health|area, pred, groups = area, type = 'l',
       auto.key = list(space='right'),
       subscripts = T, 
       lower = pred$coef - 2* pred$se,
       upper = pred$coef + 2* pred$se,
       layout = c(1,3)) +
  glayer( gpanel.band(...))

#' Make labels and axes interprettable for presentation:


xyplot(I(100*coef) ~ log(Health), pred, groups = area,type= 'l',
       auto.key=list(space='right', lines = T, points = F),
       ylab = "Estimated change in LE associated with a $100 increase in Health Exp.",
       xlab = "Health expenditures per capita in $US (log scale)",
       scales = list( x = list(
         at = log(c(30,100,300,1000,3000,8000)),
         labels = c(30,100,300,1000,3000,8000)))) 

#' Limit plot to ranges in each Area:

# create a small data set with ranges
dsr <- ds
# max within each area
dsr$max <- with(dsr, capply( Health, area, max, na.rm = T)) 
# min withing each area
dsr$min <- with(dsr, capply( Health, area, min, na.rm = T))
# summary data frame with variables that are 'area invariant'
dsr <- up(dsr, ~ area)  # keeps 'area' invariant variables only
dsr                                
# merge back into pred
predr <- merge(pred, dsr[,c('area','max','min')], all.x = T)
# keep values of Health that are within range
predr <- subset( predr, (Health <= max) & ( Health >= min))
head(predr)
#'
#' ### Plotting fitted values and bands
#' 
#' If you define arguments **fit**, **lower** and **upper** in 'xyplot', they will be available to 'gpanel.fit' to draw the fitted line and confidence or predictions bands.
#' 

xyplot(I(100*coef) ~ log(Health), predr, groups = area,type= 'l',
       auto.key=list(space='right', lines = T, points = F),
       ylab = "Estimated change in LE associated with a $100 increase in Health Exp.",
       lower = 100*(predr$coef - 2* predr$se),
       upper = 100*(predr$coef + 2* predr$se),
       sub= "Estimated change in LE with standard error bands",
       xlab = "Health expenditures per capita in $US (log scale)",
       scales = list( x = list(
         at = log(c(30,100,300,1000,3000,8000)),
         labels = c(30,100,300,1000,3000,8000)))) + 
    glayer(gpanel.fit(...))

xyplot(I(100*coef) ~ log(Health)|area, predr, groups = area,type= 'l',
       auto.key=list(space='right', lines = T, points = F),
       ylab = "Estimated change in LE associated with a $100 increase in Health Exp.",
       layout = c(1,3),
       lower = 100*(predr$coef - 2 * predr$se),
       upper = 100*(predr$coef + 2 * predr$se),
       sub= "Estimated change in LE with standard error bands",
       xlab = "Health expenditures per capita in $US (log scale)",
       scales = list( x = list(
         at = log(c(30,100,300,1000,3000,8000)),
         labels = c(30,100,300,1000,3000,8000)))) + 
  glayer(gpanel.fit(...))


#' ### Exercises
#' 
#' 1. Redo the above plot showing relationship of LE with a 1% increase in Health Expenditures.
#' 2. What happens if you introduce the possibility of interaction between health expenditures and $propGovt$?
#' 3. How do things change if we do a regression that gives more weight to larger countries? How should we do this?
#' 4. Compare Africa and South Asia: Is there evidence of a difference between LE adjusted for Health Expenditures and propGovt. Prepare an appropriate plot.
#' 5. Same for South Asia and "Other".
#' 
#' ### In the future:
#' 
#' We will explore the 'Lfx' function in 'spida' and the 'sc' function for generalized splines generated by the 'gsp' function.
#' 


