#' ---
#' title: "MATH 4939: Notes on SAS and R: Chapter 2"
#' author: "Georges Monette"
#' date: "`r format(Sys.time(), '%B %d, %Y %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 6
#' ---
#' Generated:
{{format(Sys.time(), '%B %d, %Y %H:%M')}}
#'
#' ## 2.2.1 Add derived variable to a data set
#' 
#' Adding a 'derived variable' means adding a variable
#' that is 'derived' from other variables in the data set.
#' 
#' There are 4 basic ways of doing this 
#' (+ many, many more 'non-basic' ways). 
#' The text describes two ways. I would 
#' encourage another way that I consider better.
#' 
#' Let's work with a small data frame:
(zd <- data.frame(x=1:3, y = 11:13, 
                  a = c('John','Mary','Jules'))) 
#' Note that putting () around an assignment causes it to be printed.
#' 
#' ### Method 1: Explicit references (ok for single new variable from single existing variable)
#' 
z1 <- zd
z1$z <- z1$x + z1$y
z1$z2 <- z1$z^2  # note that we are referring to a variable not in the original data frame
z1
#'
#' ### Method 2: Using 'transform' (doesn't always work)
#' 
#' Using 'transform', as in the text, won't work with this
#' problem because 'transform' can't handle variables that
#' don't already exist in the original data set.
#' 
#+ eval=FALSE
z2 <- zd
z2 <- transform(z2, z = x + y, z2 = z^2)
#' 
#' ### Method 3: Using 'with' (good if creating only one new variable from many)
#' 
#' This is useful if you're creating one new variable from many
#' existing variables in the data set and obviates the finger
#' contortions to type many '$'s.
#' 
z3 <- zd
z3$z <- with(z3, x + y)
z3$z2 <- with(z3, z^2) # not much of an advantage compared with Method 1
z3
#'
#' ### Method 4: Using 'within' (recommended for anything more complex)
#'
#' The second argument of 'within' is an expression. If it has
#' more that one statement, use curly braces as below. Note that
#' a statement can refer to previously assigned variables -- in contrast with 'transform'.  
z4 <- zd
z4 <- within( z4, {
  z <- x + y
  z2 <- z^2
})
z4
#' You can also use the deprecated '=' sign for assignment:
z4 = zd
z4 = within( z4, {
  z = x + y
  z2 = z^2
})
z4
#' One common complaint about 'within' and 'transform' is that
#' they replace the data set and could 'ruin' it if there's an
#' error in the statement! This isn't a problem with 
#' reproducible programming because your data frame is always
#' recreatable within the script.
#' 
#' ## 2.2.18 Lagged variable
#' 
#' There is a 'lag' function in R but it works with other functions that specialize in time series.
#' 
#' We will see later how R makes it possible to write a
#' flexible lag function that works with complex hierarchical data. In the meantime, the 'lagk' function in the text is pretty good. I would add k = 1 as a default and allow negative lags.
lagk <- function(x, k = 1) {
  if(!(floor(k) == k)) stop("k must be an integer")
  if(k == 0) return(x)
  if(k > 0) return(c(rep(NA,k), x)[1:length(x)])
  if(k < 0) return(rev(lagk(rev(x), -k)))
}
z <- letters[1:10]
z
lagk(z)
lagk(z, 4)
lagk(z, -3)
lagk(z, -20)
# lagk(z, .5)  # produces an error
#'
#' ## 2.3.1 Subsetting observations
#' 
#' The example in the text:
# zd[x==1,] # produces an error
#' produces an error unless 'x' is a variable 
#' that can be evaluated outside 'zd'.
#' 
#' 'subset' works with variables in the data set:
subset(zd, x > 1)
subset(zd, a %in% c('Mary','Jules'))
subset(zd, grepl('^J',a)) # the power of regular expressions
subset(zd, nchar(as.character(a)) > 4) # 'a' is a factor and need to be transformed to a character for 'nchar' to work
subset(zd, a %in% c('Mary','Jules'), c('x','a')) # but not for '%in%' !!
subset(zd, select = c('x','a'))
#'
#' Note that the second argument of 'subset' selects rows and the third selects columns.  A rare feature here is that you
#' don't need to type the quotes for lazy typists.
#' 
#' ## 2.3.7 Convert from wide to long format
#' 
#' I like 'tolong' in the 'yscs' package. I find 'reshape'
#' finicky. 'tolong' assumes that your time-varying variables
#' in the wide file have the form 'VariableName_Time',
#' where 'VariableName' is the name of the variable in
#' the long file, 'Time' is the occasion which can be character or numeric and '_' is a separator. '_' is the default but
#' you can use anything including that's valid in a 
#' variable name, including a multicharacter separator which is very useful if you are already using '_' and '.' 
#' in your variable names.  'tolong' handles unbalanced
#' 'Time's, i.e. different sets of times for different
#' 'VariableName's which, I think, breaks 'reshape'.
#' 
#' 'long' is a synonym for 'tolong' in 'yscs'.
#' 
#' Have a look at the help pages for 'tolong' and 'towide'.
#' 
#' If these functions misbehave for you, please let me know.
#' 
#' Here's an example inspired by an analysis of left and right
#' brain ventricle sizes. The variables 'v_L' and 'v_R' represent
#' ventricle sizes on the left and right side of the brain respectively.  
#'  
zw <- data.frame(id =letters[1:10], id2= 11:20, v_L = 1:10, v_R = 11:20)
zw
library(yscs)  # devtools::install_github('gmonette/yscs')
(zl <- tolong(zw))
#' 'time'is a misnomer here. We could just change it:
names(zl) <- sub('^time$', 'side', names(zl))
zl
#' (by the way, what's the purpose of the '^' and the '$'?)
#' or we could specify the name of 'timevar' when we call 'tolong':
tolong(zw, timevar = 'Side', idvar = 'idn', ids = LETTERS[1:10])
tolong(zw, timevar = 'Side', idvar = 'idn', ids = zw$id2)
#'
#' ## 2.3.8 Convert from long to wide format
#' 
#' See 'towide' in the 'yscs' package.
#' 
zl 
(zww <- towide(zl, timevar = 'side'))
#' If '_' is already used in variable names, you can use a multicharacter separator:
(zww <- towide(zl, timevar = 'side', sep = "__"))
#' 'towide' is a 'wrapper' for reshape that
#' prepares the input so that 'reshape' can be used more
#' conveniently. In particular, time-invariant variables
#' are recognized so that they are not put in wide form
#' unnecessarity. Have a look at the help page.
#' 
#' Experiment and let me know if you have problems 
#' or suggestions for improvements.
#' 
#' ## 2.3.9 Concatenate and stack data sets
#' 
#' 'rbind' in the text only work with data frames of identical column structure.  You can stack heterogenous data frames with 'merge'
#' 
(z1 <- data.frame(x=1:3,y=letters[1:3]))
(z2 <- data.frame(x=11:14, z = LETTERS[1:4]))
# rbind(z1,z2) # Error
merge(z1, z2, all = T)
#' 'merge' works here but if there are identical rows in
#' the two data sets, those rows will occurs only once. Also
#' you can't tell which data set the row came from.
#' 
#' Here's a trick:
z1$dsname <- 'set 1'
z2$dsname <- 'set 2'
merge(z1, z2, all = T)
#'
#' ## 2.3.10 Sort datasets
#'
#' The example in the book doesn't work unless x1, x2, etc. are visible in the environment.
#' The 'yscs' package has an alternative:
sortdf( zd, ~ a)
sortdf( zd, ~ a/x) # sort by a, then by x
#'
#' ## 2.3.12 Summarizing and expanding data sets (the missing section)
#'
#' The examples section, 2.6.3 discusses the 'tally' function in the 'mosaic' package.
#' Forming summary tables and expanding tables into data sets is a very important
#' part of data manipulation but it's only treated with examples. 
#' 
#' In this section, we will discuss summarizing with the 'tab', 'capply' and 'up' 
#' functions from the 'yscs' package as well as the 'merge' function in the base
#' package.
#' 
#' I'll illustrate with the 'hs' data set in the 'yscs' package
#' 
head(hs)
tab(~ Sex, hs)  # includes Total
Tab(~ Sex, hs)  # no Total margins
tab(~ Sex + Minority, hs)
Tab(~ Sex + Minority, hs)
tab(~ Sex + Minority, hs, pr = 1)  # proportions by row
tab(~ Sex + Minority, hs, pr = 2)  # proportions by column
tab(~ Sex + Minority, hs, pr = 0)  # joint proportions
tab(~ Sex + Minority, hs, pct = 1)  # percents by row
tab(~ Sex + Minority, hs, pct = 2)  # percents by column
tab(~ Sex + Minority, hs, pct = 0)  # joint percents
round(tab(hs, ~ Sex + Minority, pct = 0),2)
#'
#' #### 3-dimensional tables
#' 
tab(~ Sex + Minority + Sector, hs, pct = 1)
tab(~ Sex + Minority + Sector, hs, pct = 1)
tab(~ Sex + Minority + Sector, hs, pct = 1:2)
tab(~ Sex + Minority + Sector, hs, pct = 2:3)
tab(~ Sex + Minority + Sector, hs, pct = 0)
#'
#' #### Table as a data set
#'
#' Turning the table into a data set with a frequency variable produces
#' a summary of the data we can plot.
#' 
#' We usually don't want the total margins, so we use 'Tab' instead of 'tab'
Tab(~ Sex + Minority + Sector, hs)
as.data.frame(Tab(~ Sex + Minority + Sector, hs))
as.data.frame(Tab(~ Sex + Minority + Sector, hs, pct = 0))
as.data.frame(Tab(~ Sex + Minority + Sector, hs, pct = 1))
tab(~ Sex + Sector, hs, pct = 2)
zt <- as.data.frame(Tab(~ Sex + Sector, hs, pct = 2))
zt
library(lattice)
gd(2)
xyplot(Freq ~ Sector , zt, groups = Sex, type = 'b', auto.key= T,
       ylab = "percentage of gender within each sector")
#'
#' ### A little excursion into mosaic plots
#'
# Graphical version:
library(vcd)  # visualizing categorical data
# help(package='vcd')
mosaic( ~ Sex + Minority, hs)
mosaic( ~ Sex + Minority, hs, direction = c('v','h'))
library(RColorBrewer)
display.brewer.all()  # nice color sets -- more refined looking than 'red','green','blue'
cols <- brewer.pal(9, 'Set1')  # this is how you choose 9 colour from 'Set1'
cols # in RGB hexadecimal notation: black is "#000000" and white is "#FFFFFF"
mosaic( ~ Sex + Minority, hs, direction = c('v','h'),
        gp = gpar(fill=cols[1:2]))
mosaic( ~ Sector + Minority, hs, direction = c('v','h'),
        gp = gpar(fill=cols[1:2]))
mosaic( ~ Sector + Minority + Sex, hs, direction = c('v','h','h'),
        gp = gpar(fill=cols[1:2]))
#' Different presentations allow you to compare different conditional
#' distributions.
mosaic( ~ Sector + Minority + Sex, hs, direction = c('v','v','h'),
        gp = gpar(fill=cols[1:2]))
#'
#' ### Back to data sets
#'
#' #### Summarize
#' 
#' Each row of 'hs' represents a student. Suppose we would like a 'school'
#' level data set with just the school-level variables.
head(hs)
dim(hs)
xqplot(hs)
hs.u <- up(hs, ~ school)
head(hs.u)  # one row per school, only the school-invariant variables
#'
#' Another way is to use the capply function in 'yscs'.
#' 
#' 'capply(x, id, FUN, ...)' applies the function "FUN" within each
#' cluster of 'x' formed by levels of 'id'.
#'  
hs$ses.mean <- with(hs, capply(ses, school, mean, na.rm = T))
hs$ses.sd <- with(hs, capply(ses, school, sd, na.rm = T))
hs$prop.female <- with(hs, capply( Sex == "Female", school, mean, na.rm = T))

head(up(hs, ~ school))
#'
#' If all you need are means of numeric variables within cluster
hs.us <- up(hs, ~ school, all = T)
#' Note that this returns modes for categorical variables, which is not
#' generally very useful.
#' 
#' ## Exercises
#' 
#' Practice all of these techniques as you explore the 'Arrests' data set in the 'effects' package.
#' 