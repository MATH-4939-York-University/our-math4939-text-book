#' 
#' # Glossary of Terms and Concepts (FEEL FREE TO ADD OR CHANGE ANYTHING)
#' 
#' ## Regular Expressions 
#' 
#' Regular Expressions is a way to alter, search, count, adjust texts or strings of characters.
#' There are 3 main commands in regular expressions that we will look at.
#' 
#' grep, grepl, sub, gsub, regexpr, gregexpr
#' 
#' We will now look at the function Grep
#' Take the following

x <- c("Hello", "He", "Hel", "hello", "hel1")

grep("hel", x)

#' As we can see, it returns back all elements that have "hel".
#' It does not return back Hello because it is case sensitive.

grep("hel", x, ignore.case=T)

#' By adding the ignore.case=T, it remove the case sensitive rule.
#' Another way is to give it a []. For example

grep("[Hh]el", x)

#' This will look up all elements with or without caps on the first letter."
#' 
#' 
#' Suppose we want to add up how many of these elements contain "hel"

length(grep("hel", x, ignore.case=T))

#' This calculates the number of elements that contain "hel".
#' 
#' Suppose we want to list out all the functions that contain "hel".

grep("[Hh]el", x, value=TRUE)

#' By adding value=TRUE, this will list all elements in x that contain "hel".
#' 
#' grepl will just return the elements as TRUE or FALSE.

grepl("hel", x)

#' One of the biggest problems is that "hel" might show up on other places in the data.
#' It will not give us the specific location of it. Data can contain thousands and thousands of
#' information.
#'  



