#' Computes the length of intersection of two intervals
#'
#' @export
#'
#' @param L1 a numeric vector.
#' @param U1 a numeric vector.
#' @param L2 a numeric vector.
#' @param U2 a numeric vector.
#'
#' @return a vector.

overlap.length = function(L1,U1,L2,U2) # provide length of [L1,U1] intersection [L2,U2]
{
  pmax(0, pmin(U1,U2) - pmax(L1,L2) )
}

#' Coerce a factor vector to a numeric vector
#'
#' @export
#'
#' @param x a factor vector.
#'
#' @return a numeric vector.

unfactor = function(x)
{
  as.numeric(levels(x))[x]
  # above is recommended and slightly more efficient than as.numeric(as.character(x))
}


#' Save R objects to a file
#'
#' @description
#' This function is a wrapper for the base `R` [save()] function that adds prefix/suffix information and compression to the R objects that are saved to file.
#'
#' @export
#'
#' @param object an object or a character element representing the R object to be saved.
#' @param prefix.suffix a named (prefix, suffix), two-element character vector representing the prefix and suffix for the file name. By default, the prefix is "This file contains an R object called " and the suffix is ".SavedFromR".
#' @param path.with.trailing.slash set to null by default.
#' @param time.stamp the format for the timestamp that appears in the file name. By default, the format is yyyy-mm-dd hh-mm-ss.
#'
#' @return none.

verbose.save = function(object, path.with.trailing.slash="", prefix.suffix=c(prefix="This file contains an R object called ", suffix=".SavedFromR"),time.stamp=gsub(":","-",Sys.time()))
{
  if(time.stamp != "")
    time.stamp = paste0(" (", time.stamp,")")

  if(!is.character(object)) # called as verbose.save(Elvis) rather than verbose.save("Elvis").  Need to mock the latter
    object = deparse(substitute(object)) # object = "Elvis"

  output.file.name = paste0(path.with.trailing.slash, prefix.suffix["prefix"], object, time.stamp,prefix.suffix["suffix"])
  code = paste0("save(",object,",file='",output.file.name,"', compress = 'xz')")
  eval(parse(text=code))
}

#' Split a character vector of length 1 into several elements
#'
#' @export
#'
#' @param string a character vector of length 1.
#' @param separator a character vector on which to split the `string` parameter. By default, " ".
#'
#' @examples
#' split_str("Hello world!")
#' # [1] "Hello"  "world!"
#'
#' @return a character vector.

split_str = function(string, separator=" ")
{
  rare.symbol = "\a" #   aka intToUtf8(7)
  if(separator != " ")
   string=gsub(" ",rare.symbol,string)

  string =           strsplit(string ,separator,fixed=TRUE )[[1]] # base R version is fine
 #string = stringr::str_split(string ,separator,fixed=TRUE )[[1]]
  if("" %in% string)
    string = setdiff(string,"")

  stringr::str_trim( gsub(rare.symbol," ",string) )
}
# split_str("A B C"," ") and split_str("A  B  C"," ") and split_str("A+B+C","+") and split_str("A + B + C","+") all give "A" "B" "C" unlike base::strsplit or stringr::str_split
# c( split_str("A B C"," ")      , split_str("A  B  C"," ")      , split_str("A+B+C","+")        , split_str("A + B + C","+")        ) %in% LETTERS[1:3]
# c( strsplit ("A B C"," ")[[1]] , strsplit ("A  B  C"," ")[[1]] , strsplit ("A+B+C", "+" )[[1]] , strsplit ("A + B + C", "+" )[[1]] ) %in% LETTERS[1:3]
# c( strsplit ("A B C"," ")[[1]] , strsplit ("A  B  C"," ")[[1]] , strsplit ("A+B+C","[+]")[[1]] , strsplit ("A + B + C","[+]")[[1]] ) %in% LETTERS[1:3]
# c( str_split("A B C"," ")[[1]] , str_split("A  B  C"," ")[[1]] , str_split("A+B+C", "+" )[[1]] , str_split("A + B + C", "+" )[[1]] ) %in% LETTERS[1:3]
# c( str_split("A B C"," ")[[1]] , str_split("A  B  C"," ")[[1]] , str_split("A+B+C","[+]")[[1]] , str_split("A + B + C","[+]")[[1]] ) %in% LETTERS[1:3]
# split_str("Elvis Presley","+")                 # versions prior to 0.0.7 would fail here
# split_str("Elvis  Presley+Elton John","+")     # versions prior to 0.0.7 would fail here
# split_str("Elvis  Presley  +  Elton John","+") # versions prior to 0.0.7 would fail here

#' Reverse the characters of a character vector
#'
#' @export
#'
#' @param string a character vector of length 1.
#'
#' @examples
#' rev_str("Hello world!")
#' # [1] "!dlrow olleH"
#'
#' @return a character vector of length 1.

rev_str = function(string) { intToUtf8 ( rev ( utf8ToInt (string) ) ) }  # "Hello" becomes "olleH"
