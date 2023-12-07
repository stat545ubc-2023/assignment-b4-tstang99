Exercise 2
================
2023-12-07

# Description

This function converts an input string into my version of Pig Latin,
which basically remove the first or first group of vowel and then move
the first or first group of consonant to the back and then add xy after
it. It works for any capitalization, number and any punctuation marks as
the function wil clean these.

``` r
#' My own version of Pig Latin
#'
#' @description This function takes an input string then converts to my version of Pig Latin. 
#' @param x An input string 
#'
#' @return My version of the Pig Latin based on the input string
#' @export
#'
#' @examples mylatin("azzki")
#' @examples mylatin("nzzkaki")
mylatin <- function(x) {
  if(!(is.character(x))){
    stop("Please input a string")
  }
  if(length(x) != 1){
    stop("This function requires the input to be a single string")
  }
  vowels <- letters[c(1, 5, 9, 15, 21)]
  vowel <- paste0("[", stringr::str_flatten(vowels), "]+")
  consos <- letters[-c(1, 5, 9, 15, 21)]
  conso <- paste0("[", stringr::str_flatten(consos), "]+")
  x <- x %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all(pattern = "[^a-z]")
  if(length(unlist(stringr::str_extract_all(x, vowel))) == 0 | length(unlist(stringr::str_extract_all(x, conso))) == 0){
    return(x)
  }
  first_vowel <- stringr::str_extract_all(x, vowel)[[1]][1]
  first_conso <- stringr::str_extract_all(x, conso)[[1]][1]
  x <- stringr::str_remove(x, first_vowel)
  x <- stringr::str_remove(x, first_conso)
  return(str_c(x,first_conso,"xy"))
}
```

# Examples

``` r
example_words <- c("a", "by", "aeiou", "zxcvbnm", "lunch", "Dinner", "steak,prime rib", 
                   "Milk tea.")
for(word in example_words){
  cat(paste0('My version of Pig Latin for \"', word, '\" is ',
             '\"', mylatin(word), '\".', '\n'))
}
```

    ## My version of Pig Latin for "a" is "a".
    ## My version of Pig Latin for "by" is "by".
    ## My version of Pig Latin for "aeiou" is "aeiou".
    ## My version of Pig Latin for "zxcvbnm" is "zxcvbnm".
    ## My version of Pig Latin for "lunch" is "nchlxy".
    ## My version of Pig Latin for "Dinner" is "nnerdxy".
    ## My version of Pig Latin for "steak,prime rib" is "kprimeribstxy".
    ## My version of Pig Latin for "Milk tea." is "lkteamxy".

# Tests

``` r
test_that("mylatin correctly checks the input", {
  expect_error(mylatin(1), "Please input a string")
  expect_error(mylatin(c("abc","cde")), "This function requires the input to be a single string")
  expect_error(mylatin(TRUE))
})
```

    ## Test passed 😀

``` r
test_that("mylatin correctly rearrange letters", {
  expect_equal(mylatin("nzzkaki"), "kinzzkxy")
  expect_equal(mylatin("azznekk"), "ekkzznxy")
  expect_equal(mylatin("zzzk"),"zzzk")
  expect_equal(mylatin("aeiou"),"aeiou")
})
```

    ## Test passed 🥳

``` r
test_that("mylatin works even with punctuation marks", {
  expect_no_error(mylatin("a,b,c"))
  expect_no_error(mylatin(",a"))
})
```

    ## Test passed 🎊

``` r
test_that("The function works the same regardless of any non-letter characters and capitalization", {
  expect_equal(mylatin("cbzc"), mylatin("cb,..z12c"))
  expect_equal(mylatin("zzzxk"), mylatin("zz,z.x;k?"))
  expect_equal(mylatin("Acbzc"), mylatin("Acbzc"))
  expect_equal(mylatin("Lzzzxk"), mylatin("lzz,,.z12x!k"))
})
```

    ## Test passed 🎊
