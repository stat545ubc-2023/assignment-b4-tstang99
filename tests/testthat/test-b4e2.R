test_that("mylatin correctly checks the input", {
  expect_error(mylatin(1), "Please input a string")
  expect_error(mylatin("a"), "Length of the string must be greater than or equal to two")
})

test_that("mylatin correctly rearrange letters", {
  expect_equal(mylatin("abc"), "cabya")
  expect_equal(mylatin("nbanfl"), "lnbanfya")
})

test_that("mylatin works even with punctuation marks", {
  expect_no_error(mylatin("a,b,c"))
  expect_no_error(mylatin(",a"))
})