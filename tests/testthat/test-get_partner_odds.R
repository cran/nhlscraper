test_that('get_partner_odds() returns tibble', {
  skip_if_offline()
  test <- get_partner_odds()
  expect_true(tibble::is_tibble(test))
})

test_that('get_partner_odds(\'USA\') returns empty tibble', {
  skip_if_offline()
  test <- get_partner_odds('USA')
  expect_true(tibble::is_tibble(test) && nrow(test)==0)
})
