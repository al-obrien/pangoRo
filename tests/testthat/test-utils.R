suppressMessages(my_pangoro <- pangoro(offline = TRUE))

test_that("Recombinants can be identified...", {
  expect_type(is_recombinant(my_pangoro, c('EG.1', 'EC.1'), FALSE), 'logical')
  expect_type(is_recombinant(my_pangoro, c('EG.1', 'EC.1'), TRUE), 'character')

  expect_equal(is_recombinant(my_pangoro, 'EG.1'), TRUE)
  expect_equal(is_recombinant(my_pangoro, c('EG.1', 'EC.1', 'BA.1', 'XBB.1.9.1.1.5.1')), c(TRUE, FALSE, FALSE, TRUE))

  expect_equal(is_recombinant(my_pangoro, c('EG.1', 'EC.1'), TRUE), c('XBB', NA_character_))

})
