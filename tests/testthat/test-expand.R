suppressMessages(my_pangoro <- pangoro(offline = TRUE))

test_that("Alias scalar expansions...", {
  expect_equal(expand_pangoro(my_pangoro, 'BA.1'), 'B.1.1.529.1', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'AY.4'), 'B.1.617.2.4', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'AY.4.3.2'), 'B.1.617.2.4.3.2', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'B.1'), 'B.1', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'B'), 'B', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'XA.1'), 'XA.1', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'XA'), 'XA', ignore_attr = TRUE)
})

test_that("Alias vector expansion...", {
  expect_equal(expand_pangoro(my_pangoro, c('BA.1', 'AY.4', 'AY.4.3.2', 'B.1', 'B')),
               c('B.1.1.529.1', 'B.1.617.2.4', 'B.1.617.2.4.3.2',  'B.1', 'B'), ignore_attr = TRUE)
})

test_that("Alias expanding unknown and blanks...", {
  expect_error(expand_pangoro(my_pangoro, ''))
  expect_equal(expand_pangoro(my_pangoro, NA_character_), NA_character_, ignore_attr = TRUE)
})

test_that("Alias expanding by max_levels", {
  expect_equal(expand_pangoro(my_pangoro, 'AY.4.3.2', 0), 'AY.4.3.2', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'AY.4.3.2', 0), 'AY.4.3.2', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'BL.1.2', 1), 'BA.2.75.1.1.2', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'BL.1.2', 2), 'B.1.1.529.2.75.1.1.2', ignore_attr = TRUE)
  expect_equal(expand_pangoro(my_pangoro, 'BL.1.2', 3), 'B.1.1.529.2.75.1.1.2', ignore_attr = TRUE)
})
