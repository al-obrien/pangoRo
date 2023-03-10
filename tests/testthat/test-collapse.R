suppressMessages(my_pangoro <- pangoro(offline = TRUE))

test_that("Alias scalar compressions...", {
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.1.529.1'), 'BA.1', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.617.2.4'), 'AY.4', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.617.2.4.3.1'), 'AY.4.3.1', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.617.2'), 'B.1.617.2', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1'), 'B.1', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B'), 'B', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'XA.1'), 'XA.1', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'XA'), 'XA', ignore_attr = TRUE)
})

test_that("Alias vector compressions...", {
  expect_equal(collapse_pangoro(my_pangoro, c( 'B.1.1.529.2.75.1.2', 'B.1.1.529.1', 'B.1.617.2.4', 'B.1.617.2.4.3.1',
                                               'B.1.617.2', 'B.1', 'B',  'XA.1', 'XA')),
               c('BL.2', 'BA.1', 'AY.4', 'AY.4.3.1', 'B.1.617.2', 'B.1', 'B', 'XA.1', 'XA'), ignore_attr = TRUE)
})

test_that("Alias expanding unknown and blanks...", {
  expect_equal(collapse_pangoro(my_pangoro, NA_character_), NA_character_, ignore_attr = TRUE)
})

test_that("Alias expanding by max_levels", {
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.1.529.1.2', 0), 'B.1.1.529.1.2', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.1.529.2.75.1.2', 1), 'BA.2.75.1.2', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.1.529.2.75.1.2', 2), 'BL.2', ignore_attr = TRUE)
  expect_equal(collapse_pangoro(my_pangoro, 'B.1.1.529.2.75.1.2', 3), 'BL.2', ignore_attr = TRUE)
})
