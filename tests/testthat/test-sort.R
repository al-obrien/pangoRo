suppressMessages(my_pangoro <- pangoro(offline = TRUE))

test_that("Sorting works", {
  expect_equal(sort_pangoro(my_pangoro, c('B', 'A', 'C')), c('A', 'B', 'C'))
  expect_equal(sort_pangoro(my_pangoro, expand_pangoro(my_pangoro,c('B', 'A', 'C'))), c('A', 'B', 'B.1.1.1'), ignore_attr = TRUE)
  expect_equal(sort_pangoro(my_pangoro, c('B.1.2', 'B', 'A', 'C')), c('A', 'B', 'C', 'B.1.2'))
  expect_equal(sort_pangoro(my_pangoro, c('BA.1', 'BA.5.1', 'BL.1.2', 'DR.1')), c('BA.1', 'BL.1.2',  'BA.5.1', 'DR.1'))
})

test_that("Sorting works by index", {
  expect_equal(sort_pangoro(my_pangoro, c('B', 'A', 'C'), index_only = TRUE), c(2, 1, 3))
  expect_equal(sort_pangoro(my_pangoro, c('B.1.2', 'B', 'A', 'C'), index_only = TRUE), c(3, 2, 4, 1))
  expect_equal(sort_pangoro(my_pangoro, c('BA.1', 'BA.5.1', 'BL.1.2', 'DR.1'), index_only = TRUE),
               c(1, 3, 2, 4))
})
