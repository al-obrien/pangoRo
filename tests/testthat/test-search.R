my_pangoro <- pangoro(offline = TRUE)

test_that("Alias search...", {
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B.1.1.529.1'), FALSE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B.1.1.529.5'), TRUE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B.1.617.2.4'), FALSE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B.1.617.2.4.3.2'), FALSE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B.1'), TRUE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BZ.1', 'B'), TRUE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BA.5', 'BF'), TRUE, ignore_attr = TRUE)
  expect_equal(search_pangoro(my_pangoro, 'BA.5', 'BF.1'), TRUE, ignore_attr = TRUE)
})
