context('exphist')

test_that('exphist returns an objet of class "exphist"', {
  expect_error(exphist(NULL), 'is.numeric\\(winsize\\) && winsize > 1 is not TRUE')
  expect_error(exphist(winsize=NULL), 'is.numeric\\(winsize\\) && winsize > 1 is not TRUE')
  expect_error(exphist(epsilon=NULL), 'is.numeric\\(epsilon\\) && 0 < epsilon && epsilon < 1 is not TRUE')
  expect_error(exphist(1,1), 'is.numeric\\(winsize\\) && winsize > 1 is not TRUE')
  expect_error(exphist(2,1), 'is.numeric\\(epsilon\\) && 0 < epsilon && epsilon < 1 is not TRUE')
  expect_error(exphist(2,1,'0'), 'is.null\\(duration\\) || \\(is.numeric\\(duration\\) && duration > 0\\) is not TRUE')
  expect_error(exphist(2,1,0), 'is.null\\(duration\\) || \\(is.numeric\\(duration\\) && duration > 0\\) is not TRUE')
  expect_s3_class(exphist(2, 0.05), 'exphist')
  expect_s3_class(exphist(2, 0.05, NULL), 'exphist')
  expect_s3_class(exphist(2, 0.05, 1), 'exphist')
})

test_that('insert yields an objet of class "exphist"', {
  eh <- exphist()
  expect_error(insert(eh, NULL), 'is.numeric\\(val\\) is not TRUE')
  expect_error(insert(1, eh), "class mismatch: expected 'exphist', found 'numeric'")
  expect_s3_class(insert(eh, 2), 'exphist')
})

test_that('value yields the correct sum of bucket values', {
  eh <- exphist()
  eh <- insert(eh, 1)
  eh <- insert(eh, 1)
  expect_equal(value(eh), 2)
})

test_that('time-based exphist yield the correct value and size', {
  eh <- exphist(duration=5)
  for(t in 1:6) eh <- insert(eh, 1, t)
  expect_equal(value(eh), 6)
  expect_equal(size(eh), 2)
})
