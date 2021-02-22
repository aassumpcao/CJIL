test_that('execute all tests', {
  
  # test whether file has been downloaded
  data <- CJIL::data_download()

  # test whether download has worked
  expect_false(is.null(data))

  # test whether file has been processed
  CJIL::data_process(data) == CJIL::data

  # teste whether analysis has completed
  table <- CJIL::data_analyze(CJIL::data_process(data))

  # test whether table has been produced
  expect_false(is.null(table))

  # check whether files have been produced
  exists <- all(
    file.exists('./evolution_criminal_cases.pdf') &
    file.exists('./regression.pdf')
  )

  # if so, delete them
  if (exists) {
    file.remove('./evolution_criminal_cases.pdf')
    file.remove('./regression.pdf')
  }
})
