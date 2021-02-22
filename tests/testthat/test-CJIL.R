test_that('execute all tests', {
  
  # test whether file has been downloaded
  CJIL::data_download()

  # check if file exists
  if (file.exists('./data.rds.xz')) {file.remove('./data.rds.xz')}

  # test whether file has been processed
  CJIL::data_process('../../data-raw/data.rds.xz') == CJIL::data

  # teste whether analysis has completed
  CJIL::data_analyze()

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
