#' Function to download data from the NC Judicial Branch and the Office of
#' State Budget and Management
#'
#' @param filepath is where the download should be saved. the default is the
#' current directory.
#'
#' @importFrom rlang .data
#'
#' @export
data_download <- function() {

  # define main endpoint for court data and demography data
  endpoint1 <- 'https://data.nccourts.gov/api/records/1.0/download/'
  endpoint2 <- 'https://demography.osbm.nc.gov/api/records/1.0/download/'

  # define query parameters
  query1 <- list(dataset = 'caseload-inventory', format = 'json')
  query2 <- list(
    dataset = 'county-population-estimates-standard-revised', format = 'json'
  )

  # visit endpoint and download raw data
  r1 <- httr::GET(endpoint1, query = query1)
  r2 <- httr::GET(endpoint2, query = query2)

  # convert raw court data to json
  content <- rawToChar(r1$content)
  json <- jsonlite::fromJSON(content)

  # transform court data and variables into nice dataset
  data1 <- json %>%
    jsonlite::flatten() %>%
    tidyr::as_tibble() %>%
    dplyr::rename_all(~stringr::str_remove_all(., 'fields\\.')) %>%
    dplyr::mutate(year = stringr::str_sub(.data$period, 1, 4))

  # convert raw demography data to json
  content <- rawToChar(r2$content)
  json <- jsonlite::fromJSON(content)

  # transform demography data and variables into nice dataset
  data2 <- json %>%
    jsonlite::flatten() %>%
    tidyr::as_tibble() %>%
    dplyr::rename_all(~stringr::str_remove_all(., 'fields\\.')) %>%
    dplyr::mutate(
      county = stringr::str_remove(.data$county, ' County'),
      year   = stringr::str_sub(.data$year, 1, 4)
    )

  # join court data and demography data
  data <- dplyr::inner_join(data1, data2, by = c('county', 'year'))

  # return to file
  return(data)
}

#' Function to process the data and store a cleaned version on disk
#'
#' @param datapath is the where the NC Court and Demography data are saved.
#'
#' @importFrom rlang .data
#'
#' @export
data_process <- function(data) {

  # drop irrelevant columns and rename remaining columns
  data <- data %>%
    dplyr::select(-dplyr::matches('dataset|recordid.y|record_timestamp.y')) %>%
    janitor::clean_names() %>%
    dplyr::rename_all(~stringr::str_remove_all(., '_[xy]{1}\\b'))

  data <- data %>%
    dplyr::mutate(
      record_timestamp = lubridate::ymd_hms(.data$record_timestamp)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches('\\b(cat|file|juris|period|case)')), as.factor
    ) %>%
    dplyr::mutate(
      ln_number_of_cases = ifelse(
        .data$number_of_cases > 0, log(.data$number_of_cases), log(1)
      ),
      period = ordered(.data$period, levels = sort(unique(.data$period)))
    ) %>%
    dplyr::select(
      .data$period, .data$year, .data$county, 1:5, dplyr::matches('case_'),
      dplyr::matches('_case'), .data$population
    )

  # return dataset
  return(data)
}

#' Function to produce analysis of the data and print the results.
#'
#' @param datapath is the where the NC Court and Demography data are saved.
#' @param savedir is the folder where the analysis should be saved.
#'
#' @importFrom rlang .data
#'
#' @export
data_analyze <- function(data, savedir = './') {

  # load data from package if not set by user
  if (is.null(data)){
    nccourts <- CJIL::data
  } else {
    nccourts <- data
  }

  # create new dataset to check evolution of criminal cases over time
  nccourts_flat <- nccourts %>%
    dplyr::filter(.data$category == 'Criminal') %>%
    dplyr::group_by(.data$period, .data$case_type) %>%
    dplyr::summarize(
      total    = sum(.data$number_of_cases),
      ln_total = log(sum(.data$number_of_cases))
    ) %>%
    dplyr::ungroup(.data$case_type) %>%
    dplyr::mutate(perc = (.data$total / sum(.data$total)) * 100)

  # check case type evolution over time
  p <- nccourts_flat %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x     = .data$period,
        y     = .data$total,
        group = .data$case_type
      )
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(fill = .data$case_type),
      stat  = 'identity',
      color = 'black',
      size  = .3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x){ifelse(x > 0,paste(x / 1e6, 'million'), 0)}
    ) +
    ggplot2::scale_fill_brewer(
      ggplot2::aes(fill = .data$case_type), palette = 'RdPu'
    ) +
    ggplot2::labs(
      x = ggplot2::element_blank(), y = ggplot2::element_blank()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(angle = 90, size = 10),
      legend.position    = 'top',
      legend.title       = ggplot2::element_blank()
    )

  # save plot
  ggplot2::ggsave(
    filename = paste0(savedir, 'evolution_criminal_cases.pdf'),
    plot     = p,
    width    = 8,
    height   = 5,
    device   = grDevices::cairo_pdf
  )

  # define global mean population
  population_mean   <- mean(nccourts$population)
  population_median <- stats::median(nccourts$population)

  # is the distribution of cases similar across counties above and below the
  # mean and the median?
  nccourts_ttests <- nccourts %>%
    dplyr::mutate(
      above_mean   = ifelse(.data$population - population_mean > 0, 1, 0),
      above_median = ifelse(.data$population - population_median > 0, 1, 0)
    )

  # filter criminal cases
  ttests <- nccourts_ttests %>% dplyr::filter(.data$category == 'Criminal')

  # execute t-tests
  test_a <- stats::t.test(number_of_cases ~ above_mean, data = ttests)
  test_b <- stats::t.test(number_of_cases ~ above_median, data = ttests)

  # create two tables containing the information from the t-tests
  results <- list(test_a, test_b) %>%
    lapply(function(x){
      x %>%
        broom::tidy() %>%
        dplyr::select(-dplyr::matches('method|alternative')) %>%
        dplyr::mutate(p.value = round(.data$p.value, 3)) %>%
        dplyr::mutate_all(as.integer) %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::unite('conf.interval', dplyr::matches('conf'), sep = ',') %>%
        dplyr::mutate(
          p.value = '.000',
          conf.interval = paste0('[', .data$conf.interval, ']')
        )
    }) %>%
    dplyr::bind_rows()

  # format data to display results
  var_names <- names(results)

  # transpose data for better formatted table
  table <- results %>%
    t() %>%
    as.data.frame() %>%
    tibble::tibble() %>%
    dplyr::mutate(var = var_names) %>%
    dplyr::select(Statistic = .data$var, Mean = .data$V1, Median = .data$V2) %>%
    dplyr::slice(-6)

  table <- table[c(2:3, 1, 6, 4:5),]

  table$Statistic = c(
    'Mean of Group Below Population Threshold',
    'Mean of Group Above Population Threshold',
    'Difference in Means',
    'Confidence Interval',
    't-statistic',
    'p-value'
  )

  # run regression for the analysis between the relationship between population
  # and number of criminal cases
  reg <- stats::lm(number_of_cases ~ log(population), data = ttests) %>%
         summary()

  # plot regression chart on
  p <- ggplot2::ggplot(
    dplyr::filter(ttests, .data$category == 'Criminal'),
    ggplot2::aes(x = .data$population, y = .data$number_of_cases)
  )
  p <- p +
    ggplot2::geom_smooth(method = 'lm', formula = y ~ log(x)) +
    ggplot2::scale_x_continuous(
      labels = function(x){ifelse(x > 0,paste(x / 1e3, 'k'), 0)}
    ) +
    ggplot2::labs(
      x = 'County Population (in thousand residents)',
      y = 'Number of Criminal Cases'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10)
    )

    # save plot
    ggplot2::ggsave(
      filename = paste0(savedir, 'regression.pdf'),
      plot     = p,
      width    = 8,
      height   = 5,
      device   = grDevices::cairo_pdf
    )

  # return table for markdown
  return(table)
}
