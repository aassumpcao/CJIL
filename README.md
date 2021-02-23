# Code Sample for the Criminal Justice Innovation Lab @ UNC Chapel Hill

Author: Andre Assumpcao

Email: andre.assumpcao@gmail.com

## Description

This script executes the functions in R package `CJIL`, which I have created
as a code sample for the Criminal Justice Innovation Lab @ UNC. The package
visits and downloads data from the NC Courts and NC demography APIs, processes them
and executes a simple analysis of the data. I believe this is a neat and new way of conveying research results to the public.

## Instructions

You should just execute the following code snippet on a R Session.

```r
# make sure you have devtools installed.
if (!require(devtools)) {install.packages('devtools')}

# install the CJIL from github
devtools::install_github('aassumpcao/CJIL')

# there you go. you can use all functions in the package.
# download the data
data <- CJIL::data_download()

# process the data
data_cleaned <- CJIL::data_process(data)

# produce the analysis and store table
table <- CJIL::data_analyze(data_cleaned)

# output the table
print(table)
```

## Each function

`CJIL::data_download()` visits two NC goverment APIs and downloads data (NC Courts and the Office of State Budget and Management). They are returned as a `tibble` dataset.

`CJIL::data_process()` cleans up the data. It renames variables, transform scales and returns the new dataset.

`CJIL::data_process()` produces three simple analyses: a) it saves a graph of the evoluation of the type of criminal cases in North Carolina; b) it oureturns the tputs regression coefficients for a simple correlation between `log(population)` and `number_of_cases`; c) it saves the regression line from b).

## Others

Please feel free to email me @ andre.assumpcao@gmail.com for further clarification.
