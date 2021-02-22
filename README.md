# Code Sample for the Criminal Justice Innovation Lab @ UNC Chapel Hill

Author: Andre Assumpcao

Email: andre.assumpcao@gmail.com

## Description

This script executes the functions in R package `CJIL`, which I have created
as a code sample for the Criminal Justice Innovation Lab @ UNC. The package
visits and downloads data from the NC Courts and NC demography APIs, process
and executes a simple analysis of the data. I believe this is a neat and new way of conveying research results to the public.

## Instructions

You should just execute the following code snippet on a R Session.

```r
# make sure you have devtools installed.
if (!require(devtools)) {install.packages('devtools')}

# install the CJIL from github
devtools::install_githug('assumpcao/CJIL')

# there you go. you can use all functions in the package.
# download the data
CJIL::data_download()

# process the data
CJIL::data_process('data.rds.xz')

# produce the analysis
CJIL::data_analyze()
```

## Others

Please feel free to email me @ andre.assumpcao@gmail.com for further clarification.
