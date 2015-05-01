[![Build Status](https://travis-ci.org/rOpenHealth/rEHR.png?branch=master)](https://travis-ci.org/rOpenHealth/rEHR)

rEHR
=====

David A. Springate 2014, 2015

R tools for processing and extracting clinical information from Electronic Medical Records
-----------------------------------------------------------------------------

This in development package provides tools for accelerating and automating the most common operations researchers perform to extract and analyse data from Electronic Medical Records Databases.  

The package is currently only tested with [CPRD](www.cprd.com) data, but it should be possible to configure it with other EHR data.  See the [ehr_system](https://github.com/rOpenHealth/rEHR/blob/master/R/ehr_system.R) code for details of how the interface with CPRD is implemented. See the [vignette](https://github.com/rOpenHealth/rEHR/blob/master/vignettes/introduction-to-rehr.pdf) for more details.

The package was originally called `rCPRD`. It is not yet on CRAN but you can install from github using devtools:

```R
install.packages("devtools")
require(devtools)
install_github("rEHR", "rOpenHealth")
require(rEHR)
```

See the [vignette](https://github.com/rOpenHealth/rEHR/blob/master/vignettes/introduction-to-rehr.pdf) for full usage details. Issues can be reported [here](https://github.com/rOpenHealth/rEHR/issues).