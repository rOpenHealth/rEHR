[![Build Status](https://travis-ci.org/rOpenHealth/rEHR.png?branch=master)](https://travis-ci.org/rOpenHealth/rEHR)

rEHR
=====

David A. Springate 2014, 2015

R tools for processing and extracting clinical information from Electronic Medical Records
-----------------------------------------------------------------------------

This in development package provides tools for accelerating and automating the most common operations researchers perform to extract and analyse data from Electronic Medical Records Databases.  

The package is currently only tested with [CPRD](www.cprd.com) data, but it should be possible to configure it with other EHR data.  See the [ehr_system](https://github.com/rOpenHealth/rEHR/blob/master/R/ehr_system.R) code for details of how the interface with CPRD is implemented. See the [vignette](https://github.com/rOpenHealth/rEHR/blob/master/vignettes/introduction-to-rehr.pdf) for more details.

The package can be installed from CRAN <https://CRAN.R-project.org/package=rEHR> or from github using devtools:

```R
install.packages("devtools")
require(devtools)
install_github("rEHR", "rOpenHealth")
require(rEHR)
```

See the [vignette](https://github.com/rOpenHealth/rEHR/blob/master/vignettes/introduction-to-rehr.pdf) for full usage details. 

The package was submitted to CRAN but it was removed because it saves temporary files. This is very difficult to overcome in analyses of these databases and a major reconfigurtion would be required for it be resubmitted to CRAN.

Also note that the package has various dependencies which tend to break down when the relevant packages are updated in newer versions of R. Although we are trying our best to provide the constant updates needed, the package currently only works with R-3.3.2, as stated in the relevant publication: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0171784

Clinical code list construction functions have been absorbed from the [rpcdsearch](https://github.com/rOpenHealth/rpcdsearch) package.  See the [Codelist construction vignette](https://github.com/rOpenHealth/rEHR/blob/master/vignettes/codelists.pdf) for more details.

Issues can be reported [here](https://github.com/rOpenHealth/rEHR/issues).
