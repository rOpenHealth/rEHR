rCPRD
=====

David A. Springate 2014

R tools for processing and extracting clinical information from downloaded CPRD cohorts
---------------------------------------------------------------------------------------

This in development package will provide tools for interrogating cohorts of primary care data downloaded from the [CPRD](www.cprd.com).

It will contain tools for:

* Importing zipped text files to a SQLite database and then into R
* Searching and extracting from tables based on clinical codes, and other clinical events (e.g. diagnostic test results)
* Merging across several file types to generate flat files suitable for further analysis
* Using sampling methods (such as incidence density sampling) to produce case-control datasets
* Producing time-varying variables for survival analysis
* Generating common prevalence and incidence statistics
* Integrating with the [ClinicalCodes](www.clinicalcodes.org) repository to automate download and import of clinical codes to an analysis
* Use text-mining and NLP techniques to produce draft clinical code lists.

The tools will be able to be used interactively or fully automated from the command line and controlled by a simple [YAML](www.yaml.org) file.

Please note that although these tools are freely available and open source (See the [licence](https://github.com/rOpenHealth/rCPRD/blob/master/LICENSE)), CPRD is only available through a seperate licence and is not freely distributable (not least for reasons of patient confidentiality).  For this reason, no CPRD clinical data will ever be available from this resource.

The master branch will always be checked with devtools


