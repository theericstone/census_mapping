####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Collect Data from Census API
## 
## Notes
## From Chris's email:
## a map (likely using amcharts jquery mapping tool) that lists top employment
## sectors for each jurisdiction. Each state and each county would have its own
## dedicated page that listed the top employment sectors in the jurisdiction. 
## The Census Bureau compiles this data every 5 years:
## http://www.census.gov/econ/susb/index.html
## 
## API key: fb9a5df29c5d7510df67ef32fe7f311f9a0eb1dc
##
## SUSB MSA codes: http://www2.census.gov/econ/susb/data/msa_codes_2007_to_2011.txt
## State, county, NAICS data: http://www.census.gov/econ/susb/data/susb2011.html
##
## Creator: Eric Stone (ericstone@me.com) 
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(stringr)

naics <- data.table(read.csv("data/county_naicssectors_2011.csv"))

#get rid of US totals
naics <- naics[ !description == "State Total" ]

naics[ county_code ==  999 ]
