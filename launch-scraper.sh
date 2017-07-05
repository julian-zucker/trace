#!/usr/bin/bash
#########################################################################################
#
#          FILE: launch-scraper.sh
#
#         USAGE: sh launch-scraper.sh
#
#   DESCRIPTION: Launches multiple RSelenium tasks in parallel to scrape the TRACE
#                data at a reasonable speed.
#
#       OPTIONS: ---
#  REQUIREMENTS: R scripts from github.com/julian-zucker/trace. Also, a lo
#          BUGS: Doesn't run successfully, TRACE evaluation website prevents multiple 
#                accesses from the same account at the same time
#         NOTES: There are currently ~17,000 TRACE evaluations, the starting numbers
#                should be changed if that number changes
#        AUTHOR: Julian Zucker, julian.zucker@gmail.com
#  ORGANIZATION: uConnect
#       CREATED: 4 July 2017
#      REVISION: ---
#          TODO: ---
# 
#########################################################################################

Rscript scraper.R 4568 1     &
Rscript scraper.R 4569 1501  & 
Rscript scraper.R 4570 3001  &
Rscript scraper.R 4571 4501  &
Rscript scraper.R 4572 6001  &
Rscript scraper.R 4573 7501  &
Rscript scraper.R 4574 9001  &
Rscript scraper.R 4575 10501 &
Rscript scraper.R 4576 12001 &
Rscript scraper.R 4577 13501 &
Rscript scraper.R 4578 15001 &
Rscript scraper.R 4579 16501
