# Adapted from Sheena's code

# Libraries
library(tidyverse)

# looks like Sheena runs on one machine?
# I wonder if I can run the scrape and interpolation in parallel on ozone?
# There are some limitations on the EPA AQS AQI. Limited to 1 year and 1 county
# in 1 state. I'd like to use some version of apply rather than a for loop. 
# First step will be to construct a list of URLs to grab.

# AQS base url that contains my user name and password for AQS site; I will
# probably hide before I push to github




