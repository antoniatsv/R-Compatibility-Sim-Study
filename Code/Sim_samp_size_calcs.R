library(pmsampsize)
library(pmvalsampsize)

pmvalsampsize(
  type = "b",
  cslope = 1,
  csciwidth = 0.2,
  oe = 1,
  oeciwidth = 0.2,
  cstatistic = 0.6,
  cstatciwidth = 0.1,
  lpnormal = c(-1.470, 0.5),
  prevalence = 0.2,
  seed = 12345)

pmsampsize(type = "b",
           parameters = 2,
           prevalence = 0.2,
           cstatistic = 0.6)
