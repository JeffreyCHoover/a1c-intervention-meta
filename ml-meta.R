needed_packages <- c("tidyverse", "here", "readxl", "metafor")
load_packages <- function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(x, repos = "https://cran.rstudio.com/")
  }
  
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
vapply(needed_packages, load_packages, logical(1))

# renamed some variables for easy loading into R
# Direct/Indirect = direct_level
# Old/New = age

# converted Cohen's d to Hedges g to correct for bias with small sample sizes
  # did this since there were a handful of studies with relatively small samples

# random-effects meta-analysis
  # restricted maximum likelihood estimator (Viechtbauer, 2005; Raudenbush, 2009)
  # full citations found using `?rma`

# probably more info than you need, but this used inverse-variance weighting

data <- read_xlsx(path = here("data/Meta Article Data.xlsx"))

data <- data %>% 
  select(Study, Year, direct_level, age, g, V_g)

# all studies
rma(yi = g, vi = V_g, data = data)

# direct
rma(yi = g, vi = V_g, data = data %>% filter(direct_level == "Direct"))

# indirect
rma(yi = g, vi = V_g, data = data %>% filter(direct_level == "Indirect"))

# old
rma(yi = g, vi = V_g, data = data %>% filter(age == "Old"))

# new
rma(yi = g, vi = V_g, data = data %>% filter(age == "New"))
