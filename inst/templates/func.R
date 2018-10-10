#
# @@name@@_func.R
#
# created on @@date@@
# @@author@@, <@@email@@>
#-----------------------------------------------------------------------
#
# common libraries
libs <- c(
 "tidyr",
 "dplyr",
 "devtools",
 "broom",
 "ggplot2",
 "tidyverse",
 "cowplot"
)

if (!require("pacman")) install.packages("pacman")
library("pacman")
pacman::p_load(char=libs)
