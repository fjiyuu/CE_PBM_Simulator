# Sys.setenv(RETICULATE_PYTHON = "/Users/fujiiyuusuke/miniforge3/envs/test_env/bin/python")

python_path <- "/Users/fujiiyuusuke/miniforge3/envs/test_env/bin/python"

library(tidyverse)
library(tidyr)
library(scales)
library(tictoc)
library(readxl)
library(gridExtra)
library(actuar)
library(openxlsx)
library(ggpubr)
library(reticulate)
use_python(python_path)
options(readr.show_col_types = FALSE)
path <- str_c(getwd(),"/")
# load --------------------------------------------------------------------

load(str_c(path,"Inputs/Data/StockFlow/df_historical.RData"))
load(str_c(path,"Inputs/Data/StockFlow/df_bau.RData"))

# source ------------------------------------------------------------------
product_list <- c("デジタルカメラ","スマートフォン")
source(str_c(path,"Codes/Model/my_functions.R"))
source(str_c(path,"Codes/Model/model_CE.R"))
source(str_c(path,"Codes/Model/model_CE_beta.R"))

# tic()
# model_CE()
# toc()
theme_set(theme_bw(base_family = "HiraKakuProN-W3"))