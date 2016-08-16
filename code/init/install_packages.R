# Packages used within the book
inst_pkgs = load_pkgs =  c("devtools","reshape2","ggplot2","astsa","ggfortify",
                           "zoo","gmwm","highfrequency","timeDate")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Install from github
devtools::install_github("SMAC-Group/imudata")