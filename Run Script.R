#Install required packages
packages <- c('lubridate','pscl','car','lmtest','ZIM','sandwhich')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

