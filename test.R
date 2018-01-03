library(dplyr)
library(mlr)
library(llama)

sourceDir <- function(path, trace = TRUE, ...) {
		for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
					if(trace) cat(nm,":")
		source(file.path(path, nm), ...)
			    if(trace) cat("\n")
			}
}
sourceDir("helpFunctions")
source("utils_disjoint.R")
source("linucb_disjoint.R")
