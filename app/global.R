# Source all of R file

l_fun <- list.files(settings$sourcepath)
for(i in 1:length(l_fun)) source(paste0(settings$sourcepath, l_fun[i]))

l_proc <- list.files(paste0(getwd(),"/app/processing/"))
for(i in 1:length(l_proc)) source(paste0(getwd(),"/app/processing/", l_proc[i]))

rm(i, l_fun, l_proc)