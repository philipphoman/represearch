# R version
rvers <- version$version.string

# Python version
# pyvers <- system("python --version")
# pysurfvers <- system("pip freeze | grep pysurf")
#pyvers <- "2.7.13"
#pysurfvers <- "0.7"
pyplusvers <- system("pyv=$(python -V 2>&1); echo $pyv", intern=TRUE)
pysurfplusvers <- system("pip list | grep pysurf", intern=TRUE)
sessInfo <- sessionInfo()
