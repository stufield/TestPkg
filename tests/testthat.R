library(testthat)
library(TestPkg)
#print(Sys.getlocale("LC_COLLATE"))
#Sys.setlocale(locale = "en_US.UTF-8")
#Sys.setlocale(locale = "LC_COLLATE")

test_check("TestPkg")
