# This tests the subset argument of getAptamerDilution()
# for speed
reprex::reprex({
  suppressMessages(require(SomaNormalization))
  t <- Sys.time()
  a <- getAptamerDilution(apt.data = apt_data,
                          subset = getAptamers(sample.adat))
  t1 <- Sys.time() - t
  t <- Sys.time()
  b <- getAptamerDilution(apt.data = apt_data, subset = NULL)
  t2 <- Sys.time() - t
  t1; t2
  all.equal(a, b)
  as.numeric(t1) / as.numeric(t2)    # ratio
}, outfile = "prepNorm_speed")
