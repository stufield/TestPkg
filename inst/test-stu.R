
test_that("stu", {
  expect_equal(4, 2+2)
})

rmarkdown::render("stu.Rmd",
                  #output_file = "temp/stu.html",
                  knit_root_dir = ".",
                  #output_dir  = "temp",
                  params      = list(
                    my_name = "test",
                    study_name = "unit-tests-03-class-a",
                    resp_var   = "class_response",
                    cat_clin_cov = "gender",
                    cont_clin_cov = "age",
                    stat       = "fdr",
                    stat_cutoff = 0.7,
                    fold_change_cutoff = 0.1,
                    odds_ratio_cutoff = 2,
                    create_training = "single",
                    training_split  = 0.8
                  ),
                  envir = new.env()
                )
