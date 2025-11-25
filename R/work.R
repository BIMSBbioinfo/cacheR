
install.packages(c("usethis", "devtools", "testthat", "roxygen2"))
usethis::create_package("./cacheR")
usethis::use_git_config(
  user.name  = "frenkiboy",
  user.email = "vedran.franke@gmail.com"
)
usethis::use_git()         # optional but recommended
usethis::use_roxygen_md()  # roxygen2 with Markdown
usethis::use_testthat(3)

devtools::document()
devtools::build()
devtools::check()
devtools::test()

devtools::check_man()
devtools::check_doc()
