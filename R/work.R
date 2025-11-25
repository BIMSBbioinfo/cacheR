
install.packages(c("usethis", "devtools", "testthat", "roxygen2"))
usethis::create_package("./cacheR")
usethis::use_git()         # optional but recommended
usethis::use_roxygen_md()  # roxygen2 with Markdown
usethis::use_testthat(3)


("decorate.R")
source("cacheTree.R")

testthat::test_dir(".")

cacheTree_reset()
cache_dir <- file.path(tempdir(), "cache_manual")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

inner_fun <- cacheFile(cache_dir) %@% function(x) x + 1
outer_fun <- cacheFile(cache_dir) %@% function(x) inner_fun(x) * 2

outer_fun(3)

nodes <- cacheTree_nodes()
names(nodes)

outer_id <- grep("^outer_fun:", names(nodes), value = TRUE)
inner_id <- grep("^inner_fun:", names(nodes), value = TRUE)
nodes[[outer_id]]$children
nodes[[inner_id]]$parents

cacheTree_reset()
data_path <- file.path(tempdir(), "tree_test.csv")
write.csv(data.frame(x = 1:3), data_path, row.names = FALSE)

cache_dir <- file.path(tempdir(), "cache_manual2")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

f <- cacheFile(cache_dir) %@% function(p) {
  df <- read.csv(track_file(p))
  sum(df$x)
}

f(data_path)
cacheTree_nodes()


###
cacheTree_reset()

cache_dir <- file.path(tempdir(), "cache_manual_3")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- file.path(tempdir(), "tree_test.csv")
write.csv(data.frame(x = 1:3), data_path, row.names = FALSE)

f <- cacheFile(cache_dir) %@% function(p) {
  cat("Current node before track_file():", .cacheTree_current_node(), "\n")
  df <- read.csv(track_file(p))
  sum(df$x)
}

f(data_path)

nodes <- cacheTree_nodes()
print(names(nodes))
node <- nodes[[1]]
print(node$files)
print(node$file_hashes)


###
nodes <- cacheTree_nodes()
expect_true(length(nodes) >= 1)

np <- normalizePath(data_path, mustWork = FALSE)

# Find nodes that reference this file
with_file <- vapply(
  nodes,
  function(n) np %in% n$files,
  logical(1)
)

expect_true(any(with_file))

node <- nodes[[which(with_file)[1]]]
fh <- node$file_hashes
expect_true(np %in% names(fh))
expect_true(is.character(fh[[np]]) || is.na(fh[[np]]))
expect_gt(nchar(fh[[np]]), 0)
