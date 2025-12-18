

# # -------------------------------------------------------------------------
# # 3. Graph Tracking Helpers
# # -------------------------------------------------------------------------

# #' Register a Node (Function or File)
# #' @keywords internal
# .register_node <- function(id, type, label = id, code = NULL) {
#   # Nodes are stored in a list, so we check using `[[`
#   if (is.null(.graph_cache$nodes[[id]])) {
#     .graph_cache$nodes[[id]] <- list(id = id, type = type, label = label, code = code)
#   }
# }

# #' Register an Edge (Dependency)
# #' @keywords internal
# .register_edge <- function(from, to) {
#   edge_id <- paste(from, to, sep = "->")
#   if (is.null(.graph_cache$edges[[edge_id]])) {
#     .graph_cache$edges[[edge_id]] <- list(from = from, to = to)
#   }
# }

# #' Export Execution Graph to _targets.R
# #' @export
# export_targets_file <- function(path = "_targets.R") {
#   # [FIX] .graph_cache$nodes is a list, not an environment. Use direct access.
#   nodes <- .graph_cache$nodes
#   edges <- .graph_cache$edges
  
#   header <- c(
#     "library(targets)",
#     "library(tarchetypes)",
#     "tar_option_set(packages = c('base'))",
#     "",
#     "list("
#   )
  
#   targets_list <- c()
  
#   # 1. Process File Inputs
#   file_nodes <- Filter(function(x) x$type == "file", nodes)
#   for (f in file_nodes) {
#     t_name <- gsub("[^a-zA-Z0-9_]", "_", basename(f$id))
#     t_name <- paste0("file_", t_name)
#     block <- sprintf("  tar_target(\n    name = %s,\n    command = \"%s\",\n    format = \"file\"\n  )", t_name, f$id)
#     targets_list <- c(targets_list, block)
#   }
  
#   # 2. Process Function Calls
#   func_nodes <- Filter(function(x) x$type == "function", nodes)
#   for (fn in func_nodes) {
#     cmd <- if (!is.null(fn$code)) fn$code else "NULL"
#     block <- sprintf("  tar_target(\n    name = %s,\n    command = { %s }\n  )", fn$label, cmd)
#     targets_list <- c(targets_list, block)
#   }
  
#   footer <- ")"
#   writeLines(c(header, paste(targets_list, collapse = ",\n\n"), footer), path)
#   message(sprintf("Exported %d targets to %s", length(targets_list), path))
# }