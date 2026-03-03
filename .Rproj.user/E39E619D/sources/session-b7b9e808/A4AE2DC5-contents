# -------------------------------------------------------------
# utils_versioning.R — simple version + changelog utilities
# -------------------------------------------------------------
get_version_file <- function() file.path("logs", "VERSION")

# Read current version (string like "v0.3.0"). If missing, initialize.
read_version <- function(init = "v0.3.0") {
  vf <- get_version_file()
  if (!file.exists(vf)) {
    writeLines(init, vf)
    return(init)
  }
  v <- readLines(vf, warn = FALSE)
  v <- v[1]
  if (!nzchar(v)) v <- init
  v
}

write_version <- function(v) {
  vf <- get_version_file()
  dir.create(dirname(vf), showWarnings = FALSE, recursive = TRUE)
  writeLines(v, vf)
  invisible(v)
}

.parse_version <- function(vstr) {
  # expects 'vX.Y.Z'
  vstr <- sub('^v', '', vstr)
  parts <- strsplit(vstr, '\\.')[[1]]
  as.integer(parts)
}

.format_version <- function(xyz) {
  paste0('v', paste(xyz, collapse = '.'))
}

bump_version <- function(level = c('patch','minor','major')) {
  level <- match.arg(level)
  cur <- read_version()
  xyz <- .parse_version(cur)
  if (length(xyz) < 3 || any(is.na(xyz))) stop('Malformed version string in logs/VERSION: ', cur)
  if (level == 'patch') xyz[3] <- xyz[3] + 1
  if (level == 'minor') { xyz[2] <- xyz[2] + 1; xyz[3] <- 0 }
  if (level == 'major') { xyz[1] <- xyz[1] + 1; xyz[2] <- 0; xyz[3] <- 0 }
  newv <- .format_version(xyz)
  write_version(newv)
  newv
}

append_changelog <- function(version, items, file = 'logs/CHANGELOG.md', date = Sys.Date()) {
  if (is.null(items) || length(items) == 0) items <- 'No details provided.'
  header <- sprintf('\n## %s (%s)\n', version, format(as.Date(date), '%Y-%m-%d'))
  body <- paste0('- ', items, collapse = '\n')
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  cat(header, body, '\n', file = file, append = TRUE)
  message('CHANGELOG updated: ', normalizePath(file))
  invisible(file)
}

start_run <- function(version_bump = NULL, changelog_items = NULL, append_changelog_on_start = TRUE) {
  # Ensure VERSION exists
  cur <- read_version()
  if (!is.null(version_bump)) {
    cur <- bump_version(version_bump)
  }
  if (append_changelog_on_start) {
    append_changelog(cur, changelog_items)
  }
  cur
}

# ------- Optional: light Git integration (safe no-op if git missing) -------
.in_git_repo <- function() {
  out <- tryCatch(system('git rev-parse --is-inside-work-tree', intern = TRUE, ignore.stderr = TRUE), error = function(e) '')
  length(out) == 1 && grepl('true', tolower(out[1]))
}

try_git_commit <- function(message, tag = NULL) {
  if (!.in_git_repo()) return(invisible(FALSE))
  system('git add -A')
  system(sprintf('git commit -m "%s"', gsub('"', '\\"', message)))
  if (!is.null(tag)) system(sprintf('git tag -a %s -m "%s"', tag, gsub('"', '\\"', message)))
  invisible(TRUE)
}