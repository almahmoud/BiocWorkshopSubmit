#' @importFrom rjsoncons jmespath
#' @importFrom utils download.file
#' @importFrom gh gh
read_gh_file <- function(ghrepo) {
    ghrepo <- unlist(strsplit(ghrepo, "/", fixed = TRUE))
    names(ghrepo) <- c("owner", "repo")
    res <- gh(
        "/repos/{owner}/{repo}/contents/{path}",
        owner = ghrepo["owner"],
        repo = ghrepo["repo"],
        path = "DESCRIPTION"
    )
    res <- jmespath(res, "download_url", auto_unbox = TRUE)
    download.file(res, desc <- tempfile())
    read.dcf(desc)
}

.dcf_parse_url <- function(descfile) {
    urlfield <- descfile[, "URL"]
    urls <- unlist(strsplit(urlfield, ","))
    urls[grepl("ghcr|docker", urls)]
}

.parse_description <- function(descfile) {
    description <- descfile[, "Description"]
    description <- gsub("\n", " ", description)
    head(strsplit(description, "\\.")[[1L]], 1L)
}
