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

.dcf_parse_url <- function(urlfield) {
    urls <- unlist(strsplit(urlfield, ","))
    urls[grepl("ghcr|docker", urls)]
}
