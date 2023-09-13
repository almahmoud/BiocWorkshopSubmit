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
    urlfield <- tryCatch(
        descfile[, "URL"],
        error = function(e) {
            shinytoastr::toastr_warning(
                "No 'URL' field found in the DESCRIPTION file"
            )
            ""
        }
    )
    urls <- unlist(strsplit(urlfield, ","))
    cont_url <- grepl("ghcr|docker", urls)
    if (any(cont_url))
        shinytoastr::toastr_warning(
            "No container 'URL' found in the DESCRIPTION file"
        )
    urls[cont_url]
}

.parse_description <- function(descfile) {
    description <- descfile[, "Description"]
    description <- gsub("\n", " ", description)
    head(strsplit(description, "\\.")[[1L]], 1L)
}

create_gh_issue <- function(ghrepo, title, body) {
    ghrepo <- unlist(strsplit(ghrepo, "/", fixed = TRUE))
    names(ghrepo) <- c("owner", "repo")
    gh(
        "POST /repos/{owner}/{repo}/issues",
        owner = ghrepo["owner"],
        repo = ghrepo["repo"],
        title = jsonlite::unbox(title),
        body = jsonlite::unbox(body)
    )
}
