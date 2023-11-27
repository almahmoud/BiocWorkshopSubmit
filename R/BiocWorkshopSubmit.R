# Code adapted from
# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

.TEMPLATE_FORM <- c(
    '/request', 'id="{{id}}"', 'title="{{title}}"',
    'description="{{description}}"', 'section="{{section}}"',
    'startfile="{{startfile}}"', 'source="{{ghrepo}}"',
    'docker="{{url}}:{{tag}}"'
)

.ISSUE_GH_REPO <- "Bioconductor/workshop-contributions"

.workshop_template <- function(.data, sep = "\n", template = .TEMPLATE_FORM) {
    whisker::whisker.render(
        paste(template, collapse = sep),
        data = .data
    )
}

mandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <- paste(
    ".mandatory_star { color: red; }", "#error { color: red; }", sep = "\n"
)

.ISSUE_BTN_CSS <-
    "color: #fff; background-color; #337ab7; border-color: #2e6da4"

#' Prepare a Bioconductor Workshop Submission
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs inlineCSS toggleState hidden hide reset show
#'   disable enable html
#' @importFrom shinyAce aceEditor updateAceEditor
#'
#' @param ... Arguments passed to \code{\link{shinyApp}}
#'
#' @export
BiocWorkshopSubmit <- function(...) {
    fieldsMandatory <- c("id", "title", "section", "ghrepo", "url")
    fieldsAll <- c("id", "title", "description", "section",
        "startfile", "ghrepo", "url", "tag")
    ui <- fluidPage(
        useShinyjs(),
        inlineCSS(appCSS),
        shinytoastr::useToastr(),
        titlePanel(
            windowTitle = "BiocWorkshop Form",
            title = div(
                img(
                    src = "images/bioconductor_logo_rgb_small.png",
                    align = "right",
                    style = "margin-right:10px"
                ),
                h1(id = "big-heading",
                   "Bioconductor Workshop Submission Form")
            )
        ),
        sidebarLayout(
            div(class = "sidebar",
                sidebarPanel(
                    div(
                        id = "prepop",
                        textInput(
                            inputId = "prepop",
                            label = "Existing GitHub Repository",
                            placeholder = "username/repository"
                        ),
                        actionButton(
                            "presubmit", "Populate", class = "btn-primary"
                        )
                    ),
                    br(),
                    div(
                        id = "form",
                        textInput(
                            "id",
                            mandatory("Workshop ID"),
                            placeholder = "abc123"
                        ),
                        textInput(
                            "title",
                            label = mandatory("Title"),
                            placeholder = "A Bioconductor Workshop Title"
                        ),
                        textInput(
                            "section",
                            label = mandatory("Section"),
                            placeholder = "BioC2023"
                        ),
                        ## TODO: point out workshop.bioconductor.org examples
                        textInput("description", "Description"),
                        textInput(
                            "ghrepo",
                            label = mandatory("GitHub Repository"),
                            placeholder = "username/repository"
                        ),
                        textInput(
                            "startfile", "Start File", value = "README.md"
                        ),
                        textInput(
                            "url",
                            label = mandatory("Container URL"),
                            placeholder = "ghcr.io/username/repo"
                        ),
                        textInput("tag", "Container Tag", value = "latest"),
                        actionButton("render", "Render", class = "btn-primary")
                    ),
                    hr(),
                    div(
                        id = "additional",
                        dateRangeInput(
                            "wdate",
                            label = "Workshop Date",
                            format = "yyyy-mm-dd",
                        ),
                        shinyTime::timeInput(
                            "wtime",
                            label = "Approx. Workshop Time (24h format)",
                            value = Sys.time(),
                            seconds = FALSE,
                            minute.steps = 15
                        ),
                        numericInput(
                            "wnpart",
                            label = "Expected Number of Participants",
                            value = 0,
                            min = 0,
                            max = 100,
                            step = 5
                        )
                    ),
                    hidden(
                        div(
                            id = "render_msg",
                            h3("Review the GitHub issue comment on the right"),
                            actionButton(
                                "post", "Create Issue", icon("paper-plane"),
                                style = .ISSUE_BTN_CSS,
                                class = "btn-danger"
                            )
                        )
                    ),
                    hidden(
                        div(
                            id = "thankyou_msg",
                            h3("Submitted successfully!")
                        )
                    ),
                    hidden(
                        span(id = "submit_msg", "Submitting..."),
                        div(id = "error",
                            div(
                                br(), tags$b("Error: "), span(id = "error_msg")
                            )
                        )
                    ),
                    width = 4
                )
            ),
            mainPanel(
                fluidRow(
                    uiOutput("ace_input")
                ),
                width = 6
            )
        ) # end sidebarLayout
    ) # end fluidPage

    server <- function(input, output, session) {
        observeEvent(input$presubmit, {
            ghrepo <- input[["prepop"]]
            updateTextInput(session, "ghrepo", value = ghrepo)
            descfile <- read_gh_file(ghrepo)
            title <- descfile[, "Title"]
            updateTextInput(session, "title", value = unname(title))
            description <- .parse_description(descfile)
            updateTextInput(session, "description", value = unname(description))
            url <- .dcf_parse_url(descfile)
            updateTextInput(session, "url", value = unname(url))
            disable(id = "presubmit")
        })
        observe({
            # check if all mandatory fields have a value
            mandatoryFilled <- vapply(
                fieldsMandatory,
                function(x) {
                    BiocBaseUtils::isScalarCharacter(input[[x]])
                },
                logical(1)
            )
            mandatoryFilled <- all(mandatoryFilled)
            # enable/disable the render button
            toggleState(id = "render", condition = mandatoryFilled)
        })
        formData <- reactive({
            data <- vapply(fieldsAll, function(x) input[[x]], character(1L))
            resform <- as.data.frame(t(data))
            if (identical(resform[["id"]], "abc123"))
                stop("Provide a valid and unique identifier (id)")
            resform
        })
        addFields <- c("wdate", "wtime", "wnpart")
        names(addFields) <- addFields
        addData <- reactive({
            adat <- lapply(addFields, function(x) input[[x]])
            adat[["wdate"]] <- paste(
                format(adat[["wdate"]][[1L]], "%B %d, %Y"), "-",
                format(adat[["wdate"]][[2L]], "%B %d, %Y")
            )
            adat[["wtime"]] <- format(adat[["wtime"]], "%H:%M")
            as.data.frame(adat)
        })
        output$ace_input <- renderUI({
            aceEditor(
                outputId = "code",
                value = "",
                height = "380px", fontSize = 18, mode = "r"
            )
        })
        observeEvent(input$render, {
            tryCatch({
                fdata <- formData()
                gh_comment <- .workshop_template(.data = fdata)
                updateAceEditor(
                    session,
                    "code",
                    value = gh_comment
                )
                # reset("form")
                hide("form")
                hide("error")
                show("render_msg")
                hide("presubmit")
                },
                error = function(e) {
                    html("error_msg", e$message)
                    show(id = "error", anim = TRUE, animType = "fade")
                },
                finally = {
                    enable("render")
                    hide("submit_msg")
                }
            )
        })
        observeEvent(input$post, {
            tryCatch({
                fdata <- formData()
                gh_comment <- .workshop_template(.data = fdata, sep = " ")
                ghrepo <- .ISSUE_GH_REPO
                issue_title <- paste0(
                    "[", fdata[["section"]], "] ", fdata[["title"]]
                )
                adata <- addData()
                init_gh_comment <- .workshop_template(
                    .data = adata, template = .INITIAL_GH_COMMENT_TEMP
                )
                response <- create_gh_issue(
                    ghrepo = ghrepo,
                    title = issue_title,
                    body = init_gh_comment
                )
                add_comment_gh_issue(
                    ghrepo = ghrepo,
                    body = gh_comment,
                    issue_number = response[["number"]]
                )
                hide("render_msg")
                show("thankyou_msg")
            }, error = function(e) {
                html("error_msg", e$message)
                show(id = "error", anim = TRUE, animType = "fade")
            })
        })
    }

    shinyApp(ui, server, ...)
}
