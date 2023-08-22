# Code adapted from
# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

.TEMPLATE_FORM <- paste(
    '/request', 'id="{{id}}"', 'title="{{title}}"',
    'description="{{description}}"', 'section="{{section}}"',
    'startfile="{{startfile}}"', 'source="{{ghrepo}}"',
    'docker="{{url}}:{{tag}}"'
)

.workshop_template <- function(.data) {
    whisker::whisker.render(.TEMPLATE_FORM, data = .data)
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

#' Prepare a Bioconductor Workshop Submission
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs inlineCSS toggleState hidden hide reset show
#'   disable enable html
#' @importFrom shinyAce aceEditor updateAceEditor
#'
#' @export
BiocWorkshopSubmit <- function(...) {
    fieldsMandatory <- c("id", "title", "section", "ghrepo", "url")
    fieldsAll <- c("id", "title", "description", "section",
        "startfile", "ghrepo", "url", "tag")
    ui <- fluidPage(
        useShinyjs(),
        inlineCSS(appCSS),
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
                        actionButton("submit", "Render", class = "btn-primary")
                    ),
                    hidden(
                        div(
                            id = "thankyou_msg",
                            h3("Your response was submitted successfully!")
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
                    width = 5
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
            # enable/disable the submit button
            toggleState(id = "submit", condition = mandatoryFilled)
        })
        formData <- reactive({
            data <- vapply(fieldsAll, function(x) input[[x]], character(1L))
            resform <- as.data.frame(t(data))
            if (identical(resform[["id"]], "abc123"))
                stop("Provide a valid and unique identifier (id)")
            resform
        })
        output$ace_input <- renderUI({
            aceEditor(
                outputId = "code",
                value = "",
                height = "380px", fontSize = 18, mode = "r"
            )
        })
        observeEvent(input$submit, {
            tryCatch({
                fdata <- formData()
                updateAceEditor(
                    session,
                    "code",
                    value = .workshop_template(.data = fdata)
                )
                reset("form")
                hide("form")
                hide("error")
                show("thankyou_msg")
                },
                error = function(e) {
                    html("error_msg", e$message)
                    show(id = "error", anim = TRUE, animType = "fade")
                },
                finally = {
                    enable("submit")
                    hide("submit_msg")
                }
            )
        })
    }

    shinyApp(ui, server, ...)
}
