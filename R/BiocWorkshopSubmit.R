.TEMPLATE_FORM <- paste(
    '/request', 'id="{{id}}"', 'title="{{title}}"',
    'description="{{description}}"', 'section="{{section}}"',
    'startfile="{{startfile}}"', 'source="{{ghrepo}}"',
    'docker="{{url}}:{{tag}}"', sep = "\n"
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

appCSS <- ".mandatory_star { color: red; }"

#' Prepare a Bioconductor Workshop Submission
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs inlineCSS toggleState
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
                        id = "form",
                        textInput("id", mandatory("id"), "abc123"),
                        textInput("title", mandatory("Title")),
                        textInput(
                            "section", mandatory("Section"), "e.g. Bioc2023"
                        ),
                        textInput("description", "Description"),
                        textInput("ghrepo", mandatory("GitHub/Repository")),
                        textInput("startfile", "Start File", "README.md"),
                        textInput("url", mandatory("Container URL")),
                        textInput("tag", "Container Tag", "latest"),
                        actionButton("submit", "Submit", class = "btn-primary")
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
            as.data.frame(t(data))
        })
        output$ace_input <- renderUI({
            shinyAce::aceEditor(
                outputId = "code",
                value = "",
                height = "380px", fontSize = 18, mode = "r"
            )
        })
        observeEvent(input$submit, {
            fdata <- formData()
            shinyAce::updateAceEditor(
                session,
                "code",
                value = .workshop_template(.data = fdata)
            )
        })
    }

    shinyApp(ui, server, ...)
}
