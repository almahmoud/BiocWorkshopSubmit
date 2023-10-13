.INITIAL_GH_COMMENT_TEMP <- "
**DO NOT INCLUDE REQUESTS IN THE FIRST COMMENT.**
**PLEASE POST THIS TEMPLATE UNCHANGED THEN FOLLOW ITS INSTRUCTIONS IN A NEW
COMMENT**

# General Notes
This repository serves as a mostly automated pipeline for deploying workshops
to the [Bioconductor Workshop](https://workshop.bioconductor.org).

# Bot Commands

Issues in this repository are monitored to by a bot. All commands need to be
written on the first line of the comment, in a single line.

## Example Docker Request

```
/request
  id='tidybioc2023'
  title='Tidy Transcriptomics'
  description='For Single-Cell RNA Sequencing Analyses'
  section='Smorgasbord 2023'
  source='https://github.com/tidytranscriptomics-workshops/bioc2023_tidytranscriptomics'
  docker='ghcr.io/tidytranscriptomics-workshops/bioc2023_tidytranscriptomics:latest'
```

Note. The example `/request` is on multiple lines for readability.

## Additional Workshop Information

Expected Number of Participants: {{wnpart}}

Workshop Date(s): {{wdate}}

Workshop Start Time (approx.): {{wtime}}

"
