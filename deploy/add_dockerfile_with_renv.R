golem::add_dockerfile_with_renv(
  port = 3838,
  output_dir = here::here("deploy"),
  dockerfile_cmd = "R -e \"options('shiny.port'=3838,shiny.host='0.0.0.0');library(euro4baby);run_app()\""
)
