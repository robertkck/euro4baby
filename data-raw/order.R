## code to prepare `order` dataset goes here

order_choices <- list(c(1,2), c(1,2,1), c(2, 1), c(2, 1, 2), c(1))
order_labels <- tibble(
  name = order_choices |>
    purrr::map(~paste("Elternteil", .x)) |>
    purrr::map_chr(~paste(.x, collapse = " - ")),
  value = order_choices |>
    purrr::map(~paste("Elternteil", .x))
)

usethis::use_data(order_choices, order_labels, overwrite = TRUE)
