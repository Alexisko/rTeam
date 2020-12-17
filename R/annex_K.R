#' Add annex K lines at the end of modules and imp_exp flows
#'
#' @param path Path where your files are located
#'
#' @return
#' @export
#'
#' @examples
#' path <- "C:/Users/fr103343/Documents/R/Projects/annex 4/conversion/2020-12-10/conversion"
#' annex_K(path)
annex_K <- function(path) {

  list_files <- function(path) {
    list.files(
      path,
      full.names = TRUE) %>%
      purrr::discard(stringr::str_detect(., "imp_exp_")) %>%
      purrr::discard(. == paste0(path, "/annexe K")
      )
  }

  read_module <- function(file_name) {
    readr::read_delim(
      file_name,
      delim = "|",
      col_names = FALSE,
      skip_empty_rows = FALSE
    )
  }

  add_lines <- function(module) {
    module <- module %>%
      dplyr::mutate(previous = dplyr::lag(X1, default = "no")) %>%
      dplyr::filter(!(is.na(X1)&(is.na(previous)))) %>%
      dplyr::select(-previous) %>%
      tibble::rownames_to_column("line_nb") %>%
      dplyr::mutate(line_nb = as.numeric(line_nb))

    reminder_line <- module %>%
      dplyr::filter(X1 == "[REMINDER]") %>%
      dplyr::pull(line_nb)

    insert_line <- reminder_line - 1

    module <- module %>%
      dplyr::mutate(line_nb = ifelse(line_nb >= insert_line, line_nb + 4, line_nb)) %>%
      dplyr::add_row(outputs %>%
                       dplyr::mutate(line_nb = insert_line:(insert_line + 3))) %>%
      dplyr::arrange(line_nb) %>%
      dplyr::select(-line_nb)

    module
  }

  write_module <- function(module,
                           file_name) {
    utils::write.table(
      module,
      file_name,
      # eol = "\r\n",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      na = ""
    )
  }


  flows_copy <- function(path) {
    list.files(
      path,
      full.names = TRUE
    ) %>%
      purrr::keep(stringr::str_detect(., "imp")) %>%
      file.copy(paste0(path, "/annexe K"))


    imp_exp_flows <- readr::read_delim(
      paste0(path, "/imp_exp_flows.txt"),
      delim = ";",
      col_names = FALSE
    ) %>%
      dplyr::add_row(flows)

    utils::write.table(
      imp_exp_flows,
      paste0(path, "/annexe K/imp_exp_flows.txt"),
      # eol = "\r\n",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      na = ""
    )
  }

  path <- ifelse(stringr::str_ends(path, "/"),
                 stringr::str_sub(path, end = -2L),
                 path)

  dir.create(paste0(path, "/annexe K/"))

  file_list <- list_files(path)

  file_list %>%
    purrr::map(~read_module(.)) %>%
    purrr::map(add_lines) %>%
    purrr::walk2(
      file_list %>%
        stringr::str_extract("[^\\/]+$"),
      ~write_module(
        .x,
        paste0(path,
               "/annexe K/",
               .y))
    )

  flows_copy(path)
}
