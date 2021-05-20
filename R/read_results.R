#' Read Team Excel results
#'
#' @param path
#' @param skip_row
#'
#' @return dataframe
#' @export
#'
#' @examples
read_results <- function(path,
                         skip_row = 1) {
  readxl::read_excel(path,
                     skip = skip_row) %>%
    tidyr::pivot_longer(6:last_col(),
                        names_to = "step") %>%
    janitor::clean_names() %>%
    dplyr::rename(category = x1) %>%
    dplyr::select(-x4) %>%
    dplyr::mutate(
      category = stringr::str_replace(category, ":","")
    ) %>%
    tidyr::fill(category) %>%
    filter(!is.na(flow))
}


