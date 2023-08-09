#' @importFrom utils data
#' @keywords internal
#' @family internal
#' @importFrom dplyr arrange bind_rows case_when desc ends_with filter full_join group_by
#'             if_else mutate mutate_at mutate_if n pull rename rename_at row_number select slice
#'             starts_with transmute ungroup vars n_distinct union distinct
#'             summarise_at summarise coalesce bind_cols na_if any_of tibble first left_join
#'             between slice_tail across
#' @importFrom magrittr %>%
#' @importFrom rlang := abort arg_match as_function as_string call2 caller_env
#'             call_name current_env .data enexpr enquo eval_bare eval_tidy expr
#'             expr_interp expr_label f_lhs f_rhs inform
#'             is_bare_formula is_call is_character is_formula is_integerish
#'             is_logical is_quosure is_quosures is_symbol new_formula
#'             parse_expr parse_exprs quo quo_get_expr quo_is_call
#'             quo_is_missing quo_is_null quo_is_symbol quos quo_squash quo_text
#'             set_names sym syms type_of warn quo_set_env quo_get_env exprs
#' @importFrom utils capture.output str
#' @importFrom purrr map map2 map_chr map_lgl reduce walk keep map_if transpose
#'             flatten every modify_at modify_if reduce compose
#' @importFrom stringr str_c str_detect str_extract str_remove str_remove_all
#'             str_replace str_trim str_to_lower str_to_title str_to_upper str_glue
#'             str_to_sentence
#' @importFrom assertthat assert_that is.number on_failure<-
#' @importFrom lubridate as_datetime ceiling_date date days duration floor_date is.Date is.instant
#'             time_length %--% ymd ymd_hms weeks years hours minutes
#' @importFrom tidyr drop_na nest pivot_longer pivot_wider unnest unite
#' @importFrom tidyselect all_of contains vars_select
#' @importFrom hms as_hms
#' @importFrom lifecycle deprecate_warn deprecated deprecate_stop
#' @importFrom admiral derive_vars_dy derive_vars_merged convert_blanks_to_na convert_na_to_blanks
#' @importFrom admiraldev assert_logical_scalar assert_character_vector assert_vars
#'             assert_data_frame assert_character_scalar assert_numeric_vector assert_filter_cond
#'             assert_symbol assert_expr_list expect_dfs_equal
#' @importFrom metatools combine_supp
"_PACKAGE"

#' Tweak yml url
#' @param ... Arguments from pkgdown tweak_page
#' @keywords internal
tweak_rdrr_url <- function(...) {
  html <- ..1

  links <- xml2::xml_find_all(html, ".//a")
  if (length(links) == 0) {
    return(invisible())
  }

  hrefs <- xml2::xml_attr(links, "href")
  needs_tweak <- grepl("^https://rdrr.io/pkg/", hrefs) & xml2::url_parse(hrefs)$scheme == "https"

  fix_links <- function(x) {
    pattern <- "/pkg/(\\w+)/man/(\\w+)\\.html"

    matches <- stringr::str_match(x, pattern)
    package_name <- matches[2]
    function_name <- matches[3]

    if (!(grepl("^admiral", package_name) || package_name %in% c("matatools", "matacore"))) {
        return(x)
    }

    sprintf("https://pharmaverse.github.io/%s/pre-release/reference/%s.html", package_name, function_name)
  }

  if (any(needs_tweak)) {
    purrr::walk2(
      links[needs_tweak],
      purrr::map(hrefs[needs_tweak], fix_links),
      xml2::xml_set_attr,
      attr = "href"
    )
  }

  invisible()
}

#' onLoad function
#'
#' This function is called automatically during package loading.
#'
#' @param libname lib name
#' @param pkgname package name
#' @noRd
.onLoad <- function(libname, pkgname) { # nolint

  # Tweak page with special custom hook.
  setHook("UserHook::admiralci::tweak_page", tweak_rdrr_url)
}
