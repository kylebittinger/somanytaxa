#' Standard taxonomic ranks
#'
#' @export
taxonomic_ranks <- c(
  "Kingdom", "Phylum", "Class", "Order",
  "Family", "Genus", "Species")

#' Split taxonomic assignments into fixed taxonomic ranks
#'
#' @param assignments Character vector of taxonomic assignments
#' @param ranks Character vector of taxonomic ranks, used as column names in
#'   the result
#' @param split Pattern that separates the taxa in each assignment
#' @param remove A pattern passed to \code{stringr::str_remove}. For example,
#'   if taxa have a prefix denoting their rank, use this pattern to remove it.
#'   The taxa are not modified if this parameter is set to \code{NULL}
#' @return A \code{tibble} containing the taxa at each rank
#' @seealso \code{\link{taxonomic_ranks}}
#' @export
split_assignments <- function(assignments, ranks=taxonomic_ranks, split="; ",
                              remove = "^[kpcofgs]__") {
  n <- length(ranks)
  res <- stringr::str_split_fixed(assignments, pattern = split, n)
  colnames(res) <- ranks
  res <- tibble::as_tibble(res)

  if (!is.null(remove)) {
    res <- dplyr::mutate(res, dplyr::across(ranks, stringr::str_remove, remove))
  }

  dplyr::mutate(res, dplyr::across(ranks, dplyr::na_if, ""))
}


#' Reformat taxonomic assignments for presentation.
#'
#' @param assignments_df A data frame of taxonomic assignments.
#' @param upper_ranks The columns of \code{assignments_df} to use as the more
#'   general taxon
#' @param lower_ranks The columns of \code{assignments_df} to use as the more
#'   specific taxon
#' @param sep The separator to place between the more general and more specific
#'   taxa
#' @param incomplete_label A label to append to the output if the most specific
#'   taxon is \code{NA}. Nothing is appended if \code{incomplete_label} is set
#'   to \code{NULL}
#' @return A character vector of reformatted assignment labels.
#' @seealso \code{\link{split_assignments}}
#' @export
format_assignments <- function(assignments_df, upper_ranks = Kingdom:Phylum,
                               lower_ranks = Class:Genus, sep = " - ",
                               incomplete_label = " (unclassified)") {
  upper_df <- assignments_df %>%
    dplyr::select({{ upper_ranks }}) %>%
    # Put columns in reverse order to set up for coalesce
    dplyr::select(last_col():1)
  upper_taxon <- dplyr::coalesce(!!!upper_df)

  lower_df <- assignments_df %>%
    dplyr::select(dplyr::all_of({{ lower_ranks }})) %>%
    dplyr::select(last_col():1)
  lower_taxon <- dplyr::coalesce(!!!lower_df)

  lower_taxon_is_missing <- is.na(lower_taxon)
  formatted_taxon <- dplyr::if_else(
    lower_taxon_is_missing,
    upper_taxon,
    stringr::str_c(upper_taxon, sep, lower_taxon))

  if (!is.null(incomplete_label)) {
    assignment_is_incomplete <- is.na(dplyr::pull(lower_df, 1))
    formatted_taxon <- dplyr::if_else(
      assignment_is_incomplete,
      stringr::str_c(formatted_taxon, incomplete_label),
      formatted_taxon)
  }

  formatted_taxon
}
