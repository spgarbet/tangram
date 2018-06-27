#' Style Bundle for Lancet style
#'
#' List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
#' Next entries are keyed off returned types from function, and represent the type of a row.
#' The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
#' a function to process the intersection of those two types.
#'
#' @include cell-lancet.R
#' @keywords data
#' @export
#'
lancet <- list(
  Type        = hmisc_data_type,
  Numerical   = list(
                  Numerical   = summarize_spearman,
                  Categorical = summarize_kruskal_horz
            ),
  Categorical = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq
            ),
  Cell        = lancet_cell,
  Footnote    = "N is the number of non-missing value. ^1^Kruskal-Wallis. ^2^Pearson. ^3^Wilcoxon."
)
