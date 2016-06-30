#' Calculate total cover per species in vegetation transects
#'
#' Calculate total cover per species in each transect, layer within transect, etc.
#' Optionally, the function can also calculate length of bare ground (as ground not covered
#' by any plant species).
#'
#' @export
#' @author F Rodriguez-Sanchez
#' @importFrom plyr ddply
#' @importFrom gtools odd even
#' @importFrom tidyr gather
#' @importFrom dplyr arrange_
#' @param df A dataframe with one column per recorded species, plus additional columns
#' to identify transects (and possibly sections within transects, shrub or tree layers, etc).
#' For each species, introduce initial and ending distances every time the species appears along
#' the transect (see example).
#' @param split.cols A character vector with the names of the variables in \code{df}
#' that identify transects or any other structure of the data (e.g. sections within transects,
#' shrub or tree layers, etc.)
#' @param first.spcol Integer. Number of the first column containing species cover data.
#' @param last.spcol Integer. Number of the last column containing species cover data
#' (last column in \code{df} by default).
#' @param check.incremental Logical (default is \code{TRUE}). Check that cover data are
#' sorted incrementally? Must be \code{TRUE} if calculating bare cover with this function.
#' @param bare Logical. Calculate bare cover? Default is \code{TRUE}.
#' @param tr.length Numeric. Total transect length (e.g. in meters).
#' @param precision Numeric. Precision of species cover measurements. Only used to calculate
#' bare cover by exclusion.
#' @param prop Logical. If \code{TRUE}, results are expressed as proportion
#' of \code{tr.length} covered by each species. If \code{FALSE} (the default), results represent
#' linear absolute length covered by each species.
#' @param long.format Logical. Produce the output dataframe in long format? Default is wide format.
#' See \code{\link[tidyr]{gather}}.
#' @param show.progress Logical. Show progress bar?
#' @return A dataframe with total cover values per species and transect (plus optionally bare ground).
#' @examples
#' df <- data.frame(transect = sort(rep(c(1,2,3), 20)),
#'                  section = rep(1, 60),
#'                  species1 = c(replicate(3, sort(round(runif(20, 0, 100), digits = 1)))),
#'                  species2 = c(sort(round(runif(8, 0, 100), digits = 1)), rep(NA, times = 60 - 8)))
#' head(df)
#' calculate_cover(df, split.cols = "transect", first.spcol = 3,
#' tr.length = 100, check.incremental = FALSE)
#' calculate_cover(df, split.cols = "transect", first.spcol = 3, tr.length = 100,
#' check.incremental = FALSE, long.format = TRUE)

calculate_cover <- function(df, split.cols,
                            first.spcol, last.spcol = ncol(df),
                            tr.length,
                            bare = FALSE, precision,
                            check.incremental = TRUE,
                            prop = FALSE, long.format = FALSE,
                            show.progress = TRUE){


  out <- plyr::ddply(df, split.cols, calculate_cover_transect,
                     # calculate_cover_transect args below:
                     id.col = split.cols[1],
                     first.col = first.spcol, last.col = last.spcol,
                     check.increm = check.incremental,
                     calc.bare = bare, tr.len = tr.length, precis = precision,
                     # back to ddply args:
                     .progress = ifelse(show.progress, "text", "none"))


  if (prop){
    out[, (length(split.cols)+1):ncol(out)] <- out[, (length(split.cols)+1):ncol(out)] / tr.length
  }

  if (long.format){
    out.long <- tidyr::gather(out, "species", "cover", (length(split.cols)+1):ncol(out))
    out <- dplyr::arrange_(out.long, .dots = split.cols)
  }

  out

}



calculate_cover_transect <- function(df.split, id.col,
                                     first.col, last.col,
                                     check.increm,
                                     calc.bare, tr.len, precis) {


  fdata <- df.split[, first.col:last.col]  # retain cover data only; exclude other columns

  ### check that all columns are numeric
  if (!all(sapply(fdata, is.numeric)))
    stop("All columns with cover data must be numeric.")


  ### check that number of values is even for each column

  nvals <- apply(fdata, 2, function(x) sum(!is.na(x)))
  if (any(gtools::odd(nvals))){
    stop("\nIn transect ", as.character(df.split[1, id.col]),
         ", the following taxa do not have an even number of values:\n",
         paste(names(fdata[which(gtools::odd(nvals))]), collapse = " "),
         "\nPlease check your data.")
  }



  ### check that values are always incrementing positively

  if (calc.bare) check.increm <- TRUE  # bare cover calculations require cover data sorted incrementally

  if (check.increm){

    increm <- apply(fdata, 2, is.unsorted, na.rm = TRUE, strictly = TRUE)

    if (any(increm)){
      stop("\nIn transect ", as.character(df.split[1, id.col]),
           ", cover values for the following taxa seem not to be sorted incrementally:\n",
           paste(names(fdata[which(increm)]), collapse = " "),
           "\nPlease check your data.")
    }

  }



  ### Now add up values to get total cover per species

  cover.allsp <- apply(fdata, 2, addup)


  ## Calculate bare cover

  if (calc.bare){

    cover.sp <- apply(fdata, 2, covered_species, precis, tr.len)
    bare.c <- apply(cover.sp, 1, function(x) ifelse(isTRUE(any(x)), 0, precis))
    bare.cover <- round(sum(bare.c)/precis) * precis


    cover.allsp <- c(cover.allsp, bare = bare.cover)

  } else cover.allsp

  ## Check that total cover values are < total transect length
  if (any(cover.allsp > tr.len)){
    stop("\nIn transect ", as.character(df.split[1, id.col]),
         ", total cover for ",
         paste(names(cover.allsp[which(cover.allsp > tr.len)]), collapse = " "),
         " exceeds the transect length (", tr.len, ")")
  }

  return(cover.allsp)

}



addup <- function(species){

  species <- as.matrix(species)
  sum(species[even(row(species))] - species[odd(row(species))], na.rm = TRUE)
}


covered_species <- function(species, precision, tr.length){

  species <- as.matrix(species)
  mat <- matrix(c(species[odd(row(species))], species[even(row(species))]),
                nrow(species)/2, ncol = 2)

  spcover <- as.character(unlist(apply(mat, 1, function(x) {
    if (!is.na(x[1])) {seq(from = x[1], to = x[2], by = precision)}
  })))

  # to avoid overestimating species cover, eliminate all starting points
  spcover2 <- spcover[!spcover %in% as.character(mat[, 1])]

  allvals <- seq(precision, tr.length, by = precision)

  as.character(allvals) %in% spcover2

}
