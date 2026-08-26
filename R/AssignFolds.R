#' Assigning CV Folds
#'
#' @description Assign spatial or non-spatial cross-validation folds to a dataset for species distribution modeling.
#'
#' @param data data frame containing species observation data and spatial coordinates
#' @param spatial logical; if TRUE (default), uses blockCV to create spatial block folds. If FALSE, assigns random folds
#' @param k integer; number of cross-validation folds to create (default is 10)
#' @param species character; optional column name for species presence/absence or abundance data to balance occurrences across spatial folds
#' @param coords character vector of length 2; column names representing x/longitude and y/latitude coordinates (default is c("lon", "lat"))
#' @param crs integer or character; Coordinate Reference System (CRS) for spatial coordinates (default is 4326 / WGS84)
#' @param fold_col character; column name in the returned data frame where assigned fold identifiers will be stored (default is "Folds")
#'
#' @importFrom sf st_as_sf
#' @importFrom blockCV cv_spatial
#'
#' @return data frame corresponding to input data with an additional column containing assigned fold letters
#' @export
#'
#' @examples
#'
AssignFolds <- function(data,
                        spatial = TRUE,
                        k = 10,
                        species = NULL,
                        coords = c("lon", "lat"),
                        crs = 3338,
                        fold_col = "Folds") {

  fold_labels <- LETTERS[1:k]

  if (spatial) {
    if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("blockCV", quietly = TRUE)) {
      stop("Packages 'sf' and 'blockCV' are required for spatial fold assignment.")
    }

    # Convert data frame to spatial sf object
    data_sf <- sf::st_as_sf(data, coords = coords, crs = crs)

    # Generate spatial blocking folds
    sb <- blockCV::cv_spatial(
      x = data_sf,
      column = species,
      k = k,
      selection = "random",
      progress = FALSE
    )

    # Assign spatial block IDs mapped to fold letters
    data[[fold_col]] <- fold_labels[sb$folds_ids]

  } else {
    # Generate random fold assignments
    random_folds <- rep(fold_labels, length.out = nrow(data))
    data[[fold_col]] <- sample(random_folds, size = nrow(data), replace = FALSE)
  }

  return(data)
}
