#' Extend soil profile
#'
#' @param df data frame with soil profile. As retrieved by \code{read_profile}
#' and go to sublist "profile"
#' @param x_end maximum soil depth to be used for extrapolation (or reduction)
#'
#' @return extended (or reduced) soil profile
#' @export
#'
#' @importFrom dplyr arrange bind_rows
#'
extend_soil_profile <- function(df, x_end) {

  shorten_profile <- FALSE

  if (x_end > min(df$x)) {
    if (any(x_end == df$x)) {
      shorten_profile <- TRUE
      return(df[which(x_end <= df$x), ])
    }
  } else {
    # Original x-Abstand ermitteln
    x_diff <- mean(diff(df$x))

    # Neue x-Werte erstellen basierend auf dem vorgegebenen Endwert für x
    new_x <- seq(min(df$x), x_end, by = x_diff)

    # Anzahl der neuen x-Werte bestimmen
    num_new_rows <- length(new_x)

    # Letzte gültige Werte für die Spalten extrahieren
    last_vals <- sapply(df, tail, 1)

    # Dataframe mit neuen Werten erstellen
    new_df <- data.frame(
      x = new_x,
      node_id = max(df$node_id) + 1:num_new_rows
    )

    # Spalten aus dem ursprünglichen DataFrame übernehmen
    for (col in names(df)) {
      if (col %in% c("x", "node_id")) next  # x und node_id überspringen
      new_df[[col]] <- rep(last_vals[[col]], num_new_rows)
    }

    # Kombinieren und Sortieren der Dataframes
    combined_df <- if(shorten_profile) {
      dplyr::bind_rows(df, new_df)
    } else {
      dplyr::bind_rows(df, new_df[-1,])
    }
    sorted_df <- dplyr::arrange(combined_df, node_id)

    return(sorted_df)
  }
}
