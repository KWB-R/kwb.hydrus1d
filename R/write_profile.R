#' Helper function: write formatted soil materials
#'
#' @param df df with soil materials as retrieved by \code{read_profile} and
#' sublist "mat_props"
#' @return formatted soil materials
#' @keywords internal
#'
write_formatted_materials <- function(df) {
  # Hilfsfunktion zum Formatieren einer Zahl in wissenschaftlicher Notation
  format_number <- function(x) {
    formatted <- sprintf("% .6e", x)
    formatted <- gsub("e([+-])([0-9])", "e\\10\\2", formatted)  # Hinzufügen von führenden Nullen
    return(formatted)
  }
  # Hilfsfunktion zum Formatieren einer Integer-Zahl mit fixer Länge von 5 Zeichen
  format_integer <- function(x) {
    sprintf("%5d", x)
  }

  # Data Frame konvertieren und formatieren
  formatted_df <- df
  formatted_df[] <- lapply(seq_along(df), function(i) {
    col <- df[[i]]
    if (is.numeric(col) && i == 1) {
      sapply(col, format_integer)  # Erste Spalte als Integer formatieren
    } else if (is.numeric(col)) {
      sapply(col, format_number)   # Restliche Spalten in wissenschaftlicher Notation formatieren
    } else {
      col
    }
  })

  # Formatierte Zeilen erstellen
  apply(formatted_df, 1, function(row) {
    paste(row, collapse = " ")
  })

}


#' Helper function: write formatted soil profile
#'
#' @param df df with soil profile as retrieved by \code{read_profile} and sublist
#' profile
#'
#' @return formatted soil materials
#' @keywords internal
write_formatted_profile <- function(df) {
  # Hilfsfunktion zum Formatieren einer Zahl in wissenschaftlicher Notation
  format_number <- function(x) {
    formatted <- sprintf("% .6e", x)
    formatted <- gsub("e([+-])([0-9])", "e\\10\\2", formatted)  # Hinzufügen von führenden Nullen
    return(formatted)
  }

  # Hilfsfunktion zum Formatieren einer Integer-Zahl mit fixer Länge von 5 Zeichen
  format_integer <- function(x) {
    sprintf("%5d", x)
  }

  # Data Frame konvertieren und formatieren
  formatted_df <- df
  formatted_df[] <- lapply(seq_along(df), function(i) {
    col <- df[[i]]
    if (is.numeric(col) && i %in% c(1)) {
      sapply(col,  function(x)   sprintf("%5d", x))  # Erste Spalte als Integer formatieren
    }
      else if (is.numeric(col) && i %in% c(4,5)) {
      sapply(col, function(x) sprintf("%4d", x))
    } else if (is.numeric(col)) {
      sapply(col, format_number)   # Restliche Spalten in wissenschaftlicher Notation formatieren
    } else {
      col
    }
  })

  # Formatierte Zeilen erstellen
  apply(formatted_df, 1, function(row) {
    paste(row, collapse = " ")
  })

}



#' Write PROFILE.dat
#'
#' @param profile profile in structure as imported with \code{read_profile}
#' @param path path to export PROFILE.dat
#'
#' @return writes PROFILE.dat to user specified path
#' @export
#'
#' @importFrom stringr str_pad str_detect str_to_title
write_profile <- function(profile,
                          path) {

  stopifnot(length(path) > 0)

  n_materials <- length(unique(profile$profile$mat))

  if(nrow(profile$mat_props) != length(unique(profile$profile$mat)) |  min(profile$profile$x) != min(profile$mat_props$mat_depth)) {
    profile$mat_props <- tibble::tibble(mat_id = unique(profile$profile$mat),
                   mat_depth = if(n_materials > 1) {
                     c(rep(0, n_materials-1), min(profile$profile$x)
                       )} else {min(profile$profile$x)},
                   mat_prop3 = 1,
                   mat_prop4 = 1)
  }


  obsnodes <- if(profile$obsnodes$n > 0) {
      if(max(profile$obsnodes$ids) > max(profile$profile$node_id)) {
        valid_ids <- which(profile$obsnodes$ids <= max(profile$profile$node_id))
        profile$obsnodes$ids <- profile$obsnodes$ids[valid_ids]
        profile$obsnodes$n <- length(profile$obsnodes$ids)
      }

    stringr::str_pad(profile$obsnodes$n,width = 5,side = "left")
    }

  obsnodes <- if(is.null(profile$obsnodes$n) | profile$obsnodes$n == 0) {
    obsnodes <- stringr::str_pad(0,width = 5,side = "left")
  } else {
    stringr::str_pad(profile$obsnodes$n,width = 5,side = "left")
  }

  obsnodes <- c(obsnodes,
                paste0(stringr::str_pad(profile$obsnodes$ids,width = 5,side = "left"),
                       collapse = ""))


  headers_profile_base <- c(stringr::str_pad(c(max(profile$profile$node_id),
                            1,
                            sum(stringr::str_detect(names(profile$profile),"conc")),
                            1),
                          width = 5,
                          side = "left"),
                          " x") %>% paste0(collapse = "")

        headers_profile <- names(profile$profile)[!names(profile$profile) %in% c("x", "node_id")]

        is_conc <- stringr::str_detect(headers_profile, "conc")

        if(sum(is_conc) > 1) {
          headers_profile <- c(headers_profile[!is_conc], "conc")
        }

        headers_profile[-1] <- stringr::str_to_title(headers_profile[-1])

        headers_profile <- c(headers_profile_base,
                             stringr::str_pad(headers_profile[1], width = 10, side = "left"),
                             stringr::str_pad(headers_profile[2], width = 9, side = "left"),
                             stringr::str_pad(headers_profile[3], width = 5, side = "left"),
                             stringr::str_pad(headers_profile[4], width = 10, side = "left"),
                             stringr::str_pad(headers_profile[5], width = 14, side = "left"),
                             stringr::str_pad(headers_profile[6], width = 15, side = "left"),
                             stringr::str_pad(headers_profile[7], width = 15, side = "left"),
                             stringr::str_pad(headers_profile[8], width = 14, side = "left"),
                             if(length(headers_profile[9]) > 0) {
                              stringr::str_pad(headers_profile[9], width = 14, side = "left") %>%
                               stringr::str_pad(width = 15, side = "right")
                             }) %>% paste0(collapse = "")

  lines_to_write <- c("Pcp_File_Version=4",
    stringr::str_pad(n_materials,width = 5, side = "left"),
    write_formatted_materials(profile$mat_props),
    headers_profile,
    write_formatted_profile(profile$profile),
    obsnodes
  )

  writeLines(lines_to_write, path)
}
