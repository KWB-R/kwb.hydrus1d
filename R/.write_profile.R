write_formatted_materials <- function(df) {
  # Hilfsfunktion zum Formatieren einer Zahl in wissenschaftlicher Notation
  format_number <- function(x) {
    sprintf("% .6e", x)
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


write_formatted_profile <- function(df) {
  # Hilfsfunktion zum Formatieren einer Zahl in wissenschaftlicher Notation
  format_number <- function(x) {
    sprintf("% .13e", x)
  }

  # Hilfsfunktion zum Formatieren einer Integer-Zahl mit fixer Länge von 5 Zeichen
  format_integer <- function(x) {
    sprintf("%5d", x)
  }

  # Data Frame konvertieren und formatieren
  formatted_df <- df
  formatted_df[] <- lapply(seq_along(df), function(i) {
    col <- df[[i]]
    if (is.numeric(col) && i %in% c(1,4,5)) {
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



write_profile <- function(profile,
                          path) {


  n_materials <- nrow(profile$mat_props)



  obsnodes <- stringr::str_pad(profile$obsnodes$n,width = 5,side = "left")

  if(profile$obsnodes$n > 0) {
    obsnodes <- c(obsnodes,
                  paste0(stringr::str_pad(profile$obsnodes$ids,width = 5,side = "left"),
                       collapse = ""))
  }

  paste0(stringr::str_pad(c(max(profile$profile$node_id),
                            1,
                            sum(stringr::str_detect(names(profile$profile),"conc")),
                            1),
                          width = 5,
                          side = "left"),
         " x",
        headers_profile <- names(profile$profile)[!names(profile$profile) %in% c("x", "node_id")]

        is_conc <- stringr::str_detect(headers_profile, "conc")

        if(sum(is_conc) > 1) {
          headers_profile <- c(headers_profile[!is_conc], "conc")
        }

        headers_profile <- c(headers_profile[1],
                             stringr::str_to_title(headers_profile[-1]))

  c("Pcp_File_Version=4",
    stringr::str_pad(n_materials,width = 5, side = "left"),
    write_formatted_materials(profile$mat_props),
    "headers",
    write_formatted_profile(profile$profile),
    obsnodes
  )


}
