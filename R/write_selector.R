#' Write SELECTOR.in
#'
#' @param selector list with imported SELECTOR.in \code{read_selector}
#' @param path path to SELECTOR.in for export
#' @export

write_selector <- function(selector, path) {

  basic_values <- selector$basic
  basic_names <- names(selector$basic)

  i_head <- which(basic_names == "Heading")
  i_units <- grep("Unit", basic_names)
  i_multi_start <- grep("lWat|lSnow|NMat", basic_names)
  i_multi_end <- c(i_multi_start[2:3] - 1, length(basic_names))
  n_multi <- 1+ i_multi_end - i_multi_start


    basic_txt <- c(
    "*** BLOCK A: BASIC INFORMATION *****************************************",
    basic_names[i_head],
    as.character(basic_values[i_head]),
    paste0(paste0(basic_names[i_units], collapse = "  "),
           "(indicated units are obligatory for all input data)",
           collapse = "  "),
    sapply(i_units, function(i) basic_values[i]) %>% as.character(),
    lapply(1:3, function(i) {
      c(basic_names[i_multi_start[i]:i_multi_end[i]] %>%
          stringr::str_pad(width = 10, side = "right") %>%
          paste0(collapse = "") %>%
          stringr::str_trim(),
        unlist(basic_values[i_multi_start[i]:i_multi_end[i]]) %>%
          to_fortran_truefalse() %>%
          stringr::str_pad(width = 10, side = "right") %>%
          paste0(collapse = "") %>%
          stringr::str_trim())
    }) %>% unlist())


    waterflow_values <- selector$waterflow[-length(selector$waterflow)]
    waterflow_names <- names(selector$waterflow[-length(selector$waterflow)])


    i_multi_start <- grep("MaxIt|TopInf|BotInf|hTab1|Model", waterflow_names)
    i_multi_end <- c(i_multi_start[2:length(i_multi_start)] - 1, length(waterflow_names))
    n_multi <- 1+ i_multi_end - i_multi_start

  waterflow_txt <- c(
    "*** BLOCK B: WATER FLOW INFORMATION ************************************",
    sapply(seq_along(i_multi_start), function(i) {

      additional_text <- if (waterflow_names[i_multi_start[i]] == "MaxIt") {
        "       (maximum number of iterations and tolerances)"
    } else {
          ""
      }

    c(paste0(paste0(stringr::str_pad(waterflow_names[i_multi_start[i]:i_multi_end[i]],
                                   width = 8,
                                   side = "right"), collapse = "") %>%
                    stringr::str_trim(),
           additional_text,
           collapse = ""),
    paste0(waterflow_values[i_multi_start[i]:i_multi_end[i]] %>%
             to_fortran_truefalse() %>%
             stringr::str_pad(width = 8,
                            side = "right"), collapse = "") %>%
        stringr::str_trim())}),
    stringr::str_pad(names(selector$waterflow$soil), width = 12, side = "right") %>%
      paste0(collapse = "") %>% stringr::str_trim(),
    sapply(seq_len(nrow(selector$waterflow$soil)), function(i) {
      stringr::str_pad(selector$waterflow$soil[i,], width = 12, side = "right") %>%
        paste0(collapse = "") %>% stringr::str_trim()
    })
    )

  time_values <- selector$time[-length(selector$time)]
  time_names <- names(selector$time)[-length(selector$time)]

  i_multi_start <- grep("^dt$|^tInit$|^lPrintD$", time_names)
  i_multi_end <- c(i_multi_start[2:3] - 1, length(time_names))
  n_multi <- 1+ i_multi_end - i_multi_start

  time_print_start <- seq(1,length(selector$time$TPrint),6)
  time_print_end <- c(time_print_start[2:length(time_print_start)] - 1,
                      length(selector$time$TPrint))

  time_txt <- c(
    "*** BLOCK C: TIME INFORMATION ******************************************",
    sapply(seq_along(i_multi_start), function(i) {
      width <- if(i == 3) {
        20} else {
          10
        }
      c(paste0(stringr::str_pad(time_names[i_multi_start[i]:i_multi_end[i]],
                                       width = width,
                                       side = "right"), collapse = "") %>%
                 stringr::str_trim(),
        paste0(time_values[i_multi_start[i]:i_multi_end[i]] %>%
                 to_fortran_truefalse() %>%
                 stringr::str_pad(width = width,
                                  side = "right"), collapse = "") %>%
          stringr::str_trim())}),
    "TPrint(1),TPrint(2),...,TPrint(MPL)",
    sapply(seq_along(time_print_start), function(i) {
      selector$time$TPrint[time_print_start[i]:time_print_end[i]] %>%
        stringr::str_pad(width = 11, side = "left") %>%
        paste0(collapse = " ")
    })
  )


  sel_ids <- grep(pattern = "transport|solute_", names(selector$solute), invert = TRUE)

  sol_names <- names(selector$solute)[sel_ids]
  sol_values <- selector$solute[sel_ids]


  i_multi_start <- grep("Epsi|iNonEqul|kTopSolute|tPulse", sol_names)
  i_multi_end <- c(i_multi_start[2:length(i_multi_start)] - 1, length(sol_names))
  n_multi <- 1+ i_multi_end - i_multi_start


  solutes_txt <- c(
    "*** BLOCK F: SOLUTE TRANSPORT INFORMATION *****************************************************",
    sapply(1:2, function(i) {
    sol_names_sel <- sol_names[i_multi_start[i]:i_multi_end[i]]

    c(paste0(sol_names[i_multi_start[i]:i_multi_end[i]] %>%
               #stringr::str_remove(pattern = "unknown[0-9][0-9]?") %>%
               stringr::str_pad(width = 12,
                            side = "right"), collapse = "") %>%
      stringr::str_trim(),
    paste0(sol_values[i_multi_start[i]:i_multi_end[i]] %>%
             to_fortran_truefalse() %>%
             stringr::str_pad(width = 12,
                              side = "right"), collapse = "") %>%
      stringr::str_trim())}),
    stringr::str_pad(names(selector$solute$transport), width = 11, side = "right") %>%
      paste0(collapse = " ") %>% stringr::str_trim(),
    sapply(seq_len(nrow(selector$solute$transport)), function(i) {
      stringr::str_pad(selector$solute$transport[i,], width = 11, side = "right") %>%
        paste0(collapse = " ") %>% stringr::str_trim()}),
    sapply(seq_len(sum(stringr::str_detect(names(selector$solute), "solute_"))), function(i) {
      solute_sel <- selector$solute[[sprintf("solute_%d", i)]]

      c(names(solute_sel$diffusion) %>%
          stringr::str_pad(width = 12, side = "right") %>%
          paste0(collapse = "") %>%
          stringr::str_trim() %>%
          stringr::str_c("                n-th solute"),
        solute_sel$diffusion %>%
          stringr::str_pad(width = 12, side = "right") %>%
          paste0(collapse = "") %>%
          stringr::str_trim(),
        names(solute_sel$reaction) %>%
          stringr::str_pad(width = 12, side = "right") %>%
          paste0(collapse = "") %>%
          stringr::str_trim(),
        sapply(seq_len(nrow(solute_sel$reaction)), function(i) {

          solute_sel$reaction[i,] %>%
            stringr::str_pad(width = 12, side = "right") %>%
            paste0(collapse = "") %>%
            stringr::str_trim()})
      )

    }) %>% as.character(),
    sapply(3:4, function(i) {
      sol_names_sel <- sol_names[i_multi_start[i]:i_multi_end[i]]

      if(any(stringr::str_detect(sol_names_sel, "unknown"))) {
        sol_names_sel <- sol_names_sel %>%
          stringr::str_remove(pattern = "unknown[0-9][0-9]?")
      }

      c(paste0(sol_names_sel  %>%
                 #stringr::str_remove(pattern = "unknown[0-9][0-9]?") %>%
                 stringr::str_pad(width = 12,
                                  side = "right"), collapse = "") %>%
          stringr::str_trim(),
        paste0(sol_values[i_multi_start[i]:i_multi_end[i]] %>%
                 to_fortran_truefalse() %>%
                 stringr::str_pad(width = 12,
                                  side = "right"), collapse = "") %>%
          stringr::str_trim())})
  )


  lines <- c("Pcp_File_Version=4",
             basic_txt,
             waterflow_txt,
             time_txt,
             solutes_txt)

  writeLines(lines, path)
}
