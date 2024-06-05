paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  exe_dir = "<root_local>",
  model_name = "1a2a - Kopie",
  model_dir = "<exe_dir>/<model_name>",
  scenario = "1a2a",
  atmosphere = "<model_dir>/ATMOSPH.IN",
  a_level = "<model_dir>/A_LEVEL.out",
  profile = "<model_dir>/PROFILE.dat",
  obs_node = "<model_dir>/Obs_Node.out",
  t_level = "<model_dir>/T_LEVEL.out",
  runinf = "<model_dir>/Run_Inf.out",
  selector = "<model_dir>/SELECTOR.in",
  solute_id = "1",
  solute = "<model_dir>/solute<solute_id>.out",
  soil_data = "<extdata>/input-data/soil/soil_geolog.csv"
)


paths <- kwb.utils::resolve(paths_list)


atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)

atmos$data <- atmos$data %>%
  dplyr::mutate(dplyr::across(tidyselect::starts_with("X") | tidyselect::starts_with("c"),
                              ~ . / 1000))


atm_prep <- kwb.hydrus1d::write_atmosphere(atmos$data[1:13])
writeLines(atm_prep, paths$atmosphere)

kwb.hydrus1d::run_model(model_path = paths$model_dir)

obsnode <- kwb.hydrus1d::read_obsnode(paths$obs_node)
profile <- kwb.hydrus1d::read_profile(paths$profile)
alevel <- kwb.hydrus1d::read_alevel(paths$a_level)
tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)

obs_profile <- obsnode %>%  dplyr::left_join(profile %>%
                                             dplyr::select(c(node_id, x)),
                                             by = "node_id") %>%
  dplyr::mutate(x = forcats::as_factor(x))

obs_profile %>%
  dplyr::filter(grepl("^mass1", variable)) %>%
  dplyr::group_by(x) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::bind_rows(tibble::tibble(value = -10*sum(atmos$data$Prec*atmos$data$cTop),
                                  x = forcats::as_factor(0))) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = x,
                                         y = value)) +
  ggplot2::geom_col() +
  # ggplot2::ggplot(mapping = ggplot2::aes(x = time,
  #                                        y = value)) +
  # ggplot2::geom_line() +
  # ggplot2::facet_wrap(~ x, ncol = 1) +
  ggplot2::theme_bw()

solute <- kwb.hydrus1d::read_solute(paths$solute)


sum(atmos$data$Prec*atmos$data$cTop)

100*max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop)

# top_out <- solute$cv_top < 0
# top_in <- solute$cv_top > 0
#
# sum(solute$cv_top[top_in]*solute$c_top[top_in])
# sum(solute$cv_top[top_out]*solute$c_top[top_out])


solute_date <- flextreat.hydrus1d::aggregate_solute(solute,
                                                    col_aggr = "date")

solute_date$mass_top



