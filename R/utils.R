# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(x, ..., call. = FALSE)
{
  stop(sprintf(x, ...), call. = call.)
}
