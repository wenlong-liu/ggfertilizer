#' The built-in function of data preparation for mapping and plotting.
#'
#' @author Wenlong Liu
#' @import usfertilizer
#'
#'

data_preparation <- function(data = "us_fertilizer_county",
                         years,
                         nutrient,
                         fun = NULL,
                         facet = NULL,
                         farm_type = NULL,
                         input_type = NULL,
                         fun = NULL,
                         level = "county",
                         na.rm = TRUE,
                         ...)
{
  # if fertilizer dataset is not loaded, load it.
  if(!(exists(data) && is.data.frame(get(data)))){
    data(data)
  }

  # retrieve data from usfertilizer R package.
  # return a dataframe
  result = get_data(data = data,
                    year,
                    nutrient,
                    counties = NULL,
                    states = NULL,
                    farm_type = NULL,
                    input_type = NULL,
                    lat_max = NULL,
                    lat_min = NULL,
                    long_max = NULL,
                    long_min = NULL,
                    FIPSs = NULL,
                    overlap_state_county = TRUE,
                    combine_state_county = FALSE,
                    ...)

  # process the dataframe for ggplot2.
  # Key feature: facet, fun.

  if(is.null(level) | !(level %in% c("county", "state"))){
    stop("The spatial resolution must be specified.")
  }
  else if(level == "state"){
    #
    result = result %>%
      group_by(State) %>%
      summarise(Quantity = sum(Quantity, na.rm = na.rm)) %>%
      ungroup()
  }


}


