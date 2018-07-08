#' The built-in function of data preparation for mapping and plotting.
#'
#' @author Wenlong Liu
#' @import usfertilizer
#'
#'

data_preparation <- function(data,
                             years,
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
                             fun = NULL,
                             facet = NULL,
                             level = "county",
                             na.rm = TRUE,
                         ...)
{
  # retrieve data from usfertilizer R package.
  # return a dataframe
  result = get_data(data = data,
                    years = years,
                    nutrient = nutrient,
                    counties = counties,
                    states = states,
                    farm_type = farm_type,
                    input_type = input_type,
                    lat_max = lat_max,
                    lat_min = lat_min,
                    long_max = long_max,
                    long_min = long_min,
                    FIPSs = FIPSs,
                    overlap_state_county = TRUE,
                    combine_state_county = FALSE,
                    ...)


  # check the level of data.
  # state level will sum up the county level data.
  if(is.null(level) | !(level %in% c("county", "state"))){
    stop("The spatial resolution must be specified.")
  }
  else if(level == "state"){
    # summarise within all the states.
    if (is.null(facet)){
      result = result %>%
        group_by_("State") %>%
        summarise(Quantity = sum(Quantity, na.rm = na.rm))
    }
    else {
      result = result %>%
        group_by_("State", facet) %>%
        summarise(Quantity = sum(Quantity, na.rm = na.rm))
    }
  }

  return(result)

}


