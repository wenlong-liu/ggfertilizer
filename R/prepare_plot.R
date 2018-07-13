#' @title Prepare data for visualization.
#' @description The built-in function of data preparation for mapping and plotting.
#' If you want to fetch the whole data, please prefer using get_data().
#' @name data_preparation
#' @param data the dataset to plot. default: us_fertilizer_county.
#' @param Year the temporal coverage of the data to visualize. See link for available period.
#' @param Nutrient the nutrient type of fertilizer from usfertilizer.  Has to be
#'     N, P or both (NULL).
#' @param Farm_Type the spatial source of fertilizer, should be farm, nonfarm or both.
#' @param Input_Type the input source of fertilizer, should be manure, fertilizer.
#' @param State the states that will show in the plot,default: all states.
#' @param County the counties that will show in the plot, default: all counties.
#' @param lat_max the maximum latitude of area of interest.
#' @param lat_min the minmum latitude of area of interest.
#' @param long_max the maximum longitude of area of interest.
#' @param long_min the minimum longitude of area of interest.
#' @param FIPSs FIPS numbers of interest, defalut: all the counties.
#' @param overlap_state_county Logic. If true, the function will overlaping
#'       the input of states and counties. If false, the function will return
#'       results either in the states or in the counties.
#' @param combine_state_county Logic. If true, the county will be changed into
#'       county, state, e.g. Wake, NC; If false, no changes.
#' @param fun the function to process data, not done yet.
#' @param annual_change to check if the data represent the annual change, default: FALSE.
#' @param level the spatial resolution of map, should be county or state, default county.
#' @param facet the facet to draw subplots.
#' @param na.rm How to deal with NA values. Default: TRUE.
#' @param ... extra parameters for \code{link(get_data)}
#' @author Wenlong Liu
#' @export data_preparation
#' @return A tibble with tidy data.
#' @keywords datasets tidydata
#' @seealso \code{link(data_overview), link(get_FIPS), link(get_data)}
#' @examples
#' require(ggfertilizer)
#' require(usfertilizer)
#' data("us_fertilizer_county")
#'
#' plot_data = data_preparation(data = us_fertilizer_county,
#'                              Year = 2003,
#'                               Nutrient = "N")
#' head(plot_data)

data_preparation <- function(data,
                             Year,
                             Nutrient,
                             County = NULL,
                             State = NULL,
                             Farm_Type = NULL,
                             Input_Type = NULL,
                             lat_max = NULL,
                             lat_min = NULL,
                             long_max = NULL,
                             long_min = NULL,
                             FIPSs = NULL,
                             overlap_state_county = TRUE,
                             combine_state_county = FALSE,
                             fun = NULL,
                             annual_change = NULL,
                             facet = NULL,
                             level = "county",
                             na.rm = TRUE,
                         ...)
{
  # retrieve data from usfertilizer R package.
  # return a dataframe
  result = get_data(data = data,
                    years = Year,
                    nutrient = Nutrient,
                    counties = County,
                    states = State,
                    farm_type = Farm_Type,
                    input_type = Input_Type,
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
    stop("The spatial resolution must be specified as either county or state.")
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

  # Todo:
  # manuplicate the dataset with user-defined functions.
  # such as sum, mean, median, min, max, etc.

  return(result)
}


