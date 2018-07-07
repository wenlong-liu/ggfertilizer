#' visualize the fertilizer data in the whole contiouse U.S.
#'
#' @param year the temporal coverage of the data to visualize. See link for available period.
#' @param nutrient the nutrient type of fertilizer from usfertilizer.  Has to be
#' N, P or both.
#' @param facet the facet to draw subplots.
#' @param farm_type the spatial source of fertilizer, should be farm, nonfarm or both.
#' @param input_type the input source of fertilizer, should be manure, fertilizer.
#' @param fun the function to process data.
#' @param level the spatial resolution of map, should be county or state, default county.
#' @param projection the projection parameters for spatial projection.
#' @import ggplot2 maps usfertilizer
#' @import mapproj
#' @importFrom stringr str_pad
#' @importFrom ggthemes theme_map
#' @export map_us_fertilizer
#'
#'
map_us_fertilizer <- function(years,
                              nutrient,
                              farm_type = NULL,
                              input_type = NULL,
                              fun = NULL,
                              level = "county",
                              # map projection arguments.
                              projection = NULL,
                              parapmeters = NULL,
                              orientation = NULL,
                              xlim = NULL,
                              ylim = NULL){

  # if fertilizer dataset is not loaded, load it.
  if(!(exists("us_fertilizer_county") && is.data.frame(get("us_fertilizer_county")))){
  data("us_fertilizer_county")
  }

  # Generate potential plots.
  states <- map_data("state")

  counties <- map_data("county") %>%
    mutate(polyname = paste(region,subregion, sep = ",")) %>%
    left_join(county.fips, by = "polyname")

  # Genearte fertilizer data for each state.
  nutrient_summary <- data_preparation(
                              nutrient = nutrient,
                              input_type = input_type,
                              farm_type = farm_type,
                              years = years,
                              # for the data for all states.
                              states = NULL,
                              counties = NULL)
  if (level == "state"){

    nutrient_summary = nutrient_summary %>%
      group_by(State) %>%
      summarise(N_application = sum(Quantity, na.rm = TRUE)) %>%
      rowwise() %>%
      mutate(region = tolower(state.name[grep(State, state.abb)])) %>%
      right_join(states, by = "region")

    # plot the data.
    us_plot = nutrient_summary %>%
      ggplot() +
      geom_polygon(aes(x = long, y = lat,
                       fill = N_application, group = group),
                   color = "black", size = 0.3) +
      #scale_fill_gradient(palette = colors_palette)+
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      coord_fixed(1.3) +
      ggthemes::theme_map() +
      theme(legend.position = "bottom")
  }

  else{
    counties <- map_data("county") %>%
      mutate(polyname = paste(region,subregion, sep = ",")) %>%
      left_join(county.fips, by = "polyname") %>%
      mutate(FIPS = str_pad(fips, 5, pad = "0"))

    nutrient_summary = nutrient_summary%>%
      right_join(counties, by = "FIPS") %>%
      mutate(N_application = Quantity)
    # plot the data.
    us_plot = nutrient_summary %>%
      ggplot() +
      geom_polygon(aes(x = long, y = lat,
                       fill = N_application, group = group),
                   color = "grey", size = 0.1) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   fill = NA, data = states, color = "lightgrey")+
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      coord_fixed(1.3) +
      ggthemes::theme_map() +
      theme(legend.position = "bottom")
  }

  # add projection.
  if(is.null(projection) & !is.null(any(parapmeters, orientation, xlim, ylim)))
  {
    stop("Projection type should be specified first.")
    }
  else
    {
    us_plot = us_plot + coord_map(projection = projection,
                                  parapmeters = parapmeters,
                                  orientation = orientation,
                                  xlim = xlim,
                                  ylim = ylim,
                                  ...)
    }

  return(us_plot)
}
