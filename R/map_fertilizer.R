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
#' @importFrom viridis scale_fill_viridis
#' @importFrom scales percent
#' @export map_us_fertilizer
#'
#'
map_us_fertilizer <- function(years,
                              nutrient,
                              farm_type = NULL,
                              input_type = NULL,
                              states = NULL,
                              counties = NULL,
                              fun = NULL,
                              annual_change = FALSE,
                              level = "county",
                              facet = NULL,
                              coord_fix_ratio = 1.3,

                              # the colors for map.
                              trans = "identity",
                              fill_var_type = NULL,
                              viridis_palette = "viridis",
                              viridis_direction = -1,
                              viridis_end = 0.9,

                              # map projection arguments.
                              projection = NULL,
                              parapmeters = NULL,
                              orientation = NULL,
                              xlim = NULL,
                              ylim = NULL,
                              na.rm = TRUE,
                              default_theme = TRUE){

  # if fertilizer dataset is not loaded, load it.
  if(!(exists("us_fertilizer_county") && is.data.frame(get("us_fertilizer_county")))){
  data("us_fertilizer_county")
  }

  # Generate potential plots.
  states_shape <- map_data("state")

  counties_shape <- map_data("county") %>%
    mutate(polyname = paste(region,subregion, sep = ",")) %>%
    left_join(county.fips, by = "polyname") %>%
    mutate(FIPS = str_pad(fips, 5, pad = "0"))

  # Genearte fertilizer data for each state.
  nutrient_summary <- data_preparation(data = us_fertilizer_county,
                              nutrient = nutrient,
                              input_type = input_type,
                              farm_type = farm_type,
                              years = years,
                              # for the data for all states.
                              states = states,
                              counties = counties
                              )

  ## Guess at variable type for filling
  if (is.null(fill_var_type)) {
    if (is.numeric(nutrient_summary$Quantity)) {
      fill_var_type <- FALSE
    }else{
      fill_var_type <- TRUE
    }
  }else{
    if (fill_var_type %in% "discrete") {
      fill_var_type <- TRUE
    }else if (fill_var_type %in% "continuous") {
      fill_var_type <- FALSE
    }else{
      stop('fill_var_type must be either NULL, "discrete" or "continuous"')
    }
  }

  # generate base map.
  if (!(level %in% c("county", "state"))){
    stop("The map level for us fertilizer should be either county or state.")
  }
  else if(level == "state"){

    ## not working.

    # add nutrient data with state level region.
    nutrient_summary = nutrient_summary %>%
      group_by(State) %>%
      #rowwise() %>%
      mutate(region = tolower(state.name[grep(State, state.abb)])) %>%
      right_join(states_shape, by = "region")
    head(nutrient_summary)

    us_plot <- ggplot(nutrient_summary)+
      geom_sf(aes(fill = Quantity), color = NA)
  }
  else if(level == "county"){
    nutrient_summary = nutrient_summary %>%
      right_join(counties_shape, by = "FIPS")
    # plot data.
    us_plot <- ggplot(nutrient_summary)+
      geom_polygon(aes(x = long, y = lat,
                      fill = Quantity, group = group),
                   color = "grey", size = 0.05)+
      geom_polygon(aes(x = long, y = lat, group = group),
                   fill = NA, data = states_shape, color = "lightgrey",
                   size = 0.1)
  }

  # add fixed coordinates ratio.
  if(!is.null(coord_fix_ratio)){
    us_plot = us_plot +
      coord_fixed(coord_fix_ratio)
  }
  # add color pallet.
  # This section was highly inspired by xx in getTBinR.
  # https://github.com/seabbs/getTBinR/blob/master/R/map_tb_burden.R

  if (annual_change) {

    if (fill_var_type) {
      us_plot <- us_plot +
        scale_fill_viridis(end = viridis_end,
                           direction = viridis_direction, discrete = TRUE,
                           labels = percent,
                           option = viridis_palette)
    }else{
      us_plot <- us_plot +
        scale_fill_viridis(end = viridis_end, trans = trans,
                           direction = viridis_direction, discrete = FALSE,
                           labels = percent,
                           option = viridis_palette)
    }
  }else{

    if (fill_var_type) {
      us_plot <- us_plot +
        scale_fill_viridis(end = viridis_end,
                           direction = viridis_direction, discrete = TRUE,
                           option = viridis_palette)
    }else{
      us_plot <- us_plot +
        scale_fill_viridis(end = viridis_end, trans = trans,
                           direction = viridis_direction, discrete = FALSE,
                           option = viridis_palette)
    }

  }


  # add facets.
  if (length(facet) > 1){
    stop("Maximum one facet is supported.")
  }
  else if(length(facet) == 1)
  {
    us_plot <- us_plot +
      facet_wrap(reformulate(facet,"." ))
    print("facet length  = 1")
  }

  # add projection.
  if(!is.null(projection))
  {
    us_plot <- us_plot + coord_map(projection = projection,
                                   parapmeters = parapmeters,
                                   orientation = orientation,
                                   xlim = xlim,
                                   ylim = ylim,
                                   ...)
  }

  # check if use the default map theme.
  if (default_theme){
    us_plot <- us_plot +
      theme_map_fertilizer()
  }

  return(us_plot)
}


#' Create base map theme for further use.
#' Adapted from theme maps.
#' https://www.rdocumentation.org/packages/ggthemes/versions/3.5.0/source
#'
#'
#'

theme_map_fertilizer <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          plot.title = element_text(size = 11, color = "#1c5074", hjust = 0, vjust = 2, face = "bold"),
          plot.subtitle = element_text(size = 8, color = "#3474A2", hjust = 0, vjust = 0),
          plot.margin = margin(1, 1, 1, 1, 'cm'),
          legend.direction = "vertical",
          legend.position = "right",
          legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))
}

