#' @title Map the fertilizer data in U.S.A.
#' @name map_us_fertilizer
#' @description  visualize the fertilizer data in the whole contiouse U.S.
#' @param data the dataset to plot. default: us_fertilizer_county.
#' @param Year the temporal coverage of the data to visualize. See link for available period.
#' @param Nutrient the nutrient type of fertilizer from usfertilizer.  Has to be
#'     N, P or both (NULL).
#' @param Farm_Type the spatial source of fertilizer, should be farm, nonfarm or both.
#' @param Input_Type the input source of fertilizer, should be manure, fertilizer.
#' @param State the states that will show in the plot,default: all states.
#' @param County the counties that will show in the plot, default: all counties.
#' @param FIPSs the FIPS code for counties in USA.
#' @param fun the function to process data, not done yet.
#' @param annual_change to check if the data represent the annual change, default: FALSE.
#' @param level the spatial resolution of map, should be county or state, default county.
#' @param facet the facet to draw subplots.
#' @param trans the transformation parameter for scale_fill_viridis.
#' @param fill_var_type the type of data to visualize, default: Null.
#' @param viridis_palette the palette type for viridis,
#'     options:  “Viridis”,“magma”, “plasma”, "cividis", and “inferno.”
#' @param viridis_direction 	Sets the order of colors in the scale.
#'     If 1, the default, colors are ordered from darkest to lightest.
#'     If -1, the order of colors is reversed.
#' @param viridis_end	The (corrected) hue in [0,1] at which the viridis colormap ends.
#' @param projection the projection name for spatial projection.
#' @param parameters parameters for map projection.
#' @param orientation orientation for map projection.
#' @param xlim x axis limits for map project.
#' @param ylim y axis limits for map project.
#' @param add_north logic.  If TRUE, the ggplot object will include a states_shape df to
#'     draw north symbols and scale bars.
#' @param na.rm How to deal with NA values. Default: TRUE.
#' @param coord_fix_ratio the ratio for fixed coordinate system, default: 1.3.
#' @param map_theme the map theme for dataset. Default: theme_map_fertilizer()
#' @import ggplot2 maps usfertilizer
#' @import mapproj
#' @importFrom stringr str_pad
#' @importFrom viridis scale_fill_viridis
#' @importFrom scales percent comma
#' @export map_us_fertilizer
#' @return a ggplot object.
#' @seealso \code{link(data_preparation)}
#' @keywords map
#' @examples
#' require(usfertilizer)
#' require(ggfertilizer)
#' data("us_fertilizer_county")
#'
#' us_plot <- map_us_fertilizer(
#' data = us_fertilizer_county,
#' Year = 2010, Nutrient = "N",
#' level = "county",  Farm_Type = "farm",
#' Input_Type = "Fertilizer",
#' map_theme = theme_map_fertilizer(base_size = 12),
#' viridis_palette = "inferno")
#'
#' us_plot
map_us_fertilizer <- function(data = "us_fertilizer_county",
                              # parameters for data preparation.
                              Year,
                              Nutrient,
                              Farm_Type = NULL,
                              Input_Type = NULL,
                              State = NULL,
                              County = NULL,
                              FIPSs = NULL,
                              fun = NULL,
                              annual_change = FALSE,
                              level = "county",
                              facet = NULL,

                              # the colors for map.
                              trans = "identity",
                              fill_var_type = NULL,
                              viridis_palette = "viridis",
                              viridis_direction = -1,
                              viridis_end = 0.9,

                              # map projection arguments.
                              projection = NULL,
                              parameters = NULL,
                              orientation = NULL,
                              xlim = NULL,
                              ylim = NULL,

                              # others
                              add_north = FALSE,
                              na.rm = TRUE,
                              coord_fix_ratio = 1.3,
                              map_theme = theme_map_fertilizer()){

  # Generate shapes
  states_shape <- map_data("state")
  names(state.name) = state.abb

  counties_shape <- map_data("county") %>%
    mutate(polyname = paste(region,subregion, sep = ",")) %>%
    left_join(county.fips, by = "polyname") %>%
    mutate(FIPS = str_pad(fips, 5, pad = "0"))

  if (!is.null(State)){
    states_shape <- states_shape %>%
      filter(region %in% tolower(state.name[State]))
  }

  # check facet.
  if (length(facet) > 1){
  stop("Maximum one facet is supported.")
  }
  else if(!is.null(facet) & any(length(facet) == 0 | !(facet %in% colnames(data)) )){
  stop("Facet should fall in the colnames of dataset.")
  }

  # Genearte fertilizer data for each state.
  nutrient_summary <- data_preparation(data = data,
                              Nutrient = Nutrient,
                              Input_Type = Input_Type,
                              Farm_Type = Farm_Type,
                              Year = Year,
                              # for the data for all states.
                              State = State,
                              County = County,
                              FIPSs = FIPSs,
                              facet = facet,
                              level = level
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

    # add nutrient data with state level region.
    add_state <- function(data, state_shape, state.name){
      data = data %>%
        mutate(region = tolower(state.name[State])) %>%
        right_join(states_shape, by = "region")
    }

    # there is warning which is suppressed. See below for details.
    # https://github.com/tidyverse/dplyr/issues/2688
    nutrient_summary = suppressWarnings(
                         add_state(nutrient_summary,
                                   state_shape,
                                   state.name)
                        )


    us_plot <- ggplot(nutrient_summary)+
      geom_polygon(aes(x = long, y = lat,
                       fill = Quantity, group = group),
                   color = "grey", size = 0.1)+
      labs(caption = "Data source: United State Geography Service (USGS)")

  }
  else if(level == "county"){
    nutrient_summary = nutrient_summary %>%
      left_join(counties_shape, by = "FIPS")
    # plot data.
    us_plot <- ggplot(nutrient_summary)+
      geom_polygon(aes(x = long, y = lat,
                      fill = Quantity, group = group))+
      geom_polygon(aes(x = long, y = lat, group = group),
                   fill = NA, data = states_shape, color = "lightgrey",
                   size = 0.1)+
      labs(caption = "Data source: United State Geography Service (USGS)")
  }

  # add fixed coordinates ratio.
  if(is.null(coord_fix_ratio)){
    warning("The map might not perform well with free y:x ratio.")
  }
  else if(!is.numeric(coord_fix_ratio)){
    stop("Only numeric values are supported for fixed y:x ratio.")
  }
  else if(abs(coord_fix_ratio) > 100){
    warning("The y:x ratio seems too large.")
    us_plot = us_plot +
      coord_fixed(ratio = coord_fix_ratio)
  }
  else
  {
      us_plot = us_plot +
      coord_fixed(ratio = coord_fix_ratio)
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
                           label = comma,
                           option = viridis_palette)
    }else{
      us_plot <- us_plot +
        scale_fill_viridis(end = viridis_end, trans = trans,
                           direction = viridis_direction, discrete = FALSE,
                           label = comma,
                           option = viridis_palette)
    }

  }

  # check facet in the colnames of dataset.


  if(length(facet) == 1)
  {
    us_plot <- us_plot +
      facet_wrap(reformulate(facet,"." ))
  }

  # add projection system.
  if(!is.null(projection))
  {
    us_plot <- us_plot + coord_map(projection = projection,
                                   parameters = parameters,
                                   orientation = orientation,
                                   xlim = xlim,
                                   ylim = ylim)
  }

  # check if use the default map theme.
  if (!is.null(map_theme)){
    us_plot <- us_plot +
      map_theme
  }

  # check if want to add states_shape or not.
  if (add_north){
  # add stateshapes for further plotting.
  us_plot$states_shape = states_shape
  }

  return(us_plot)
}



#' Create base map theme for further use.
#' Adapted from theme maps.
#' https://www.rdocumentation.org/packages/ggthemes/versions/3.5.0/source
#' @param base_size the size for font in the map.
#' @param base_family the font family in the map.
#' @export theme_map_fertilizer
#' @author Wenlong Liu
#' @examples
#' require(usfertilizer)
#' require(ggfertilizer)
#' data(us_fertilizer_county)
#' # Generate a map.
#' us_plot <- map_us_fertilizer(data = us_fertilizer_county,
#' Year = 2010, Nutrient = "N",
#' level = "county", facet="Year", Farm_Type = "farm")
#'
#' # Add user_defined map theme.
#' us_plot = us_plot + theme_map_fertilizer(base_size = 12)


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

