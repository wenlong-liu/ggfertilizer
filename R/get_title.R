#' @name get_title
#' @title Automatically generate the title for the map or plot.
#'
#' @description The title of map of plot heavily depends on the input parameters.
#' To help user generate the title, this function will guess the range and type of
#' inputs and provide a title.  Always, this title can be override by ggtitle().
#'
#' @param Year the temporal coverage of the data to visualize. See link for available period.
#' @param Nutrient the nutrient type of fertilizer from usfertilizer.  Has to be
#'     N, P or both (NULL).
#' @param Farm_Type the spatial source of fertilizer, should be farm, nonfarm or both.
#' @param Input_Type the input source of fertilizer, should be manure, fertilizer.
#' @param State the states that will show in the plot,default: all states.
#' @param County the counties that will show in the plot, default: all counties.
#' @param State_title the user-defined title for state.
#' @param County_title the user-defined title for county.
#' @param FIPS FIPS numbers of interest, defalut: all the counties.
#' @param level the spatial resolution of map, should be county or state, default county.
#' @export get_title
#' @return a ggtile obejct.
#' @importFrom ggplot2 ggtitle
#' @seealso \code{link(get_data), link(get_FIPS)}
#' @examples
#' require(ggfertilizer)
#' Year = 2002
#' Nutrient = "N"
#' get_title(Year = Year, Nutrient = Nutrient)

get_title <- function(Year = NULL,
                      Nutrient = NULL,
                      Input_Type = NULL,
                      Farm_Type = NULL,
                      State = NULL,
                      County = NULL,
                      State_title = NULL,
                      County_title = NULL,
                      FIPS = NULL,
                      level = "county"
                      ){
   # Guess years.
   if (is.null(Year) | length(Year) == 0){
     Year_title <- NULL
   }
  else if(length(Year) == 1)
    {
    Year_title <- paste("in",Year)
  }
  # Years greater than 2.
  else if(length(Year) == 2){
    Year_title <- paste("in", Year[1], "and", Year[2])
  }
  else if(length(Year) == 3){
    Year_title <- paste("in", min(Year),",", median(Year), "and", max(Year))
  }
  # if evenly distributed.
  else if((diff(range(Year))+1) %% length(Year) == 0 ) {
    Year_title <- paste("from", min(Year), "to", max(Year),
                        "every", ((diff(range(Year))+1) / length(Year)), "year(s)")
  }
  # if not evenly distributed.
  else if(diff(range(Year)) %% length(Year) != 0 ){
    Year_title <- paste("from", min(Year), "to", max(Year))
    message("Plot title might not be properly generated. Please double check, or use ggtitle() to override it.")
  }

  # Guess Nutrient
  if (is.null(Nutrient) || tolower(Nutrient) == "both"){
    Nutrient_title <- "N and P"
  }
  else if(tolower(Nutrient) == "n" | tolower(Nutrient) == "nitrogen") {
    Nutrient_title <- "N"
  }
  else if(tolower(Nutrient) == "p" | tolower(Nutrient) == "phosphorus"){
    Nutrient_title <- "P"
  }

  # Guess Farm Type
  if (is.null(Farm_Type) || tolower(Farm_Type) == "both"){
    Farm_Type_title <- "farm and nonfarm"
  }
  else if(tolower(Farm_Type) == "farm" ) {
    Farm_Type_title <- Farm_Type
  }
  else if(tolower(Farm_Type) == "nonfarm" ){
    Farm_Type_title <- Farm_Type
  }

  # Guess Fertilizer Type
  if (is.null(Input_Type) || tolower(Input_Type) == "both"){
    Input_Type_title <- "synthetic fertilizer and manure"
  }
  else if(tolower(Input_Type) == "fertilizer" ) {
    Input_Type_title <- "synthetic fertilizer"
  }
  else if(tolower(Input_Type) == "manure" ){
    Input_Type_title <- Input_Type
  }

  # Guess states
  if (is.null(State)){
    State_title <- "United States"
  } else if(!is.null(State_title))
    {
    State_title <- State_title
  }
  else{
    State_title <- NULL
  }

  # Guess counties
  if (is.null(County) ){
    County_title <- NULL
  }
  else if(!is.null(County_title)){
    County_title <- paste(" and", County_title)
  }
  else {
    County_title <- NULL
  }

  # Guess spatial resolution.
  if(is.null(level) || !(tolower(level) %in% c("county", "state"))){
    stop("The map level for us fertilizer should be either county or state.")
  }
  else{
    level_title <- level
  }

  # Produce title.
  title <- ggtitle(paste(Nutrient_title,  " from ", Input_Type_title, " input to ", Farm_Type_title,
                         " ",Year_title, " ", "in ", State_title, County_title,
                         " at the ", level, " level", sep = ""))
  return(title)
}
