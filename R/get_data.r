#'get_data
#'@name get_data
#'@description This function will return the full dataset (or a subset based on input parameters) of county-level estimates
#'  of fertilizer application quantity.  The fertilizer application type includes farm and non-farms.
#'  The dataset was primarily cleaned into Tidy data.  Details are availalble via <https://cran.r-project.org/web/packages/usfertilizer/vignettes/Introduction.html>
#'
#'@param data the dataset to work with, generally the full usfertilizer or its subsets.
#'@param nutrient fertilizer type of interest, default: both Nitrogen and Phosphorus.
#'@param farm_type where the fertilizer was applied. e.g. farm, nonfarm.
#'@param years start year to show data, e.g. 1994, 2000:2005.
#'@param counties counties of interest, defalut: all avaible counties.
#'@param states states of interest, defalt: all avaialble states.
#'@param lat_max the maximum latitude of area of interest.
#'@param lat_min the minmum latitude of area of interest.
#'@param long_max the maximum longitude of area of interest.
#'@param long_min the minimum longitude of area of interest.
#'@param FIPSs FIPS numbers of interest, defalut: all the counties.
#'@param input_type the source of nutrient, e.g. fertilizer or manure.
#'@param overlap_state_county Logic. If true, the function will overlaping
#'       the input of states and counties. If false, the function will return
#'       results either in the states or in the counties.
#'@param combine_state_county Logic. If true, the county will be changed into
#'       county, state, e.g. Wake, NC; If false, no changes.
#'
#'@return A tibble with tidy data.
#'@export
#'@keywords datasets tidydata
#'@import dplyr
#'@seealso \code{link(get_FIPS)}
#'@examples
#' data = fertilizer
#' get_data(data, counties = "Wake")
#' get_data(data, years = 1990)
#' get_data(data, nutrient = "N", years = 2003, states = "NC", FIPSs = 37145)
#'
get_data <- function(data,
                        nutrient = NULL,
                        input_type = NULL,
                        farm_type = NULL,
                        years = NULL,
                        counties = NULL,
                        states = NULL,
                        lat_max = NULL,
                        lat_min = NULL,
                        long_max = NULL,
                        long_min = NULL,
                        FIPSs = NULL,
                        overlap_state_county = TRUE,
                        combine_state_county = FALSE
                        )
{
  # copy the original dataset to avoid protential pollution.
  output = data
  if (is.null(nutrient)){
    nutrient = c("N","P")
}
  else{
    lapply(tolower(nutrient),check_list,check_list_data = tolower(data$Nutrient),
           warning_content= "Nutrient")
  }
  output = filter(output, Nutrient %in% toupper(nutrient) )
  # check input type.
  if (is.null(input_type)){
    input_type = c("fertilizer","manure")
  }
  else{
    lapply(tolower(input_type),check_list,check_list_data = tolower(data$Input.Type),
           warning_content= "Input.Type")
  }
  output = filter(output, tolower(Input.Type) %in% tolower(input_type) )

  # check farm type.
  if (is.null(farm_type)){
    farm_type = c("farm","nonfarm")
  }
  else{
    lapply(tolower(farm_type),check_list,check_list_data = tolower(data$Farm.Type),
           warning_content= "Farm.Type")
  }

  output = filter(output, tolower(Farm.Type) %in% tolower(farm_type) )

  # check years in the list or not.
  if(is.null(years)) {
    years = seq(1945,2012)
  }
  else
    {
    lapply(tolower(years),check_list,check_list_data = data$Year,
           warning_content= "Years")
  }
  output = filter(output, Year %in% years)
  # The FINO and counties can only exist once.

  if(!is.null(FIPSs) & !is.null(counties)){
    stop("Users can only use one of Counties and FIPSs parameters.",call. = FALSE)
  }

  # check if the overlapping is true or not.
  if (overlap_state_county){
    # check the states and counties exist at the same time or not.
    if(!is.null(counties) & !is.null(states)){
      lapply(tolower(counties),check_list,check_list_data = tolower(data$County),
             warning_content= "Counties")
      lapply(tolower(states),check_list,check_list_data = tolower(data$State),
             warning_content= "States")
      output = filter(output, tolower(County) %in% tolower(counties) & tolower(State) %in% tolower(states) )
    }
    # check and return unavaialbe counties.
    # only check the lowercase, in case some users input lower case of county names.
    else {
      if(is.null(counties) & !is.null(states)){
        #counties = data$County
        lapply(tolower(states),check_list,check_list_data = tolower(data$State),
               warning_content= "States")
        output = filter(output, tolower(State) %in% tolower(states))
      }
      else{
        if(!is.null(counties) & is.null(states)){
          lapply(tolower(counties),check_list,check_list_data = tolower(data$County),
                 warning_content= "Counties")
          output = filter(output, tolower(County) %in% tolower(counties))

        }
        else {
          counties = data$County
          states = data$State
          output = filter(output, tolower(County) %in% tolower(counties) | tolower(State) %in% tolower(states) )
        }
      }
    }
    # check and return unavaialbe counties.
  }
  else{
       if (!is.null(counties) & !is.null(states)) {
         lapply(tolower(counties),check_list,check_list_data = tolower(data$County),
                warning_content= "Counties")
         lapply(tolower(states),check_list,check_list_data = tolower(data$State),
                warning_content= "States")
         output = filter(output, tolower(County) %in% tolower(counties) |
                                 tolower(State) %in% tolower(states) )
       }
       else {
         stop("If overlap_state_county is false, both counties and states need to be entered! ")
       }
  }

  if(!is.null(lat_max) & !is.null(lat_min)
     & !is.null(long_max) & !is.null(long_min)){
         check_boundary(lat_max,max(data$INTPTLAT,na.rm = T), F, "Max lat" )
         check_boundary(lat_min,min(data$INTPTLAT,na.rm = T), T, "Min lat" )
         check_boundary(long_max,max(data$INTPTLONG,na.rm = T), F, "Max long" )
         check_boundary(long_min,min(data$INTPTLONG,na.rm = T), T, "Min long" )

  output = filter(output,
                  (INTPTLAT > lat_min) & (INTPTLAT < lat_min) &
                  (INTPTLONG > long_min) & (INTPTLONG < long_min)
                  )
  }

  # check FINO in the list or not.
  if(is.null(FIPSs)){
    FIPSs = data$FIPS
  }

  else {
    lapply(FIPSs,check_list,check_list_data = data$FIPS,
           warning_content= "FIPS")
  }

   output = filter(output, FIPS %in% FIPSs)

   if (length(output$County) == 0 ){
     warning("None data selected, refine your filters!")
   }
   else if (combine_state_county)
     {
     output$County = paste(output$County, ", ", output$State, sep = "")
   }
   return(output)

}

#' function to check the lat and long are within limits.
#'@param input the input lat or long value.
#'@param check_value the max or min value of lat or long.
#'@param expected_greater the input is expected to be greater or not.  Defalut:TRUE.
#'@param warning_content the content used to insert in the error message.

check_boundary <- function(input, check_value, expected_greater = TRUE, warning_content) {
  if(expected_greater){
    if(input < check_value){
      stop( paste(warning_content,input, "is out of bounde!"), call. = FALSE)}
  }
  else {
    if(input > check_value){
      stop( paste(warning_content,input, "is out of bounde!"), call. = FALSE)}
  }
}

#'check the list in the dataset or not.
#'@param input the list to check.
#'@param check_list_data the list in the datasets.
#'@param warning_content the content to be raised as an error.
#'

check_list <- function(input, check_list_data, warning_content){
  if(!is.element(input,check_list_data)){
    stop(paste(warning_content, input[!(input %in% check_list_data)], "are not available in the dataset.\n
               Please deleted them and try again.\n"),call. = FALSE)
  }
}

