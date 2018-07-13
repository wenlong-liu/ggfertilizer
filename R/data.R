#' fertilizer
#'
#' This is a sample dataset from usfertilizer.   Please visit <https://wenlong-liu.github.io/usfertilizer>. for more details.
#'
#' @format A data frame with 31,279 rows and 12 variables:
#' \describe{
#'   \item{FIPS}{FIPS is a combination of state and county codes, in character format.}
#'   \item{State}{The state abbr. of U.S.}
#'   \item{County}{County name in U.S.}
#'   \item{ALAND}{The land area in each county, unit: km squared}
#'   \item{AWATER}{The water area in each county, unit: km squared}
#'   \item{INTPTLAT}{The latitude of centriod in each county, e.g. 32.53638}
#'   \item{INTPTLONG}{The longitude of centriod in each county, e.g. -86.64449}
#'   \item{Quantity}{The quantity of fertilizeation as N or P, e.g. kg N or kg P}
#'   \item{Year}{The year of estimated data, e.g. 1994}
#'   \item{Nutrient}{The fertilizer type, e.g. N or P}
#'   \item{Farm.Type}{The land use type of fertilizer, e.g. farm and nonfarm}
#'   \item{Input.Type}{The input type of nutrient, e.g. Fertilizer or Manure}
#'   ...
#' }
#' @examples
#'   require(ggfertilizer)
#'   data(fertilizer)

"fertilizer"

if(getRversion() >= "2.15.1") utils::globalVariables(
    c("FIPS", "County", "Year", "Input.Type","Nutrient","Farm.Type", "State",
           "INTPTLAT","INTPTLONG","Quantity","us_fertilizer_county","region",
      "subregion","fips","group","lat","state.abb","reformulate","county.fips",
      "state_shape","long"))
