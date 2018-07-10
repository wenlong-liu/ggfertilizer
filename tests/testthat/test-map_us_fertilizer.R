context("map_us_fertilizer")

################
# Test the input of the function.
################

data = fertilizer

# Produce some error data.
level_errors = c("counties", "states", 22, NULL, NA, 123456, TRUE, "waretes")
fill_ver_type_errors = c("none", NA, "sample", 2311, 5555555555)
facet_errors = c("none", "year", NA, 24123)
fix_ratio_errors = c("none", "year", NA, "sample", TRUE)

# Test input of Year and Nutrient.
test_that("Check to make sure the Year and Nutrient are required.",
          {expect_error(map_us_fertilizer(data = data, Year = 2000))
           expect_error(map_us_fertilizer(data = data, Nutrient = "N"))})


 # Test level should be county or state.
 for (level in level_errors){
      expect_error(map_us_fertilizer(data = data, Year = 2000,
                                          Nutrient = "N", level = level ),
                        "The spatial resolution must be specified as either county or state.", fixed=TRUE)
 }

 # Test fill_var_type should be either NULL, "discrete" or "countinuous".
 for (fill in fill_ver_type_errors){
      expect_error(map_us_fertilizer(data = data, Year = 2000,
                                     Nutrient = "N", fill_var_type = fill ),
                   'fill_var_type must be either NULL, "discrete" or "continuous"', fixed = TRUE)
 }

 # Test for length of facet, should be null or one..
 expect_error(map_us_fertilizer(data = data, Year = 2000,
                                Nutrient = "N", facet = c("Year","County")),
              "Maximum one facet is supported.", fixed = TRUE)
 # Test for the facet should fall in the colnames.
 for (facet in facet_errors){
      expect_error(map_us_fertilizer(data = data, Year = 2000,
                                     Nutrient = "N", facet = facet),
                  "Facet should fall in the colnames of dataset.", fixed = TRUE)
 }

 # Test for the coor_fix_ratio is NULL.
 expect_warning(map_us_fertilizer(data = data, Year = 2000,
                                     Nutrient = "N", coord_fix_ratio = NULL),
                   "The map might not perform well with free y:x ratio.", fixed = TRUE)
 # Test for the coor_fix_ratio should be numeric.
 for (ratio in fix_ratio_errors){
     expect_error(map_us_fertilizer(data = data, Year = 2000,
                                    Nutrient = "N", coord_fix_ratio = ratio))
 }

 # Tes the warning when ratio is too large, but still plot the data.
 test_that("Check the warning of ratio greater than 100, but still plot data.",
           {
             expect_warning(map_us_fertilizer(data = data, Year = 2000,
                                              Nutrient = "N", coord_fix_ratio = 1000),
                            "The y:x ratio seems too large.", fixed = TRUE)
             expect_warning(map_us_fertilizer(data = data, Year = 2000,
                                              Nutrient = "N", coord_fix_ratio = -1000),
                            "The y:x ratio seems too large.", fixed = TRUE)
             us_plot <- map_us_fertilizer(data = data, Year = 2000,
                                          Nutrient = "N", coord_fix_ratio = -1000)
             expect_true(!is.null(us_plot))
             expect_equal("ggplot", class(us_plot)[2])
           })



################
# Test for the correctness of plotting.
# Inspired from Sam Abbott via
# https://github.com/seabbs/getTBinR/tree/master/tests

test_that("map_us_fertilizer produces a map with nitrogen input from synthetic fertilizer in 2000.",
          {
            us_plot <- map_us_fertilizer(data = data, Year = 2000,
                                         Nutrient = "N", Input_Type = "fertilizer")
            expect_true(!is.null(us_plot))
            expect_equal("ggplot", class(us_plot)[2])

            #skip()
            #vdiffr::expect_doppelganger("nitrogen-fertilizer-2000", us_plot)
          })

 test_that("map_us_fertilizer produces a map with nitrogen input from manure in 2007.",
           {
             us_plot <- map_us_fertilizer(data = data, Year = 2007,
                                          Nutrient = "N", Input_Type = "manure")
             expect_true(!is.null(us_plot))
             expect_equal("ggplot", class(us_plot)[2])

             #skip()
             #vdiffr::expect_doppelganger("nitrogen-manure-2007", us_plot)
           })

 test_that("map_us_fertilizer produces a map with phophorus input from manure in 1997.",
           {
             us_plot <- map_us_fertilizer(data = data, Year = 1997,
                                          Nutrient = "P", Input_Type = "fertilizer")
             expect_true(!is.null(us_plot))
             expect_equal("ggplot", class(us_plot)[2])

             #skip()
             #vdiffr::expect_doppelganger("p-fertilizer-1997", us_plot)
           })

 test_that("map_us_fertilizer produces a map with phophorus input from fertilizer in 1997 at state level.",
           {
             us_plot <- map_us_fertilizer(data = data, Year = 1997,
                                          Nutrient = "P", Input_Type = "fertilizer", level = "state")
             expect_true(!is.null(us_plot))
             expect_equal("ggplot", class(us_plot)[2])

            # skip()
            # vdiffr::expect_doppelganger("state_p_manure_1997", us_plot)
           })

 test_that("map_us_fertilizer produces a map with nitrogen input to farms from 1997 to 2004 faceted by Year.",
           {
             us_plot <- map_us_fertilizer(data = data, Year = 1997:2004,
                                          Nutrient = "N", Farm_Type = "farm", facet = "Year")
             expect_true(!is.null(us_plot))
             expect_equal("ggplot", class(us_plot)[2])

             #skip()
             #vdiffr::expect_doppelganger("facet-n-farm-97-04", us_plot)
           })
