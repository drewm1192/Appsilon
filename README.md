# Appsilon Recruitment Task Dashboard


This Shiny application, Marine Track, features a simple, but clear and informative user interface that shows the longest distance (in meters) between any two observations of a given vessel on one of its trips. Simply use the dropdowns to select both a type of vessel, and a particular vessel of that type. 

Please note: all "Navigation" vessels in the provided ships.csv file were marked as "parked". Given that they were parked, it did not seem appropriate to show distances between their observations, and as such, instead of showing those ships on the map, the app displays a message to the user that the ship was parked during the observation period. I can easily adjust this, but it was a display choice I made as it seemed odd to show distances for ships that were parked (even though they may have shifted a bit between observations). There was also a small handful of vessels of other types that had no active trip records. The same was done for those.
