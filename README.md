# Appsilon Recruitment Task Dashboard


Thank you so much for the opportunity to be considered for an external contractor role at Appsilon.

This Shiny application, Marine Track, is my submission for the recruitment task. It features a simple, but clear and informative user interface that shows the longest distance (in meters) between any two observations of a given vessel on one of its trips. Simply use the dropdowns to select both a type of vessel, and a particular vessel of that type. 

Please note: all "Navigation" vessels in the provided ships.csv file were marked as "parked". Given that they were parked, it did not seem appropriate to show distances between their observations, and as such, instead of showing those ships on the map, the app displays a message to the user that the ship was parked during the observation period. I can easily adjust this, but it was a display choice I made as it seemed odd to show distances for ships that were parked (even though they may have shifted a bit between observations). There was also a small handful of vessels of other types that had no active trip records. The same was done for those.

Additionally, please note that I have used the shiny.semantic package, and have also used some of its CSS class names to format UI elements.

If you have any questions regarding interface design, code structure or functionality, please let me know.

Thank you again for this opportunity, and I look forward to hearing from you.

Very best,
Andrew McManus
