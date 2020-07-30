module Copy exposing (copy)

import String.Interpolate exposing (interpolate)


copy =
    { monday = "Monday"
    , tuesday = "Tuesday"
    , wednesday = "Wednesday"
    , thursday = "Thursday"
    , friday = "Friday"
    , saturday = "Saturday"
    , sunday = "Sunday"
    , january = "January"
    , february = "February"
    , march = "March"
    , april = "April"
    , may = "May"
    , june = "June"
    , july = "July"
    , august = "August"
    , september = "September"
    , october = "October"
    , november = "November"
    , december = "December"
    , currentSelectedDateIs = "The currently selected date is:"
    , noCurrentDateSelected = "There is no date selected yet."
    , useArrows = "Use the arrow keys to navigate."
    , selectDate = "Select a date"
    , clearDate = "deselect date"
    , selectDay = "Select a day with the buttons below."
    , selectMonth = "Select a month. The current month is:"
    , switchMonth =
        \month isPrev ->
            interpolate "{0} {1} {2} {3}"
                [ "Select the"
                , if isPrev then
                    "previous"

                  else
                    "next"
                , "month. The current month is:"
                , month
                ]
    }
