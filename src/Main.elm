module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import Copy exposing (..)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, onClick, preventDefaultOn)
import Json.Decode as Decode
import String.Interpolate exposing (interpolate)
import Task
import Time exposing (Month(..), Weekday(..))



-- MAIN


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { today : Date
    , year : Int
    , month : Date.Month
    , selectedDate : Maybe Date
    }


type alias Calendar =
    { offset : Int
    , days : List Date
    }


init : Int -> ( Model, Cmd Msg )
init now =
    let
        dateNow =
            Date.fromPosix Time.utc <|
                Time.millisToPosix now
    in
    ( { today = dateNow
      , year = Date.year dateNow
      , month = Date.month dateNow
      , selectedDate = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | PrevMonth
    | NextMonth
    | SelectDate (Maybe Date)
    | FocusOn Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PrevMonth ->
            let
                newDate =
                    Date.add Date.Months -1 <| Date.fromCalendarDate model.year model.month 1
            in
            ( { model | year = Date.year newDate, month = Date.month newDate }
            , Cmd.none
            )

        NextMonth ->
            let
                newDate =
                    Date.add Date.Months 1 <| Date.fromCalendarDate model.year model.month 1
            in
            ( { model | year = Date.year newDate, month = Date.month newDate }
            , Cmd.none
            )

        SelectDate maybeDate ->
            ( { model | selectedDate = maybeDate }, Cmd.none )

        FocusOn date ->
            ( { model | year = Date.year date, month = Date.month date }
            , Task.attempt (\_ -> NoOp) <|
                Browser.Dom.focus (Date.toIsoString date)
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Accessible datepicker"
    , body =
        [ main_ [ class "main" ]
            [ h1 [] [ text "pick a date!" ]
            , div [ class "wrapper" ] [ viewDatepicker model ]
            ]
        ]
    }



-- VIEW DATEPICKER


viewDatepicker : Model -> Html Msg
viewDatepicker model =
    let
        ( a11yHint, headerText ) =
            case model.selectedDate of
                Just date ->
                    ( copy.currentSelectedDateIs, String.dropRight 1 <| fullDateString date )

                _ ->
                    ( copy.noCurrentDateSelected, copy.selectDate )
    in
    div
        [ class "date-picker" ]
        [ header [ class "date-picker__header" ]
            [ h3 [ attribute "role" "status" ]
                [ span [ class "visually-hidden" ]
                    [ text a11yHint ]
                , text headerText
                ]
            ]
        , div [ class "date-picker__pick-month" ]
            [ h3 []
                [ span [ class "visually-hidden" ]
                    [ text copy.selectMonth ]
                , text <|
                    interpolate "{0} {1}"
                        [ monthToString model.month
                        , String.fromInt model.year
                        ]
                ]
            , viewSwitchButton
                model.year
                PrevMonth
                (copy.switchMonth <| monthToString model.month)
                True
            , viewSwitchButton
                model.year
                NextMonth
                (copy.switchMonth <| monthToString model.month)
                False
            ]
        , div
            [ class "date-picker__calendar__header"
            , attribute "aria-hidden" "true"
            ]
          <|
            List.map (viewCalendarSquare << Just << String.left 2) dayStrings
        , h3 [ class "visually-hidden" ]
            [ text copy.selectDay ]
        , viewCalendar model.selectedDate (toCalendar model.year model.month)
        , viewClearDateButton model.selectedDate
        ]



-- VIEW BUTTONS


viewSwitchButton : Int -> msg -> (Bool -> String) -> Bool -> Html msg
viewSwitchButton year msg copy isPrev =
    button
        [ onClick msg
        , class "btn-next"
        , classList [ ( " -arrow-left", isPrev ) ]
        ]
        [ span [ class "visually-hidden" ]
            [ text (copy isPrev)
            , text <| String.fromInt year
            ]
        ]


viewClearDateButton : Maybe Date -> Html Msg
viewClearDateButton selectedDate =
    case selectedDate of
        Just _ ->
            button
                [ onClick (SelectDate Nothing)
                , class "btn-delete"
                ]
                [ text copy.clearDate ]

        _ ->
            text ""



-- VIEW CALENDAR


viewCalendar : Maybe Date -> Calendar -> Html Msg
viewCalendar selectedDate calendar =
    div [ class "date-picker__calendar" ] <|
        List.repeat calendar.offset (viewCalendarSquare Nothing)
            ++ List.indexedMap (viewCalendarDay selectedDate) calendar.days


viewCalendarSquare : Maybe String -> Html Msg
viewCalendarSquare s =
    div [ class "date-picker__calendar__square" ] [ text <| Maybe.withDefault "" s ]


viewCalendarDay : Maybe Date -> Int -> Date -> Html Msg
viewCalendarDay selectedDate index date =
    let
        isSelected =
            selectedDate == Just date

        tabindex_ =
            if index > 0 then
                -1

            else
                0

        onKeyDown keyCode =
            let
                focus int =
                    FocusOn <| Date.add Date.Days int date
            in
            case keyCode of
                37 ->
                    --arrow left
                    ( focus -1, True )

                38 ->
                    --arrow up
                    ( focus -7, True )

                39 ->
                    --arrow right
                    ( focus 1, True )

                40 ->
                    --arrow down
                    ( focus 7, True )

                _ ->
                    ( NoOp, False )
    in
    button
        [ class "date-picker__calendar__square"
        , classList [ ( "-selected", isSelected ) ]
        , id (Date.toIsoString date)
        , tabindex tabindex_
        , attribute "aria-pressed" (stringFromBool isSelected)
        , preventDefaultOn "keydown" (Decode.map onKeyDown keyCode)
        , onClick <| SelectDate (Just date)
        ]
        [ span [ class "visually-hidden" ]
            [ text <| fullDateString date ]
        , span [ attribute "aria-hidden" "true", class "date-picker__calendar__square__day" ]
            [ text <| String.fromInt (Date.day date) ]
        , viewA11yCurrentSelectedDate selectedDate
        ]



-- BUILD CALENDAR


toCalendar : Int -> Time.Month -> Calendar
toCalendar year month =
    let
        pad month_ day =
            Date.fromCalendarDate year (Date.numberToMonth month_) day
                |> Date.weekday
                |> Date.weekdayToNumber

        days =
            List.map (Date.fromCalendarDate year month) <|
                List.range 1 (daysInMonth year month)
    in
    { offset = pad (Date.monthToNumber month) 1 - 1
    , days = days
    }


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    let
        february =
            if (modBy 4 year == 0 && modBy 100 year /= 0) || modBy 400 year == 0 then
                29

            else
                28
    in
    Maybe.withDefault 31 <|
        Dict.get (Date.monthToNumber month) <|
            Dict.fromList
                [ ( 2, february )
                , ( 4, 30 )
                , ( 6, 30 )
                , ( 9, 30 )
                , ( 11, 30 )
                ]


monthToString : Time.Month -> String
monthToString month =
    case month of
        Jan ->
            copy.january

        Feb ->
            copy.february

        Mar ->
            copy.march

        Apr ->
            copy.april

        May ->
            copy.may

        Jun ->
            copy.june

        Jul ->
            copy.july

        Aug ->
            copy.august

        Sep ->
            copy.september

        Oct ->
            copy.october

        Nov ->
            copy.november

        Dec ->
            copy.december


weekDayToString : Weekday -> String
weekDayToString weekday =
    case weekday of
        Mon ->
            copy.monday

        Tue ->
            copy.tuesday

        Wed ->
            copy.wednesday

        Thu ->
            copy.thursday

        Fri ->
            copy.friday

        Sat ->
            copy.saturday

        Sun ->
            copy.sunday


dayStrings : List String
dayStrings =
    [ copy.monday
    , copy.tuesday
    , copy.wednesday
    , copy.thursday
    , copy.friday
    , copy.saturday
    , copy.sunday
    ]



-- TEXT HELPERS


viewA11yCurrentSelectedDate : Maybe Date -> Html msg
viewA11yCurrentSelectedDate selectedDate =
    span [ class "visually-hidden" ] <|
        case selectedDate of
            Just date ->
                [ text copy.currentSelectedDateIs
                , text <| fullDateString date
                , text copy.useArrows
                ]

            _ ->
                [ text copy.noCurrentDateSelected
                , text copy.useArrows
                ]


fullDateString : Date -> String
fullDateString date =
    interpolate "{0}, {1} {2} {3}."
        [ weekDayToString (Date.weekday date)
        , String.fromInt (Date.day date)
        , monthToString (Date.month date)
        , String.fromInt (Date.year date)
        ]


stringFromBool : Bool -> String
stringFromBool bool =
    if bool then
        "true"

    else
        "false"
