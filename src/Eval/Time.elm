module Eval.Time exposing (processor)

{-| The interpreter's `Time.*` builtins, as an {@link Eval.Core.Processor}. A Posix time is just its
millisecond `VNum`; `Time.every` is a subscription the editor drives as a live tick. Zones are
modelled as a 0 offset (like `Time.utc`/`Time.here`), so the calendar conversions read the instant
as UTC — matching the existing `toHour`/`toMinute`/`toSecond`, which ignore their zone argument. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "Time.millisToPosix"
    , "Time.posixToMillis"
    , "Time.toHour"
    , "Time.toMinute"
    , "Time.toSecond"
    , "Time.toMillis"
    , "Time.toYear"
    , "Time.toMonth"
    , "Time.toDay"
    , "Time.toWeekday"
    , "Time.customZone"
    , "Time.getZoneName"
    , "Time.every"
    ]


arities : List ( Int, List String )
arities =
    [ ( 0, [ "Time.getZoneName" ] )
    , ( 1, [ "Time.millisToPosix", "Time.posixToMillis" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    case ( name, args ) of
        ( "Time.millisToPosix", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "Time.posixToMillis", [ VNum n ] ) ->
            Just (Ok (VNum n))

        ( "Time.toHour", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 24 (round ms // 3600000)))))

        ( "Time.toMinute", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 60 (round ms // 60000)))))

        ( "Time.toSecond", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 60 (round ms // 1000)))))

        ( "Time.toMillis", [ _, VNum ms ] ) ->
            Just (Ok (VNum (toFloat (modBy 1000 (round ms)))))

        ( "Time.toYear", [ _, VNum ms ] ) ->
            let
                ( y, _, _ ) =
                    civilFromMillis (round ms)
            in
            Just (Ok (VNum (toFloat y)))

        ( "Time.toMonth", [ _, VNum ms ] ) ->
            let
                ( _, m, _ ) =
                    civilFromMillis (round ms)
            in
            Just (Ok (VCtor (monthName m) []))

        ( "Time.toDay", [ _, VNum ms ] ) ->
            let
                ( _, _, d ) =
                    civilFromMillis (round ms)
            in
            Just (Ok (VNum (toFloat d)))

        ( "Time.toWeekday", [ _, VNum ms ] ) ->
            let
                days =
                    floorDiv (round ms) 86400000
            in
            -- 1970-01-01 was a Thursday (index 3 with Mon=0).
            Just (Ok (VCtor (weekdayName (modBy 7 (days + 3))) []))

        ( "Time.customZone", [ VNum offset, _ ] ) ->
            -- A Zone is modelled as its (ignored) minute offset; the conversions read UTC.
            Just (Ok (VNum offset))

        ( "Time.getZoneName", [] ) ->
            -- Task Never ZoneName, resolved to an Offset of 0 (the editor has no real zone db).
            Just (Ok (VCtor "Task.value" [ VCtor "Offset" [ VNum 0 ] ]))

        ( "Time.every", [ VNum interval, toMsg ] ) ->
            Just (Ok (VCtor "Sub.every" [ VNum interval, toMsg ]))

        _ ->
            Nothing


{-| Floored integer division (Elm's `//` truncates toward zero; calendar math needs floor). -}
floorDiv : Int -> Int -> Int
floorDiv a b =
    (a - modBy b a) // b


{-| (year, month 1–12, day 1–31) of a UTC instant, via Howard Hinnant's days→civil algorithm. -}
civilFromMillis : Int -> ( Int, Int, Int )
civilFromMillis ms =
    let
        days =
            floorDiv ms 86400000

        z =
            days + 719468

        era =
            floorDiv
                (if z >= 0 then
                    z

                 else
                    z - 146096
                )
                146097

        doe =
            z - era * 146097

        yoe =
            (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365

        y =
            yoe + era * 400

        doy =
            doe - (365 * yoe + yoe // 4 - yoe // 100)

        mp =
            (5 * doy + 2) // 153

        d =
            doy - (153 * mp + 2) // 5 + 1

        m =
            if mp < 10 then
                mp + 3

            else
                mp - 9
    in
    ( if m <= 2 then
        y + 1

      else
        y
    , m
    , d
    )


monthName : Int -> String
monthName m =
    case m of
        1 ->
            "Jan"

        2 ->
            "Feb"

        3 ->
            "Mar"

        4 ->
            "Apr"

        5 ->
            "May"

        6 ->
            "Jun"

        7 ->
            "Jul"

        8 ->
            "Aug"

        9 ->
            "Sep"

        10 ->
            "Oct"

        11 ->
            "Nov"

        _ ->
            "Dec"


weekdayName : Int -> String
weekdayName w =
    case w of
        0 ->
            "Mon"

        1 ->
            "Tue"

        2 ->
            "Wed"

        3 ->
            "Thu"

        4 ->
            "Fri"

        5 ->
            "Sat"

        _ ->
            "Sun"
