module Chart exposing (chart1, chart2)

import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Axis.Line
import LineChart.Axis.Range
import LineChart.Axis.Tick
import LineChart.Axis.Ticks
import LineChart.Axis.Title
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import Svg


type alias Info =
    { x : Float
    , y : Float
    }


listNumber : List number
listNumber =
    [ 46, 48, 42, 38, 34, 38, 43, 45, 34, 36, 37, 32, 46, 43, 49, 48 ]


actual1 : List { x : Float, y : Float }
actual1 =
    List.indexedMap (\index y -> { x = toFloat index, y = y }) (0 :: listNumber)


expected1 : List { x : Float, y : Float }
expected1 =
    List.indexedMap (\index y -> { x = toFloat index + 1, y = y }) (List.reverse listNumber ++ listNumber)


actual2 : List { x : Float, y : Float }
actual2 =
    List.indexedMap (\index y -> { x = toFloat index, y = y * 3 }) (0 :: List.reverse listNumber)


expected2 : List { x : Float, y : Float }
expected2 =
    List.indexedMap (\index y -> { x = toFloat index + 1, y = y }) (listNumber ++ List.reverse listNumber)


chartConfig : String -> LineChart.Config Info msg
chartConfig yAxis =
    { x = LineChart.Axis.default 600 "Seconds" .x
    , y = LineChart.Axis.default 300 yAxis .y
    , container = LineChart.Container.responsive "line-chart-1"
    , interpolation = LineChart.Interpolation.default
    , intersection = LineChart.Axis.Intersection.default
    , legends = LineChart.Legends.default
    , events = LineChart.Events.default
    , junk = LineChart.Junk.default
    , grid = LineChart.Grid.default
    , area = LineChart.Area.default
    , line = LineChart.Line.default
    , dots = LineChart.Dots.custom (LineChart.Dots.full 4)
    }


chart : List Info -> List Info -> String -> Svg.Svg msg
chart data1 data2 yAxis =
    LineChart.viewCustom (chartConfig yAxis)
        [ LineChart.line LineChart.Colors.purple LineChart.Dots.circle "Actual" data1
        , LineChart.line LineChart.Colors.blue LineChart.Dots.square "Expected" data2
        ]


chart1 : Svg.Svg msg
chart1 =
    chart actual1 expected1 "CVR"


chart2 : Svg.Svg msg
chart2 =
    chart actual2 expected2 "Errors"
