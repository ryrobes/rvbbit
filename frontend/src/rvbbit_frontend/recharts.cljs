(ns rvbbit-frontend.recharts
  (:require
    ["recharts"              :as recharts]
    [clojure.edn             :as edn]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.utility :as ut]))

(def LineChart (.-LineChart recharts))
(def YAxis (.-YAxis recharts))
(def BarChart (.-BarChart recharts))
(def Bar (.-Bar recharts))
(def RadialBarChart (.-RadialBarChart recharts))
(def RadialBar (.-RadialBar recharts))
(def Radar (.-Radar recharts))
(def RadarChart (.-RadarChart recharts))
(def Scatter (.-Scatter recharts))
(def ScatterChart (.-ScatterChart recharts))
(def ComposedChart (.-ComposedChart recharts))

(def AreaChart (.-AreaChart recharts))
(def Area (.-Area recharts))
(def Rectangle (.-Rectangle recharts))
(def Tooltip (.-Tooltip recharts))
(def Brush (.-Brush recharts))
(def Treemap (.-Treemap recharts))
(def Pie (.-Pie recharts))
(def PieChart (.-PieChart recharts))

(def FunnelChart (.-FunnelChart recharts))
(def Funnel (.-Funnel recharts))

(def ResponsiveContainer (.-ResponsiveContainer recharts))
(def CartesianGrid (.-CartesianGrid recharts))
(def Line (.-Line recharts))
(def ReferenceLine (.-ReferenceLine recharts))
(def LabelList (.-LabelList recharts))
(def Legend (.-Legend recharts))
(def XAxis (.-XAxis recharts))
(def Hint (.-Hint recharts))
(def PolarGrid (.-PolarGrid recharts))
(def PolarChart (.-PolarChart recharts))
(def PolarAngleAxis (.-PolarAngleAxis recharts))
(def PolarRadiusAxis (.-PolarRadiusAxis recharts))
(def PolarRadiusAxisTick (.-PolarRadiusAxisTick recharts))

(defn chart
  [container-map]
  (let [data [{:name "Page A" :uv 4000 :pv 2400 :amt 2400} {:name "Page B" :uv 3000 :pv 1398 :amt 2210}
              {:name "Page C" :uv 2000 :pv 9800 :amt 2290} {:name "Page D" :uv 2780 :pv 3908 :amt 2000}
              {:name "Page E" :uv 1890 :pv 4800 :amt 2181} {:name "Page F" :uv 2390 :pv 3800 :amt 2500}
              {:name "Page G" :uv 3490 :pv 4300 :amt 2100}]]
    [:> ResponsiveContainer container-map
     [:> LineChart
      {;:width 500 :height 300 ;; not needed if Container gives it
       :data   data
       :margin {:top 5 :right 30 :left 20 :bottom 5}} [:> CartesianGrid {:strokeDasharray "3 3"}] [:> Legend]
      [:> XAxis {:dataKey :name}] [:> Line {:type "monotone" :dataKey :pv :stroke "#8884d8" :activeDot {:r 8}}]
      [:> Line {:type "monotone" :dataKey :uv :stroke "#82ca9d"}]]]))

