;; in this model what is spread at first is just an avowal,
;; and then people form their belief through aggregation (by majority),
;; and then they avow their belief to neighbors, and so on.

globals [
  infinity                               ;; used to denote distance between two turtles which don't have a connected or unconnected path between them
  average-path-length                  ;; average path length of the network
  clustering-coefficient               ;; the clustering coefficient of the network; this is the average of clustering coefficients of all turtles
  orange-clustering-coefficient
  violet-clustering-coefficient
  average-degree
  check-connected-and-measure
  average-bubble
  equilibrium                   ;;if the system is in equilibrium, stop
  N ;number of nodes
  vlibNo ; num of very liberals that believe No
  vlibYes ; num of very liberal that believe Yes
  libNo ; num of liberals that...
  libYes
  mYes ; num of moderate that believe Yes
  mNo ;
  conYes
  conNo
  vconYes
  vconNo
  No.vlib ;prob of belief no given vlib
  Yes.vlib
  No.lib
  Yes.lib
  No.m
  Yes.m
  No.vcon
  Yes.vcon
  No.con
  Yes.con
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agent Attributes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
turtles-own[
 node-clustering-coefficient
 same-type-clustering-coefficient
 distance-from-other-turtles ;; list of distances of this node from other turtles
 bubble-coefficient
 ideology-level ;; 0 means moderate, 1 means liberal/conservative, 2 means very liberal/conservative
 ideology ;1 very liberal, 2 liberal, 3 moderate, 4 conservative, 5 very conservative (Purple for lib, orange for con)
 belief ;0 unknown, 1 no, 2 yes;; yes is associated with violet, no with orange
 avowals-heard
 changed-belief?
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link Attributes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
links-own[
  ideology-diversity
]




;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set infinity 99999 ;; just an arbitrary choice for a large number, needed for path length computation
  set-default-shape turtles "circle"
  make-nodes 200
  make-edges
  measure-bubble
  make-ideology
  ask turtles [
    set avowals-heard []
    set changed-belief? false
  ]
  ;block-by-ideology ; this removes all links between very liberals and very conservatives
  measure-link-diversity
  if layout? [ layout ]
  seed-info
  ;while [count turtles with [belief = 1] = 0 or count turtles with [belief = 2] = 0] [
  ;  seed-info]
  set equilibrium 0
  measure-ideology-info
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;
to reset-beliefs
  seed-info
  reset-ticks
end

to go
  belief-spread
  measure-ideology-info
  tick
  if not member? true [changed-belief?] of turtles [stop]
  ;let b count turtles with [belief = 0]
  ;ifelse equilibrium = b [stop] [set equilibrium b]
end


to belief-spread
  ask turtles with [belief != 0] [
    ask link-neighbors [
            if ideology-level = 0 [;moderates use the baseline level set by the motivated-bias global variable
        ifelse random-float 1 < motivated-bias [
          if color = orange and [belief] of myself = 1 [set avowals-heard lput 1 avowals-heard ]
          if color = violet and [belief] of myself = 2 [set avowals-heard lput 2 avowals-heard]
        ]
        [
          set avowals-heard lput [belief] of myself avowals-heard
          ;set belief [belief] of myself
        ]
      ]
      if ideology-level = 2 [;highest ideologies use the max bias level set by the max-motivated-bias global variable
        ifelse random-float 1 < max-motivated-bias [
          if color = orange and [belief] of myself = 1 [set avowals-heard lput 1 avowals-heard]
          if color = violet and [belief] of myself = 2 [set avowals-heard lput 2 avowals-heard]
        ]
        [
          set avowals-heard lput [belief] of myself avowals-heard
         ; set belief [belief] of myself
        ]
      ]
      if ideology-level = 1 [; level 1 ideologies use the mid point of the two bias levels
        let mb motivated-bias + ((max-motivated-bias - motivated-bias) / 2)
        ifelse random-float 1 < mb [
          if color = orange and [belief] of myself = 1 [set avowals-heard lput 1 avowals-heard]
          if color = violet and [belief] of myself = 2 [set avowals-heard lput 2 avowals-heard]
        ]
        [
          set avowals-heard lput [belief] of myself avowals-heard
          ;set belief [belief] of myself
        ]
      ]
    ]
  ]
  ask turtles [
   ifelse (member? 1 modes avowals-heard) and (member? 2 modes avowals-heard)  [
      ;if the modes include both 1 and 2 (or neither), this is a tie and so the agent does not update
    ]
    [; else one of 1 or 2 is the mode and the agent will update their belief to the most common avowal
      let old-b belief
      if member? 1 modes avowals-heard [set belief 1 ]
      if member? 2 modes avowals-heard [set belief 2 ]
      ifelse old-b = belief [set changed-belief? false] [set changed-belief? true]
    ]
    set avowals-heard []
  ]
end


to seed-info
  ask turtles [set belief 0]
  if StartType = "PyyOnn" [
    ask n-of 2 turtles with [ideology <= 2] [set belief 2]
    ask n-of 2 turtles with [ideology >= 4] [set belief 1] ]
  if StartType = "PynOyn" [
    ask n-of 1 turtles with [ideology <= 2] [set belief 2]
    ask n-of 1 turtles with [ideology >= 4] [set belief 1]
    ask n-of 1 turtles with [ideology <= 2] [set belief 1]
    ask n-of 1 turtles with [ideology >= 4] [set belief 2]
  ]
  if StartType = "PnnOyy" [
    ask n-of 2 turtles with [ideology <= 2] [set belief 1]
    ask n-of 2 turtles with [ideology >= 4] [set belief 2] ]
  if StartType = "Myynn" [
    ask n-of 2 turtles with [ideology = 3] [set belief 2]
    ask n-of 2 turtles with [ideology = 3] [set belief 1] ]
  if StartType = "Myyyn" [
    ask n-of 3 turtles with [ideology = 3] [set belief 2]
    ask n-of 1 turtles with [ideology = 3] [set belief 1] ]
  if StartType = "Mynnn" [
    ask n-of 1 turtles with [ideology = 3] [set belief 2]
    ask n-of 3 turtles with [ideology = 3] [set belief 1] ]
  if StartType = "PyyyOn" [
    ask n-of 3 turtles with [ideology <= 2] [set belief 2]
    ask n-of 1 turtles with [ideology >= 4] [set belief 1]
  ]
  if StartType = "PyyOyn" [
    ask n-of 2 turtles with [ideology <= 2] [set belief 2]
    ask n-of 1 turtles with [ideology >= 4] [set belief 1]
    ask n-of 1 turtles with [ideology >= 4] [set belief 2]
  ]
  if StartType = "PyOyyn" [
    ask n-of 1 turtles with [ideology <= 2] [set belief 2]
    ask n-of 1 turtles with [ideology >= 4] [set belief 1]
    ask n-of 2 turtles with [ideology >= 4] [set belief 2]
  ]
  if StartType = "PnOyyy" [
    ask n-of 1 turtles with [ideology <= 2] [set belief 1]
    ask n-of 3 turtles with [ideology >= 4] [set belief 2]
  ]
  if StartType = "Pyynn" [
    ask n-of 2 turtles with [ideology <= 2] [set belief 2]
    ask n-of 2 turtles with [ideology <= 2] [set belief 1]
  ]
  if StartType = "Pyyyn" [
    ask n-of 3 turtles with [ideology <= 2] [set belief 2]
    ask n-of 1 turtles with [ideology <= 2] [set belief 1]
  ]
  if StartType = "Pynnn" [
    ask n-of 1 turtles with [ideology <= 2] [set belief 2]
    ask n-of 3 turtles with [ideology <= 2] [set belief 1]
  ]
  if StartType = "Oynnn" [
    ask n-of 1 turtles with [ideology >= 4] [set belief 2]
    ask n-of 3 turtles with [ideology >= 4] [set belief 1]
  ]
  if StartType = "Oyynn" [
    ask n-of 2 turtles with [ideology >= 4] [set belief 2]
    ask n-of 2 turtles with [ideology >= 4] [set belief 1]
  ]
  if StartType = "Oyyyn" [
    ask n-of 3 turtles with [ideology >= 4] [set belief 2]
    ask n-of 1 turtles with [ideology >= 4] [set belief 1]
  ]
end


to block-by-ideology
  ask links [if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 5 [die]]
end

to make-ideology
  ask turtles [ set ideology-level -1]
  let num-mod round (0.35 * count turtles)
  let num-ext round (0.25 * count turtles)
  ask min-n-of num-mod turtles [bubble-coefficient] [ set ideology-level 0 ]
  ask max-n-of num-ext turtles [bubble-coefficient] [ set ideology-level 2]
  ask turtles with [ideology-level = -1 ] [set ideology-level 1]
  ask turtles [
    if color = violet and ideology-level = 2 [set ideology 1]
    if color = violet and ideology-level = 1 [set ideology 2]
    if ideology-level = 0 [set ideology 3]
    if color = orange and ideology-level = 1 [set ideology 4]
    if color = orange and ideology-level = 2 [set ideology 5]
  ]
end

to make-edges
  ifelse small-world? = true [;create a small-wrold network with short average paths (2-3) and high clustering (0.3-0.6)
    ask turtles [
      let cluster min-n-of 14 other turtles [distance myself]
      create-links-with cluster [set color white]
      let same-type n-of 3 other turtles with [color = [color] of myself]
      create-links-with same-type [set color [color] of myself]
      let other-type n-of 0 other turtles with [color != [color] of myself]
      create-links-with other-type [set color gray]
    ]
  ]
  [;else make a random graph (short paths, but low clustering)
    ask turtles [
      let same-type n-of 7 other turtles with [color = [color] of myself]
      create-links-with same-type [set color [color] of myself]
      let other-type n-of 4 other turtles with [color != [color] of myself]
      create-links-with other-type [set color gray]
    ]
  ]
end

to make-nodes [t]
  create-turtles t
 [
    setxy random-xcor random-ycor
    ifelse random-float 1 < .5[ ;this controls how much of a bubble agents are in
      if xcor > 0 [set color orange]
      if xcor <= 0 [set color violet]
    ]
    [
      if xcor > 0 [set color violet]
      if xcor <= 0 [set color orange]
    ]
  ]
end

to measure-link-diversity
  ask links [
    if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 1 [set ideology-diversity 0]
    if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 2 [set ideology-diversity 1]
    if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 3 [set ideology-diversity 2]
    if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 4 [set ideology-diversity 3]
    if min [ideology] of both-ends = 1 and max [ideology] of both-ends = 5 [set ideology-diversity 4]
    if min [ideology] of both-ends = 2 and max [ideology] of both-ends = 2 [set ideology-diversity 0]
    if min [ideology] of both-ends = 2 and max [ideology] of both-ends = 3 [set ideology-diversity 1]
    if min [ideology] of both-ends = 2 and max [ideology] of both-ends = 4 [set ideology-diversity 2]
    if min [ideology] of both-ends = 2 and max [ideology] of both-ends = 5 [set ideology-diversity 3]
    if min [ideology] of both-ends = 3 and max [ideology] of both-ends = 3 [set ideology-diversity 0]
    if min [ideology] of both-ends = 3 and max [ideology] of both-ends = 4 [set ideology-diversity 1]
    if min [ideology] of both-ends = 3 and max [ideology] of both-ends = 5 [set ideology-diversity 2]
    if min [ideology] of both-ends = 4 and max [ideology] of both-ends = 4 [set ideology-diversity 0]
    if min [ideology] of both-ends = 4 and max [ideology] of both-ends = 5 [set ideology-diversity 1]
    if min [ideology] of both-ends = 5 and max [ideology] of both-ends = 5 [set ideology-diversity 0]
  ]
end

to measure-ideology-info
  set N count turtles
  set vlibNo count turtles with [ideology = 1 and belief = 1]
  set vlibYes count turtles with [ideology = 1 and belief = 2]
  set libNo count turtles with [ideology = 2 and belief = 1]
  set libYes count turtles with [ideology = 2 and belief = 2]
  set mYes count turtles with [ideology = 3 and belief = 2]
  set mNo count turtles with [ideology = 3 and belief = 1]
  set conYes count turtles with [ideology = 4 and belief = 2]
  set conNo count turtles with [ideology = 4 and belief = 1]
  set vconYes count turtles with [ideology = 5 and belief = 2]
  set vconNo count turtles with [ideology = 5 and belief = 1]
  ifelse vlibNo + vlibYes != 0 [set No.vlib vlibNo / (vlibNo + vlibYes)] [set No.vlib 999999999]
  ifelse vlibNo + vlibYes != 0 [set Yes.vlib vlibYes / (vlibNo + vlibYes)] [set Yes.vlib 999999999]
  ifelse libNo + libYes != 0 [set No.lib libNo / (libNo + libYes)] [set No.lib 999999999]
  ifelse libNo + libYes != 0 [set Yes.lib libYes / (libNo + libYes)] [set Yes.lib 999999999]
  ifelse mNo + mYes != 0 [set No.m mNo / (mNo + mYes)] [set No.m 999999999]
  ifelse mNo + mYes != 0 [set Yes.m mYes / (mNo + mYes)] [set Yes.m 999999999]
  ifelse conNo + conYes != 0 [set No.con conNo / (conNo + conYes)] [set No.con 999999999]
  ifelse conNo + conYes != 0 [set Yes.con conYes / (conNo + conYes)] [set Yes.con 999999999]
  ifelse vconNo + vconYes != 0 [set No.vcon vconNo / (vconNo + vconYes)] [set No.vcon 999999999]
  ifelse vconNo + vconYes != 0 [set Yes.vcon vconYes / (vconNo + vconYes)] [set Yes.vcon 999999999]
end


to measure-bubble
  ask turtles [
    set bubble-coefficient count link-neighbors with [color = [color] of myself] / count link-neighbors
  ]
  set average-bubble mean [bubble-coefficient] of turtles
end


to measure
  ;;find the average degree
  set average-degree mean [count link-neighbors] of turtles
  set check-connected-and-measure do-calculations
end

to same-type-measure
  find-same-type-clustering-coefficient
end


;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end




;; do-calculations reports true if the network is connected,
;;   and reports false if the network is disconnected.
;; (In the disconnected case, the average path length does not make sense,
;;   or perhaps may be considered infinite)
to-report do-calculations

  ;; set up a variable so we can report if the network is disconnected
  let connected? true

  ;; find the path lengths in the network
  find-path-lengths

  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ;; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ;; and none of those distances should be infinity.
  ;; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ;; In that case, calculating the average-path-length doesn't really make sense.
  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
  [
      set average-path-length infinity
      ;; report that the network is not connected
      set connected? false
  ]
  [
    set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
  ]
  ;; find the clustering coefficient and add to the aggregate for all iterations
  find-clustering-coefficient
  find-same-type-clustering-coefficient
  ;; report whether the network is connected or not
  report connected?
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clustering computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to find-clustering-coefficient
  ifelse all? turtles [count link-neighbors <= 1]
  [
    ;; it is undefined
    ;; what should this be?
    set clustering-coefficient 0
  ]
  [
    let total 0
    ask turtles with [ count link-neighbors <= 1]
      [ set node-clustering-coefficient "undefined" ]
    ask turtles with [ count link-neighbors > 1]
    [
      let hood link-neighbors
      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
      ;; find the sum for the value at turtles
      set total total + node-clustering-coefficient
    ]
    ;; take the average
    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
  ]
end

to find-same-type-clustering-coefficient
  ifelse all? turtles [count link-neighbors with [color = [color] of myself] <= 1]
  [
    ;; it is undefined
    ;; what should this be?
    set orange-clustering-coefficient 0
    set violet-clustering-coefficient 0
  ]
  [
    ask turtles with [ count link-neighbors with [color = [color] of myself] <= 1]
      [ set same-type-clustering-coefficient "undefined" ]
    ask turtles with [ count link-neighbors with [color = [color] of myself] > 1]
    [
      let hood link-neighbors with [color = [color] of myself]
      set same-type-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
    ]
    ;; take the average
    ;set clustering-coefficient total / count turtles with [count link-neighbors > 1]
    set orange-clustering-coefficient mean [same-type-clustering-coefficient] of turtles with [color = orange]
    set violet-clustering-coefficient mean [same-type-clustering-coefficient] of turtles with [color = violet]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path length computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implements the Floyd Warshall algorithm for All Pairs Shortest Paths
;; It is a dynamic programming algorithm which builds bigger solutions
;; from the solutions of smaller subproblems using memoization that
;; is storing the results.
;; It keeps finding incrementally if there is shorter path through
;; the kth node.
;; Since it iterates over all turtles through k,
;; so at the end we get the shortest possible path for each i and j.

to find-path-lengths
  ;; reset the distance list
  ask turtles
  [
    set distance-from-other-turtles []
  ]

  let i 0
  let j 0
  let k 0
  let node1 one-of turtles
  let node2 one-of turtles
  let node-count count turtles
  ;; initialize the distance lists
  while [i < node-count]
  [
    set j 0
    while [j < node-count]
    [
      set node1 turtle i
      set node2 turtle j
      ;; zero from a node to itself
      ifelse i = j
      [
        ask node1 [
          set distance-from-other-turtles lput 0 distance-from-other-turtles
        ]
      ]
      [
        ;; 1 from a node to it's neighbor
        ifelse [ link-neighbor? node1 ] of node2
        [
          ask node1 [
            set distance-from-other-turtles lput 1 distance-from-other-turtles
          ]
        ]
        ;; infinite to everyone else
        [
          ask node1 [
            set distance-from-other-turtles lput infinity distance-from-other-turtles
          ]
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set i 0
  set j 0
  let dummy 0
  while [k < node-count]
  [
    set i 0
    while [i < node-count]
    [
      set j 0
      while [j < node-count]
      [
        ;; alternate path length through kth node
        set dummy ( (item k [distance-from-other-turtles] of turtle i) +
                    (item j [distance-from-other-turtles] of turtle k))
        ;; is the alternate path shorter?
        if dummy < (item j [distance-from-other-turtles] of turtle i)
        [
          ask turtle i [
            set distance-from-other-turtles replace-item j distance-from-other-turtles dummy
          ]
        ]
        set j j + 1
      ]
      set i i + 1
    ]
    set k k + 1
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
213
10
551
349
-1
-1
3.333333333333334
1
10
1
1
1
0
0
0
1
-49
49
-49
49
1
1
1
ticks
30.0

BUTTON
8
20
74
53
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
86
19
171
52
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
9
60
72
93
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
586
28
689
61
layout?
layout?
1
1
-1000

BUTTON
587
76
657
109
NIL
layout
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
846
30
1064
185
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

MONITOR
1075
111
1168
156
NIL
average-path-length
17
1
11

MONITOR
1076
165
1205
210
NIL
clustering-coefficient
17
1
11

MONITOR
1074
58
1175
103
NIL
average-degree
17
1
11

BUTTON
1072
19
1157
52
NIL
measure
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
846
214
1064
344
Bubble-coefficient Distribution
rounded bubble coef.
NIL
0.0
1.1
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" "histogram [precision bubble-coefficient 1] of turtles"

MONITOR
713
152
824
197
NIL
average-bubble
17
1
11

PLOT
890
532
1050
682
Ideology-level distribution
NIL
NIL
0.0
3.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ideology-level] of turtles"

PLOT
1067
533
1227
683
Ideology
NIL
NIL
1.0
6.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ideology] of turtles"

PLOT
3
370
203
520
Beliefs
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"\"0\"" 1.0 0 -16777216 true "" "plot count turtles with [belief = 0] / count turtles"
"\"no\"" 1.0 0 -817084 true "" "plot count turtles with [belief = 1] / count turtles"
"\"yes\"" 1.0 0 -8630108 true "" "plot count turtles with [belief = 2] / count turtles"

SLIDER
16
284
188
317
motivated-bias
motivated-bias
0
1
0.0
.1
1
NIL
HORIZONTAL

PLOT
594
217
828
337
Link Diversity (diff in node ideology)
NIL
NIL
0.0
6.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ideology-diversity] of links"

MONITOR
591
362
641
407
1-1
count links with [min [ideology] of both-ends = 1 and max [ideology] of both-ends = 1]
17
1
11

MONITOR
648
362
698
407
1-2
count links with [min [ideology] of both-ends = 1 and max [ideology] of both-ends = 2]
17
1
11

MONITOR
704
361
754
406
1-3
count links with [min [ideology] of both-ends = 1 and max [ideology] of both-ends = 3]
17
1
11

MONITOR
761
361
811
406
1-4
count links with [min [ideology] of both-ends = 1 and max [ideology] of both-ends = 4]
17
1
11

MONITOR
817
361
867
406
1-5
count links with [min [ideology] of both-ends = 1 and max [ideology] of both-ends = 5]
17
1
11

MONITOR
648
413
698
458
2-2
count links with [min [ideology] of both-ends = 2 and max [ideology] of both-ends = 2]
17
1
11

MONITOR
706
414
756
459
2-3
count links with [min [ideology] of both-ends = 2 and max [ideology] of both-ends = 3]
17
1
11

MONITOR
762
414
812
459
2-4
count links with [min [ideology] of both-ends = 2 and max [ideology] of both-ends = 4]
17
1
11

MONITOR
818
414
868
459
2-5
count links with [min [ideology] of both-ends = 2 and max [ideology] of both-ends = 5]
17
1
11

MONITOR
707
467
757
512
3-3
count links with [min [ideology] of both-ends = 3 and max [ideology] of both-ends = 3]
17
1
11

MONITOR
764
468
814
513
3-4
count links with [min [ideology] of both-ends = 3 and max [ideology] of both-ends = 4]
17
1
11

MONITOR
818
467
868
512
3-5
count links with [min [ideology] of both-ends = 3 and max [ideology] of both-ends = 5]
17
1
11

MONITOR
763
520
813
565
4-4
count links with [min [ideology] of both-ends = 4 and max [ideology] of both-ends = 4]
17
1
11

MONITOR
819
520
869
565
4-5
count links with [min [ideology] of both-ends = 4 and max [ideology] of both-ends = 5]
17
1
11

MONITOR
820
575
870
620
5-5
count links with [min [ideology] of both-ends = 5 and max [ideology] of both-ends = 5]
17
1
11

MONITOR
591
148
648
193
links
count links
17
1
11

PLOT
215
371
467
521
Beliefs by ideology
NIL
NIL
0.0
1.0
0.0
10.0
true
true
"" ""
PENS
"orange \"yes\"" 1.0 0 -6459832 true "" "plot count turtles with [color = orange and belief = 2]"
"orange \"no\"" 1.0 0 -2674135 true "" "plot count turtles with [color = orange and belief = 1]"
"violet \"yes\"" 1.0 0 -8630108 true "" "plot count turtles with [color = violet and belief = 2]"
"violet \"no\"" 1.0 0 -13791810 true "" "plot count turtles with [color = violet and belief = 1]"

MONITOR
1074
220
1200
265
orange-clustering
orange-clustering-coefficient
17
1
11

MONITOR
1077
276
1180
321
violet-cluster
violet-clustering-coefficient
17
1
11

BUTTON
84
62
194
95
NIL
reset-beliefs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
11
116
147
149
small-world?
small-world?
0
1
-1000

MONITOR
13
537
73
582
NIL
Yes.vlib
17
1
11

MONITOR
87
537
145
582
NIL
Yes.lib
17
1
11

MONITOR
158
537
216
582
NIL
Yes.m
17
1
11

MONITOR
228
537
291
582
NIL
Yes.con
17
1
11

MONITOR
304
538
374
583
NIL
Yes.vcon
17
1
11

MONITOR
14
599
72
644
NIL
No.vlib
17
1
11

MONITOR
87
599
145
644
NIL
No.lib
17
1
11

MONITOR
158
599
216
644
NIL
No.m
17
1
11

MONITOR
229
599
287
644
NIL
No.con
17
1
11

MONITOR
304
600
369
645
NIL
No.vcon
17
1
11

MONITOR
893
419
1033
464
bubble | ideo-level 1
mean [bubble-coefficient] of turtles with [ideology-level = 1]
17
1
11

MONITOR
894
472
1034
517
bubble | ideo-level 2
mean [bubble-coefficient] of turtles with [ideology-level = 2]
17
1
11

MONITOR
893
362
1033
407
bubble | ideo-level 0
mean [bubble-coefficient] of turtles with [ideology-level = 0]
17
1
11

SLIDER
13
331
193
364
max-motivated-bias
max-motivated-bias
motivated-bias
1
1.0
.1
1
NIL
HORIZONTAL

CHOOSER
29
186
167
231
StartType
StartType
"PyyOnn" "PynOyn" "PnnOyy" "Myynn" "Myyyn" "Mynnn" "Pyynn" "Pyyyn" "Pynnn" "PyyyOn" "PyyOyn" "PyOyyn" "PnOyyy" "Oynnn" "Oyynn" "Oyyyn"
12

TEXTBOX
591
555
741
583
Counts for links between ideologies (1,2,3,4, or 5)
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

We present a network model with an empirically-validated structure that demonstrates how a dynamic interaction between the preference for belief and common structures of epistemic communities can turn very small unequal distributions of initial beliefs into full-blown polarization.

## HOW IT WORKS

Hitting "setup" will create a network of agents of the specified type (either random or small world). The degree distribution and clustering distributions ("bubble" coefficeint) are then shown to the right of the depicted network. White edges are connections between agents that may or may not share ideologies. Colored edges (purple or orange) are connections between agents that have the same ideology. There are five ideologies: 1 very liberal, 2 liberal, 3 moderate, 4 conservative, 5 very conservative. Purple (/violet) is the color used for both liberal categories, and orange for the conservatives.

Agents have three belief states: empty-belief, belief-yes, and belief-no. By default at the beginning, all agents have empty-belief, i.e. they have not formed a belief about some proposition S. A few agents in the community receive ``news'' about S, and some will form belief-yes and others belief-no. From these initial seeding conditions, the process of belief spread consists of cycling through the following two steps:

    1. Agents with belief-yes or belief-no invite their empty-belief friends in their epistemic community to accept their belief, b.
    2. Agents with empty-belief and being invited to believe b will either: i) adopt b if it is congruent with their ideology, or ii) if b is incongruent with their ideology, adopt b with probability 1 minus motivated-bias.

## HOW TO USE IT

First decide on the network type. Then decide on how to seed the news to the network using "StartType". "P" is a purple (violet in Netlogo) agent, "O" is an orange agent, and "M" is a moderate agent. The lower case letters following these uppercase letters indicates how many token beliefs are to be assigned. "n" means a belief-no and "y" means a belief-yes. So "PnOyyn" means one belief-no was assigned to a purple agent, two belief-yes were assigned to two orange agents, and a belief-no was assigned to an orange agent.

Click "setup". Then look at the network properties in the different monitors and distribution plots.

Decide on how much motivated bias agents should have. 0 means agents will accept any invitation to believe, regardless of whether that belief is congruent with their ideology ("yes" is associated with purple and "no" with orange). 1 means agents will only accept invitations to believe something that is congruent with their ideology. The "motivated-bias" slider controls the middle ideologies (e.g. orange) and "max-motivated-bias controls the extreme ideologies (e.g. very orange).

Clicking "go" will run the model until all agents have formed some belief or other, using the two step cycle described above.

## THINGS TO NOTICE

Any single run has a fair bit of stochasticity, but you should notice that typically a correlation develops between beliefs and ideology (e.g. "yes" given ideology purple (or "lib")) for different settings of start type and motivated bias. 

## THINGS TO TRY

When motivated bias is high, the correlation between beliefs and ideology tends to be high. But even when motivated bias is low, certain start conditions, e.g. PyyOnn, can still generate these correlations.


## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Preliminary" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>count turtles with [color = red and belief = 1] / count turtles with [belief = 1]</metric>
    <metric>count turtles with [color = green and belief = 2] / count turtles with [belief = 2]</metric>
    <metric>count turtles with [color = red and belief = 1]</metric>
    <metric>count turtles with [color = red and belief = 2]</metric>
    <metric>count turtles with [color = green and belief = 2]</metric>
    <metric>count turtles with [color = green and belief = 1]</metric>
    <enumeratedValueSet variable="small-world?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-seed-info">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-start?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IdeologyFilterUniformBias" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <enumeratedValueSet variable="small-world?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-seed-info">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-start?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IdeologyFilterDifferentialBias" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <enumeratedValueSet variable="small-world?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-seed-info">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-motivated-bias">
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-start?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="AsymmetricStarts" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <enumeratedValueSet variable="max-motivated-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-seed-info">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-start?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-start?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="StartScenarios" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <enumeratedValueSet variable="max-motivated-bias">
      <value value="0"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StartType">
      <value value="&quot;PyyOnn&quot;"/>
      <value value="&quot;PynOyn&quot;"/>
      <value value="&quot;PnnOyy&quot;"/>
      <value value="&quot;Myynn&quot;"/>
      <value value="&quot;Myyyn&quot;"/>
      <value value="&quot;Mynnn&quot;"/>
      <value value="&quot;Pyynn&quot;"/>
      <value value="&quot;Pyyyn&quot;"/>
      <value value="&quot;Pynnn&quot;"/>
      <value value="&quot;PyyyOn&quot;"/>
      <value value="&quot;PyyOyn&quot;"/>
      <value value="&quot;PyOyyn&quot;"/>
      <value value="&quot;PnOyyy&quot;"/>
      <value value="&quot;Oynnn&quot;"/>
      <value value="&quot;Oyynn&quot;"/>
      <value value="&quot;Oyyyn&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IncongruentPnnOyy" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <steppedValueSet variable="max-motivated-bias" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="StartType">
      <value value="&quot;PnnOyy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="IncongruentPnOyyy" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>average-bubble</metric>
    <metric>Yes.vlib</metric>
    <metric>Yes.lib</metric>
    <metric>Yes.m</metric>
    <metric>Yes.con</metric>
    <metric>Yes.vcon</metric>
    <metric>No.vlib</metric>
    <metric>No.lib</metric>
    <metric>No.m</metric>
    <metric>No.con</metric>
    <metric>No.vcon</metric>
    <steppedValueSet variable="max-motivated-bias" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="StartType">
      <value value="&quot;PnOyyy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
