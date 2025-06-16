 ;##################################################################################
; Author: Juan Pablo Ospina-López
; National University of Colombia
; Ph.D Thesis: A computational Justice model for resource distribution in ad hoc networks
; e-mail: jpospinalo@unal.edu.co
;##################################################################################

extensions [matrix] ;;enable us to use matrix inside the simulation

breed [players player]  ;; players of the prisoner's dilemma
breed [institutions institution]  ;; set of rules for controling the resource distribution


globals [

  ;;total score of all turtles playing each strategy
  random-score
  cooperate-score
  defect-score
  current-game
  last-game
  resource-1
  total-resource-1
  resource-2
  total-resource-2
  gini-index
  w1
  w1b
  w1c
  w2
  w3
  w4
  w5
  w6

]

players-own [
  utility ;; utility value of each agent during the games
  a  ; the relative utilities of getting resources that are needed
  b ; the relative utilities of getting resources that are not needed
  c  ;the relative utilities of getting resources that are needed
  alpha         ; coefficient in [0,1] that determine the reinforcement of satisfaction
  beta          ; coefficient in [0,1] that determine the reinforcement of dissatisfaction
  strategy ;;
  trust-history
  cooperate?  ; strategy for the agents
  satisfaction-threshold  ;
  satisfaction-level
  m; number of unsatisfied rounds; if satisfaction-level > satisfaction-threshol for m consecutive rounds
   ; the agent change his strategy to defect
  cooperate-now? ; Behavior of the institution in the instant (t); In this context,
                 ; cooperate means allocate at least the same amount of resources that are needed
  m-threshold
  total-allocation
  current-allocation
  total-demand
  current-resources
  current-need
  total-need
  current-demand
  total-provision
  current-provision
  total-rounds-as-prosumer
  total-rounds-as-head
  total-appropriation
  current-appropriation
  compliant-current-round
  total-compliant-rounds
  borda-points
  total-rounds
  role
  p-cheat
  cheat-on
]

institutions-own [
  allocation-method-2
  collective-choice-method


]

;##################################################################################
;############################# setup procedures ###################################
;##################################################################################

to setup
  clear-all
  setup-environment
  setup-players
  setup-institutions
  setup-game
  reset-ticks
end


;##################################################################################

to setup-environment
  ask patches [ if (pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) [set pcolor green]]
  set resource-1 1
  set resource-2 1
  set total-resource-1 resource-1
  ask patches [ if (pxcor < -1 and pxcor > -16) and (pycor > 1 and pycor < 16) [set pcolor green]]
end

;##################################################################################

to setup-players

  ; basic configuration parameters
  create-players players-number
  ask players [set size 2.5 set shape "person farmer" set color red set utility 0]
  ask players [setxy (2 + random 14) (2 + random 14) ]
  ask players [set strategy "institutional-agreement"]
  ;ask players [set strategy "random"]

 ; history records of allocation, demand, provision, appropiation and compliant rounds
  ask players [
    set total-allocation 0
    set total-demand 0
    set total-provision 0
    set total-appropriation 0
    set total-compliant-rounds 0
    set total-rounds-as-head 0
    set total-rounds-as-prosumer 0
    set total-need 0
  ]
  ask players [
    set current-demand 0
    set current-appropriation 0
    set current-allocation 0
    set current-provision 0
    set current-need 0
  ]

  ;These parameters are set according to the constraints c > a ≥ b.
  ;This creates the collective dilemma, which individuals are incentivised to defect,
  ;but the optimal strategy for the collective is full compliance
  ask players [set a 2 set b 1 set c 3]

  ;This set of values represents a moderate strategy where agents will wait several rounds without a satisfactory
  ;allocation before leaving.;
  ask players [
    set alpha 0.1
    set beta beta-agent
    set satisfaction-threshold s-threshold
    set satisfaction-level precision (0.5) 5  ;we can use a random satisfaction level
    set m 0
    set m-threshold threshold
  ]

  let agent-set ([who] of players)
  let agents-need  (n-values count(players) [ i -> precision (random-float 1) 5])
  let total sum(agents-need)

  (foreach agent-set agents-need
    [ [i alloc] ->
       ask player i [
           set current-need precision ((alloc / total ) * scarcity) 7
    ]
  ])

  ask players [set current-demand current-need]

  ask players [set current-resources precision random-float 1 5 ]

  ask players [set cheat-on false set p-cheat 0]

  ask n-of no-compliant-agent players [set cheat-on true set p-cheat 0.25]


end

;##################################################################################

to setup-institutions
  create-institutions 1
  ask institutions [set size 2.5 set shape "house" set color violet]
  ask institutions [setxy 9 0 ]
  ask institutions [set allocation-method-2 allocation-method]

  set w1 (1 / 8)
  set w1b (1 / 8)
  set w1c (1 / 8)
  set w2 (1 / 8)
  set w3 (1 / 8)
  set w4 (1 / 8)
  set w5 (1 / 8)
  set w6 (1 / 8)

end

;##################################################################################

to setup-game
  set current-game  "game-1"
end

;##################################################################################
;############################# runtime procedures ;################################
;##################################################################################

to go
  tick
  play-a-round
  ;legitimate-claims
  set total-resource-1 total-resource-1 + resource-1


  if (ticks >= 500) [stop]
end

;##################################################################################

to play-a-round

  update-player-parameterers
  ask players [perform-strategy]
  ask institutions [perform-allocation]
  ask players [play-game]
  update-game
  update-compliant-rounds
  update-resource-1
  update-resource-2
  ifelse ticks > 1 [set gini-index compute-gini-index][set gini-index 0]
end

;##################################################################################

to update-player-parameterers

   ask players [
   set total-demand (total-demand + current-demand)
   set total-appropriation (total-appropriation + current-appropriation)
   set total-allocation (total-allocation + current-allocation)
   set total-provision (total-provision + current-provision)
   set total-need (total-need + current-need)
   ]

  ask players [
    set current-demand 0
    set current-appropriation 0
    set current-allocation 0
    set current-provision 0
    set current-need 0
  ]

  ask players [set current-resources precision random-float 1 5 ]
  ;ask players [set current-need precision random-float ((2 * resource-1 / count(players))* scarcity) 4]

  let agent-set ([who] of players)
  let agents-need  (n-values count(players) [ i -> precision (random-float 1) 5])
  let total sum(agents-need)

  (foreach agent-set agents-need
    [ [i alloc] ->
       ask player i [
        set current-need precision ((alloc / total ) * scarcity) 7
    ]
  ])


   ;let p (random-float 1)

   ask players [
    let p (random-float 1)
    ifelse (cheat-on = true and cheating-method = "demand" and p < p-cheat)[
      set current-demand current-need + (random-float 1) * (1 - current-need)
      ][
      set current-demand current-need
      ]
    ]

  ;ask players [set current-demand current-need]


  ask players [set current-provision current-resources]

end

;##################################################################################

to perform-allocation ;;turtle procedure
  if allocation-method = "ostrom-random-allocation" [ostrom-random-allocation]
  if allocation-method = "legitimate-claims-S0" [legitimate-claims-S0]
  if allocation-method = "legitimate-claims" [legitimate-claims]
  if allocation-method = "greedy-random-allocation" [greedy-random-allocation]
  if allocation-method = "equal-allocation" [equal-allocation]
  if allocation-method = "legitimate-claims-Pitt" [legitimate-claims-Pitt]
  if allocation-method = "legitimate-claims-fixed-pitt" [legitimate-claims-fixed-pitt]


  legitimate-claims-fixed-pitt

end

;##################################################################################

to perform-strategy ;;turtle procedure
  if strategy = "random" [ act-randomly ]
  if strategy = "cooperate" [ cooperate ]
  if strategy = "defect" [ defect ]
  if strategy = "institutional-agreement" [institutional-agreement]
end

;##################################################################################

to play-game
  ifelse current-game = "game-1" [
    get-payoff-game1
  ] [
    get-payoff-game2
  ]
end

;##################################################################################

to update-game ;; change the state of the game according to the agents' cooperation level
  if (current-game = "game-1" and count players with [cooperate? = true] != players-number ) [set current-game  "game-2" set last-game "game-1"]
  if (current-game = "game-2" and count players with [cooperate? = true]  = players-number ) [set current-game  "game-1" set last-game "game-2"]
end

;##################################################################################

to update-resource-1
 ;If regeneration-rate = 1 the resources regenerate the 100% of their original value
    ifelse (current-game = "game-2")
    [
      let total-patches patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16)]
      let green-patches patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) and pcolor = green]
      let lost-patches round(count(green-patches) * degeneration-rate)
      ask n-of lost-patches green-patches [ set pcolor brown ]
      set resource-1 round(((count(green-patches) - lost-patches)  / count(total-patches)))
    ]
    [
     let total-patches patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) ]
     let lost-patches patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) and pcolor = brown ]
     let new-patches ceiling((count patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) and pcolor = brown ] )
          * regeneration-rate)

     let green-patches patches with [(pxcor > 1 and pxcor < 16) and (pycor > 1 and pycor < 16) and pcolor = green]
     set resource-1 round((count(green-patches) / count(total-patches)))
     ask n-of new-patches lost-patches [ set pcolor green ]
    ]
end

;##################################################################################

to update-resource-2

end

;##################################################################################

to update-compliant-rounds
  ;ask players [set current-appropriation random 2]
  ;ask players [set compliant-current-round random 2]
  ask players [ifelse ( (current-appropriation <= current-allocation)
    and (current-demand <= current-need)
    and (current-provision = current-resources)) [set compliant-current-round 1 ][set compliant-current-round 0]]
  ask players [set total-compliant-rounds total-compliant-rounds + compliant-current-round ]
end

;##################################################################################

to get-payoff-game1 ;; game in which everyone cooperate (it has a greater payoff)
  ; This model was taken from Hilbe, Christian, et al.
  ; "Evolution of cooperation in stochastic games." Nature 559.7713 (2018): 246.
  ;show "playing game-1"

    ifelse cooperate? [
     ifelse cooperate-now? [
       set utility (utility + 1 * resource-1)
       set label precision utility 1
       ] [
       set utility (utility + -1 * resource-1)
       set label precision utility  1
       ]
     ] [
       ifelse cooperate-now? [
       set utility (utility + 2 * resource-1)
       set label precision utility 1
       ] [
       set utility (utility + 0)
       set label precision utility 1
       ]
    ]
end

;##################################################################################

to get-payoff-game2 ;; IF any of the players do not cooperate the game has lower payoff than in game 1)
 ; This model was taken from Hilbe, Christian, et al.
 ; "Evolution of cooperation in stochastic games." Nature 559.7713 (2018): 246.
 ;show "playing game-2"
  ifelse cooperate? [
     ifelse cooperate-now? [
       set utility (utility + 0.2 * resource-1)
       set label precision utility 1
       ] [
       set utility (utility + -1 * resource-1)
       set label precision utility 1
       ]
     ] [
       ifelse cooperate-now? [
       set utility (utility + 1.2 * resource-1)
       set label precision utility 1
       ] [
       set utility (utility + 0)
       set label precision utility 1
       ]
    ]
end


;##################################################################################
;############################### strategies #######################################
;##################################################################################

to act-randomly
  ifelse (random-float 1.0 < 0.5) [
    set cooperate? false
  ] [
    set cooperate? true
  ]
end

;##################################################################################

to cooperate
  set cooperate? true
end

;##################################################################################

to defect
  set cooperate? false
end

;##################################################################################

to institutional-agreement
  ; the strategy  depends on the number of rounds in which the agent
  ; has got the amount of resources that are needed

  let p random-float 1

  ifelse (m < m-threshold) [


    ifelse (cheat-on = true and cheating-method = "appropriation" and p < p-cheat) [
      set cooperate? false][
      set cooperate? true
    ]
  ][
    set cooperate? false
  ]
end

;##################################################################################

to update-satisfaction-level
  ; update the satisfaction level of an agent according to the last allocation and
  ; his needs
  ifelse ((current-allocation >= current-need )) [
    set satisfaction-level (satisfaction-level + alpha * (1 - satisfaction-level))
    set cooperate-now? true

  ]
  [
   set satisfaction-level (satisfaction-level - beta * satisfaction-level)
   set cooperate-now? false

  ]
  ; uptade m according to the agent's satistafction level
  ifelse ((satisfaction-level > satisfaction-threshold)) [ set m 0 ] [ set m m + 1 ]

end


;##################################################################################
;######################### allocation methods ;####################################
;##################################################################################

to ostrom-random-allocation
; this allocation method uses the 2 principle of self-organizing open institutions proposed for Elionor Ostrom

  let r1 resource-1
  let agentset ([who] of players)
  let allocation  (n-values count(players) [ i -> random-float 1])
  let total sum(allocation)

    (foreach agentset allocation
    [ [i alloc] ->
        ask player i [

            set current-allocation (alloc / total )
            set current-appropriation current-allocation
    ]
  ])

  set r1 0

  ask players [update-satisfaction-level]

end

;#############################################################################


to greedy-random-allocation
  let r1 resource-1
  let agentset ([who] of players)
  let allocation  (n-values count(players) [ i -> random-float 1])
  let total sum(allocation)

    (foreach agentset allocation
    [ [i alloc] ->
        ask player i [
            ifelse ((r1 > alloc) and r1 != 0)[
            ;show who
               set current-allocation alloc
               set current-appropriation current-allocation
               set r1 (r1 - alloc)
            ][

               set current-allocation r1
               set r1 0

             ]
    ]
  ])

    if (r1 > 0)[
    ask players [
      set current-allocation (current-allocation + r1 / count(players))
      set current-appropriation (current-appropriation + r1 / count(players))
    ]
    set r1 0
  ]

  ask players [update-satisfaction-level]

end

;##################################################################################


to equal-allocation

  let r1 resource-1
  let temp (r1 / count(players))

    ask players [
      set current-allocation temp
      set current-appropriation current-allocation
    ]
    set r1 0

  ask players [update-satisfaction-level]

end

;##################################################################################



to max-min-allocation

  ask players [update-satisfaction-level]

end

;##################################################################################

to legitimate-claims

  let r1 resource-1 ; temporal variable who represents the current value of the resource-1
  select-prosumer
  select-head


  ;show "current demand"
  ;ask players [show current-demand]
  ;show "borda"
  let bbb borda-count
  ;show bbb

  ; allocation process according to the Borda count results

  (foreach bbb
    [ agent ->
        ask agent[
            ifelse (r1 > current-demand)[
               ifelse cooperate?[
                  set current-allocation current-demand
                  set current-appropriation current-allocation
                  set r1 (r1 - current-appropriation)

                ][
                  set current-allocation current-demand
                  set current-appropriation current-allocation + random-float (1 - current-allocation)
                  set r1 (r1 - current-appropriation)
                ]

            ][
               set current-allocation r1
               set r1 0
          ]
    ]
  ])

  ; if there is any remainder of the resource after the allocation process, it is divided equally among the agents
  if (r1 > 0)[
    ask players [set current-allocation (current-allocation + r1 / count(players))]
    ask players [set current-appropriation (current-appropriation + r1 / count(players))]
    set r1 0
  ]

  ask players [update-satisfaction-level]

end

;##################################################################################



to legitimate-claims-fixed-pitt

  let r1 resource-1 ; temporal variable who represents the current value of the resource-1
  select-prosumer
  select-head


  ;show "current demand"
  ;ask players [show current-demand]
  ;show "borda"
  let bbb borda-fixed-pitt
  ;show bbb

  ; allocation process according to the Borda count results

  (foreach bbb
    [ agent ->
        ask agent[
            ifelse (r1 > current-demand)[
               ifelse cooperate?[
                  set current-allocation current-demand
                  set current-appropriation current-allocation
                  set r1 (r1 - current-appropriation)

                ][
                  set current-allocation current-demand
                  set current-appropriation current-allocation + random-float (1 - current-allocation)
                  set r1 (r1 - current-appropriation)
                ]

            ][
               set current-allocation r1
               set r1 0
          ]
    ]
  ])

  ; if there is any remainder of the resource after the allocation process, it is divided equally among the agents
  if (r1 > 0)[
    ask players [set current-allocation (current-allocation + r1 / count(players))]
    ask players [set current-appropriation (current-appropriation + r1 / count(players))]
    set r1 0
  ]

  ask players [update-satisfaction-level]

end

;##################################################################################





to legitimate-claims-S0

  let r1 resource-1 ; temporal variable who represents the current value of the resource-1
  select-prosumer
  select-head

  let bbb borda-count-SO

  ; allocation process according to the Borda count results

  (foreach bbb
    [ agent ->
        ask agent[
            ifelse (r1 > current-demand)[
               ifelse cooperate?[
                  set current-allocation current-demand
                  set current-appropriation current-allocation
                  set r1 (r1 - current-appropriation)

                ][
                  set current-allocation current-demand
                  set current-appropriation current-allocation + (random-float 1) * (1 - current-allocation)
                  set r1 (r1 - current-appropriation)
                ]

            ][
               set current-allocation r1
               set r1 0
          ]
    ]
  ])

  ; if there is any remainder of the resource after the allocation process, it is divided equally among the agents
  if (r1 > 0)[
    ask players [set current-allocation (current-allocation + r1 / count(players))]
    ask players [set current-appropriation (current-appropriation + r1 / count(players))]
    set r1 0
  ]

  ask players [update-satisfaction-level]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to legitimate-claims-Pitt

  let r1 resource-1 ; temporal variable who represents the current value of the resource-1
  select-prosumer
  select-head

  let bbb borda-count-Pitt

  ; allocation process according to the Borda count results

  (foreach bbb
    [ agent ->
        ask agent[
            ifelse (r1 > current-demand)[
               ifelse cooperate?[
                  set current-allocation current-demand
                  set current-appropriation current-allocation
                  set r1 (r1 - current-appropriation)
                  set total-rounds (total-rounds + 1)

                ][
                  set current-allocation current-demand
                  set current-appropriation current-allocation + (random-float 1) * (1 - current-allocation)
                  set r1 (r1 - current-appropriation)
                  set total-rounds (total-rounds + 1)
                ]

            ][
               set current-allocation r1
               set r1 0
          ]
    ]
  ])

  ; if there is any remainder of the resource after the allocation process, it is divided equally among the agents
  if (r1 > 0)[
    ask players [set current-allocation (current-allocation + r1 / count(players))]
    ask players [set current-appropriation (current-appropriation + r1 / count(players))]
    set r1 0
  ]

  ask players [update-satisfaction-level]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to select-head
  ;In this case, the appoint action is actually an arrogation rather than an assignment of power.
  ;However,such details are not essential to the analysis of self-organisation with legitimate claims.
  ask one-of players [set total-rounds-as-head (total-rounds-as-head + 1) set utility utility - 1 ]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to select-prosumer
     ;In this case, the appoint action is actually an arrogation rather than an assignment of power.
  ;However,such details are not essential to the analysis of self-organisation with legitimate claims.
  ask one-of players [set total-rounds-as-prosumer (total-rounds-as-prosumer + 1) set utility utility - 1 ]


end


;##################################################################################

to-report compute-gini-index

  ; This method reports the Gini index according to the allocated resources.
  ;

  let agents ([who] of players)

  ; Total resources per agent
  let allocation []
  (foreach agents [ [i] -> set allocation lput ([total-appropriation] of player i) allocation])

  ; Cumulative population of agents
  let cumulative-agent []
  let i 1
  set cumulative-agent lput (1 / count(players)) cumulative-agent
  while [ i < count(players)]
  [
   set cumulative-agent lput ((1 / count(players)) + (item (i - 1) cumulative-agent) ) cumulative-agent

   set i i + 1
    ]

  ; Cumulative allocation per agent
  let cumulative-allocation []
  set i 1
  set cumulative-allocation lput ((item 0 (allocation))/ sum(allocation)) cumulative-allocation

  while [ i < count(players)]
  [

   set cumulative-allocation lput (((item i allocation) / sum(allocation)) +
      (item (i - 1) cumulative-allocation)) cumulative-allocation
   set i i + 1

    ]
  ; A
  let a-gini []
  set i 1
  set a-gini lput (item 0 cumulative-agent) a-gini
  while [ i < count(players)]
  [
   set a-gini lput ((item i cumulative-agent) - (item (i - 1) cumulative-agent)) a-gini
   set i i + 1
    ]

  ;B
  let b-gini []
  set i 1
  set b-gini lput (item 0 cumulative-allocation) b-gini
  while [ i < count(players)]
  [
   set b-gini lput ((item i cumulative-allocation) + (item (i - 1) cumulative-allocation)) b-gini
   set i i + 1
    ]

let ab []
  set i 0
  while [ i < count(players)]
  [
   set ab lput ((item i a-gini) * (item i b-gini)) ab
   set i i + 1
    ]

 report  abs (precision (1 - sum(ab)) 3)


end

  ;##################################################################################


to-report borda-count-SO

  let agents-vote matrix:make-constant count(players) 7 0
  let score matrix:make-constant count(players) 7 0
  let final-score matrix:make-constant count(players) 7 0
  let pos matrix:make-constant count(players) 7 0


  let t-equality-satisfaction equality-satisfaction
  let t-equality-allocation equality-allocation
  let t-need need
  let t-productivity-avg-provision productivity-avg-provision
  let t-effort effort
  let t-social-utility social-utility
  let t-supply-and-demand supply-and-demand


  ;##################################################################################
  ;######################## Institution vote ########################################
  ;##################################################################################

  ask players [set borda-points 0]

  ; borda points for f_1a: the canon of equality
  (foreach t-equality-satisfaction (range (count(players) ) 0 -1)
    [ [agent f_1a-points] -> ask agent [set borda-points borda-points + (w1 * f_1a-points)]])

  ; borda points for f_1b: the canon of equality
  (foreach t-equality-allocation (range (count(players) ) 0 -1)
    [ [agent f_1b-points] -> ask agent [set borda-points borda-points + (w1b * f_1b-points)]])

  ; borda points for f_2: the canon of need
  (foreach t-need (range (count(players) ) 0 -1)
    [ [agent f_2-points] -> ask agent [set borda-points borda-points + (w2 * f_2-points) ]])

  ; borda points for f_3: the canon of productivity
  (foreach t-productivity-avg-provision (range (count(players) ) 0 -1)
    [ [agent f_3-points] -> ask agent [set borda-points borda-points + (w3 * f_3-points) ]])

   ;borda points for f_4: the canon of effort (prosumer)
  (foreach t-effort (range (count(players) ) 0 -1)
    [ [agent f_4-points] -> ask agent [set borda-points borda-points + (w4 * f_4-points) ]])

  ; borda points for f_5: the canon of social utility (head)
  (foreach t-social-utility (range (count(players) ) 0 -1)
    [ [agent f_5-points] -> ask agent [set borda-points borda-points + (w5 * f_5-points) ]])

    ; borda points for f_6: the canon of supply-and-demand
  (foreach t-supply-and-demand (range (count(players) ) 0 -1)
    [ [agent f_6-points] -> ask agent [set borda-points borda-points + (w6 * f_6-points) ]])


  ;#############################################################################
  ;######################## agents vote ########################################
  ;#############################################################################


  foreach t-equality-satisfaction [
    x -> let i [who] of x
    let j ((length t-equality-satisfaction) - (position x t-equality-satisfaction))
    matrix:set agents-vote i 0 j
  ]

  foreach t-equality-allocation [
    x -> let i [who] of x
    let j ((length t-equality-allocation) - (position x t-equality-allocation))
    matrix:set agents-vote i 1 j
  ]


  foreach t-need [
    x -> let i [who] of x
    let j ((length t-need) - (position x t-need))
    matrix:set agents-vote i 2 j
  ]

  foreach t-productivity-avg-provision [
    x -> let i [who] of x
    let j ((length t-productivity-avg-provision) - (position x t-productivity-avg-provision))
    matrix:set agents-vote i 3 j
  ]

  foreach t-effort [
    x -> let i [who] of x
    let j ((length t-effort) - (position x t-effort))
    matrix:set agents-vote i 4 j
  ]

  foreach t-social-utility [
    x -> let i [who] of x
    let j ((length t-social-utility) - (position x t-social-utility))
    matrix:set agents-vote i 5 j
  ]

  foreach t-supply-and-demand [
    x -> let i [who] of x
    let j ((length t-supply-and-demand) - (position x t-supply-and-demand))
    matrix:set agents-vote i 6 j
  ]
;#############################################################################


  let k 0
  while [k < count(players) ][
    let j 0
    let temp matrix:get-row agents-vote k
    foreach range(7) [
      x -> let val (position max(temp) temp)
           let val2 (item val temp)
           matrix:set pos k j (val + 1)
           matrix:set score k j val2
           set temp replace-item val temp -1
           set j (j + 1)
    ]
    set k (k + 1)
  ]

 ;#############################################################################
  let p 0
  while [p < count (players)][
    let j 0
    let l 1
    let temp matrix:get-row score p
    while [j < (length (temp) - 1 )][
      ifelse ( (item j temp) != (item (j + 1) temp)) [
        matrix:set final-score p j (length (temp) - j)
      ][
        let repeated? true
        let val (item j temp)
        let first-value j
        let last-value (j + 1)
        let flag []
        let t-score []
        set flag lput (first-value) flag
        set t-score lput (length(temp) - first-value) t-score

        while [repeated? ][

          ifelse (val = (item last-value temp) and (last-value < (length (temp) - 1 )))[
            set flag lput (last-value) flag
            set t-score lput (length(temp) - last-value) t-score
            set last-value (last-value + 1)
          ][

             if ( ((item (length(temp) - 1 ) temp) = (item (length(temp) - 2 ) temp) and item (length(temp) - 1 ) temp = val) ) [
               set flag lput (length(temp) - 1 ) flag
               set t-score lput (length(temp) - (length(temp) - 1 )) t-score
             ]

             foreach flag [
               x -> matrix:set final-score p x (sum(t-score) / length(flag) )
             ]
             set j last-value - 1
             set repeated? false
           ]
         ]
       ]
    set j (j + 1)
    set l (l + 1)
    if ( (item (length(temp) - 2 ) temp) != (item (length(temp) - 1 ) temp)) [matrix:set final-score p (length (temp) - 1) 1]
    ]
    set p (p + 1)
    ]

  ;#############################################################################

  let borda-w1 0
  let borda-w1b 0
  let borda-w2 0
  let borda-w3 0
  let borda-w4 0
  let borda-w5 0
  let borda-w6 0

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 1 temp )
         set borda-w1 (borda-w1 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 2 temp )
         set borda-w1b (borda-w1b + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 3 temp )
         set borda-w2 (borda-w2 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 4 temp )
         set borda-w3 (borda-w3 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 5 temp )
         set borda-w4 (borda-w4 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 6 temp )
         set borda-w5 (borda-w5 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 7 temp )
         set borda-w6 (borda-w6 + (matrix:get final-score x y))
  ]

  ;#############################################################################

  ;after the voting process, the values of W are updated
  let total-borda-w (borda-w1 + borda-w1b + borda-w2 + borda-w3 + borda-w4 + borda-w5 + borda-w6 )
  let avg-borda-w (total-borda-w / 7)

  set w1 (w1 + w1 * ((borda-w1 - avg-borda-w) / total-borda-w))
  set w1b (w1b + w1b * ((borda-w1b - avg-borda-w) / total-borda-w))
  set w2 (w2 + w2 * ((borda-w2 - avg-borda-w) / total-borda-w))
  set w3 (w3 + w3 * ((borda-w3 - avg-borda-w) / total-borda-w))
  set w4 (w4 + w4 * ((borda-w4 - avg-borda-w) / total-borda-w))
  set w5 (w5 + w5 * ((borda-w5 - avg-borda-w) / total-borda-w))
  set w6 (w6 + w6 * ((borda-w6 - avg-borda-w) / total-borda-w))

  let total-w (w1 + w1b + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


;#############################################################################
;######################## Hamming Distance ###################################
;#############################################################################


  let hamming-w1 0
  let hamming-w1b 0
  let hamming-w2 0
  let hamming-w3 0
  let hamming-w4 0
  let hamming-w5 0
  let hamming-w6 0

  (foreach t-equality-satisfaction (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w1 (hamming-w1 + 1)]])

  (foreach t-equality-allocation (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w1b (hamming-w1b + 1)]])

  (foreach t-need (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w2 (hamming-w2 + 1)]])

  (foreach t-productivity-avg-provision (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w3 (hamming-w3 + 1)]])

  (foreach t-effort (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w4 (hamming-w4 + 1)]])

  (foreach t-social-utility (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w5 (hamming-w5 + 1)]])

  (foreach t-supply-and-demand (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w6 (hamming-w6 + 1)]])


  let total-hamming (hamming-w1 + hamming-w1b + hamming-w2 + hamming-w3 + hamming-w4 + hamming-w5 + hamming-w6)
  let avg-hamming (total-hamming / 7)

  set w1 (w1 + w1 * ((hamming-w1 - avg-hamming) / total-hamming))
  set w1b (w1b + w1b * ((hamming-w1b - avg-hamming) / total-hamming))
  set w2 (w2 + w2 * ((hamming-w2 - avg-hamming) / total-hamming))
  set w3 (w3 + w3 * ((hamming-w3 - avg-hamming) / total-hamming))
  set w4 (w4 + w4 * ((hamming-w4 - avg-hamming) / total-hamming))
  set w5 (w5 + w5 * ((hamming-w5 - avg-hamming) / total-hamming))
  set w6 (w6 + w6 * ((hamming-w6 - avg-hamming) / total-hamming))

  set total-w (w1 + w1b + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


;#############################################################################
;######################## Restore Equilibrium state ##########################
;#############################################################################


  set w1 (w1 + restore-equilibrium * ((1 / 7) - w1))
  set w1b (w1b + restore-equilibrium * ((1 / 7) - w1b))
  set w2 (w2 + restore-equilibrium * ((1 / 7) - w2))
  set w3 (w3 + restore-equilibrium * ((1 / 7) - w3))
  set w4 (w4 + restore-equilibrium * ((1 / 7) - w4))
  set w5 (w5 + restore-equilibrium * ((1 / 7) - w5))
  set w6 (w6 + restore-equilibrium * ((1 / 7) - w6))


  set total-w (w1 + w1b + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


  report sort-on [(- borda-points)] players

end


  ;##################################################################################




to-report borda-count-Pitt

  let agents-vote matrix:make-constant count(players) 8 0
  let score matrix:make-constant count(players) 8 0
  let final-score matrix:make-constant count(players) 8 0
  let pos matrix:make-constant count(players) 8 0


  let t-equality-satisfaction equality-satisfaction  ;#canon f1_a
  let t-equality-allocation equality-allocation      ;#canon f1_a
  let t-equality-rounds equality-rounds
  let t-need need
  let t-productivity-avg-provision productivity-avg-provision
  let t-effort effort
  let t-social-utility social-utility
  let t-supply-and-demand supply-and-demand


  ;##################################################################################
  ;######################## Institution vote ########################################
  ;##################################################################################

  ask players [set borda-points 0]

  ; borda points for f_1a: the canon of equality - satisfaction
  (foreach t-equality-satisfaction (range (count(players) ) 0 -1)
    [ [agent f_1a-points] -> ask agent [set borda-points borda-points + (w1 * f_1a-points)]])


  ; borda points for f_1b: the canon of equality - allocation
  (foreach t-equality-allocation (range (count(players) ) 0 -1)
    [ [agent f_1b-points] -> ask agent [set borda-points borda-points + (w1b * f_1b-points)]])


  ; borda points for f_1c: the canon of equality - rounds
  (foreach t-equality-rounds (range (count(players) ) 0 -1)
    [ [agent f_1c-points] -> ask agent [set borda-points borda-points + (w1 * f_1c-points)]])

  ; borda points for f_2: the canon of need
  (foreach t-need (range (count(players) ) 0 -1)
    [ [agent f_2-points] -> ask agent [set borda-points borda-points + (w2 * f_2-points) ]])

  ; borda points for f_3: the canon of productivity
  (foreach t-productivity-avg-provision (range (count(players) ) 0 -1)
    [ [agent f_3-points] -> ask agent [set borda-points borda-points + (w3 * f_3-points) ]])

   ;borda points for f_4: the canon of effort (prosumer)
  (foreach t-effort (range (count(players) ) 0 -1)
    [ [agent f_4-points] -> ask agent [set borda-points borda-points + (w4 * f_4-points) ]])

  ; borda points for f_5: the canon of social utility (head)
  (foreach t-social-utility (range (count(players) ) 0 -1)
    [ [agent f_5-points] -> ask agent [set borda-points borda-points + (w5 * f_5-points) ]])

    ; borda points for f_6: the canon of supply-and-demand
  (foreach t-supply-and-demand (range (count(players) ) 0 -1)
    [ [agent f_6-points] -> ask agent [set borda-points borda-points + (w6 * f_6-points) ]])


  ;#############################################################################
  ;######################## agents vote ########################################
  ;#############################################################################


  foreach t-equality-satisfaction [
    x -> let i [who] of x
    let j ((length t-equality-satisfaction) - (position x t-equality-satisfaction))
    matrix:set agents-vote i 0 j
  ]

  foreach t-equality-allocation [
    x -> let i [who] of x
    let j ((length t-equality-allocation) - (position x t-equality-allocation))
    matrix:set agents-vote i 1 j
  ]

  foreach t-equality-rounds [
    x -> let i [who] of x
    let j ((length t-equality-rounds) - (position x t-equality-rounds))
    matrix:set agents-vote i 2 j
  ]

  foreach t-need [
    x -> let i [who] of x
    let j ((length t-need) - (position x t-need))
    matrix:set agents-vote i 3 j
  ]

  foreach t-productivity-avg-provision [
    x -> let i [who] of x
    let j ((length t-productivity-avg-provision) - (position x t-productivity-avg-provision))
    matrix:set agents-vote i 4 j
  ]

  foreach t-effort [
    x -> let i [who] of x
    let j ((length t-effort) - (position x t-effort))
    matrix:set agents-vote i 5 j
  ]

  foreach t-social-utility [
    x -> let i [who] of x
    let j ((length t-social-utility) - (position x t-social-utility))
    matrix:set agents-vote i 6 j
  ]

  foreach t-supply-and-demand [
    x -> let i [who] of x
    let j ((length t-supply-and-demand) - (position x t-supply-and-demand))
    matrix:set agents-vote i 7 j
  ]
;#############################################################################


  let k 0
  while [k < count(players) ][
    let j 0
    let temp matrix:get-row agents-vote k
    foreach range(8) [
      x -> let val (position max(temp) temp)
           let val2 (item val temp)
           matrix:set pos k j (val + 1)
           matrix:set score k j val2
           set temp replace-item val temp -1
           set j (j + 1)
    ]
    set k (k + 1)
  ]

 ;#############################################################################
  let p 0
  while [p < count (players)][
    let j 0
    let l 1
    let temp matrix:get-row score p
    while [j < (length (temp) - 1 )][
      ifelse ( (item j temp) != (item (j + 1) temp)) [
        matrix:set final-score p j (length (temp) - j)
      ][
        let repeated? true
        let val (item j temp)
        let first-value j
        let last-value (j + 1)
        let flag []
        let t-score []
        set flag lput (first-value) flag
        set t-score lput (length(temp) - first-value) t-score

        while [repeated? ][

          ifelse (val = (item last-value temp) and (last-value < (length (temp) - 1 )))[
            set flag lput (last-value) flag
            set t-score lput (length(temp) - last-value) t-score
            set last-value (last-value + 1)
          ][

             if ( ((item (length(temp) - 1 ) temp) = (item (length(temp) - 2 ) temp) and item (length(temp) - 1 ) temp = val) ) [
               set flag lput (length(temp) - 1 ) flag
               set t-score lput (length(temp) - (length(temp) - 1 )) t-score
             ]

             foreach flag [
               x -> matrix:set final-score p x (sum(t-score) / length(flag) )
             ]
             set j last-value - 1
             set repeated? false
           ]
         ]
       ]
    set j (j + 1)
    set l (l + 1)
    if ( (item (length(temp) - 2 ) temp) != (item (length(temp) - 1 ) temp)) [matrix:set final-score p (length (temp) - 1) 1]
    ]
    set p (p + 1)
    ]

  ;#############################################################################

  let borda-w1 0
  let borda-w1b 0
  let borda-w1c 0
  let borda-w2 0
  let borda-w3 0
  let borda-w4 0
  let borda-w5 0
  let borda-w6 0

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 1 temp )
         set borda-w1 (borda-w1 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 2 temp )
         set borda-w1b (borda-w1b + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 3 temp )
         set borda-w1c (borda-w1c + (matrix:get final-score x y))
  ]


  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 4 temp )
         set borda-w2 (borda-w2 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 5 temp )
         set borda-w3 (borda-w3 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 6 temp )
         set borda-w4 (borda-w4 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 7 temp )
         set borda-w5 (borda-w5 + (matrix:get final-score x y))
  ]

  foreach range(count(players)) [
    x -> let temp matrix:get-row pos x
         let y (position 8 temp )
         set borda-w6 (borda-w6 + (matrix:get final-score x y))
  ]

  ;#############################################################################

  ;after the voting process, the values of W are updated
  let total-borda-w (borda-w1 + borda-w1b + borda-w1c + borda-w2 + borda-w3 + borda-w4 + borda-w5 + borda-w6 )
  let avg-borda-w (total-borda-w / 8)

  set w1 (w1 + w1 * ((borda-w1 - avg-borda-w) / total-borda-w))
  set w1b (w1b + w1b * ((borda-w1b - avg-borda-w) / total-borda-w))
  set w1c (w1c + w1c * ((borda-w1c - avg-borda-w) / total-borda-w))
  set w2 (w2 + w2 * ((borda-w2 - avg-borda-w) / total-borda-w))
  set w3 (w3 + w3 * ((borda-w3 - avg-borda-w) / total-borda-w))
  set w4 (w4 + w4 * ((borda-w4 - avg-borda-w) / total-borda-w))
  set w5 (w5 + w5 * ((borda-w5 - avg-borda-w) / total-borda-w))
  set w6 (w6 + w6 * ((borda-w6 - avg-borda-w) / total-borda-w))

  let total-w (w1 + w1b + w1c + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w1c (w1c / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


;#############################################################################
;######################## Hamming Distance ###################################
;#############################################################################


  let hamming-w1 0
  let hamming-w1b 0
  let hamming-w1c 0
  let hamming-w2 0
  let hamming-w3 0
  let hamming-w4 0
  let hamming-w5 0
  let hamming-w6 0

  (foreach t-equality-satisfaction (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w1 (hamming-w1 + 1)]])

  (foreach t-equality-allocation (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w1b (hamming-w1b + 1)]])


  (foreach t-equality-rounds (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w1c (hamming-w1c + 1)]])

  (foreach t-need (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w2 (hamming-w2 + 1)]])

  (foreach t-productivity-avg-provision (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w3 (hamming-w3 + 1)]])

  (foreach t-effort (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w4 (hamming-w4 + 1)]])

  (foreach t-social-utility (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w5 (hamming-w5 + 1)]])

  (foreach t-supply-and-demand (sort-on [(- borda-points)] players)
    [[x y] -> if (([who] of x) != ([who] of y)) [set hamming-w6 (hamming-w6 + 1)]])


  let total-hamming (hamming-w1 + hamming-w1b + hamming-w1c + hamming-w2 + hamming-w3 + hamming-w4 + hamming-w5 + hamming-w6)
  let avg-hamming (total-hamming / 7)

  set w1 (w1 + w1 * ((hamming-w1 - avg-hamming) / total-hamming))
  set w1b (w1b + w1b * ((hamming-w1b - avg-hamming) / total-hamming))
  set w1c (w1c + w1c * ((hamming-w1c - avg-hamming) / total-hamming))
  set w2 (w2 + w2 * ((hamming-w2 - avg-hamming) / total-hamming))
  set w3 (w3 + w3 * ((hamming-w3 - avg-hamming) / total-hamming))
  set w4 (w4 + w4 * ((hamming-w4 - avg-hamming) / total-hamming))
  set w5 (w5 + w5 * ((hamming-w5 - avg-hamming) / total-hamming))
  set w6 (w6 + w6 * ((hamming-w6 - avg-hamming) / total-hamming))

  set total-w (w1 + w1b + w1c + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w1c (w1c / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


;#############################################################################
;######################## Restore Equilibrium state ##########################
;#############################################################################


  set w1 (w1 + restore-equilibrium * ((1 / 8) - w1))
  set w1b (w1b + restore-equilibrium * ((1 / 8) - w1b))
  set w1c (w1c + restore-equilibrium * ((1 / 8) - w1c))
  set w2 (w2 + restore-equilibrium * ((1 / 8) - w2))
  set w3 (w3 + restore-equilibrium * ((1 / 8) - w3))
  set w4 (w4 + restore-equilibrium * ((1 / 8) - w4))
  set w5 (w5 + restore-equilibrium * ((1 / 8) - w5))
  set w6 (w6 + restore-equilibrium * ((1 / 8) - w6))


  set total-w (w1 + w1b + w1c + w2 + w3 + w4 + w5 + w6)

  set w1 (w1 / total-w)
  set w1b (w1b / total-w)
  set w1c (w1c / total-w)
  set w2 (w2 / total-w)
  set w3 (w3 / total-w)
  set w4 (w4 / total-w)
  set w5 (w5 / total-w)
  set w6 (w6 / total-w)


  report sort-on [(- borda-points)] players

end


  ;##################################################################################

to-report borda-count

  let t-equality-satisfaction equality-satisfaction
  let t-equality-allocation equality-allocation
  let t-need need
  let t-productivity-avg-provision productivity-avg-provision
  let t-effort effort
  let t-social-utility social-utility
  let t-supply-and-demand supply-and-demand


  ;##################################################################################
  ;######################## Institution vote ########################################
  ;##################################################################################

  ask players [set borda-points 0]

  ; borda points for f_1a: the canon of equality
  (foreach t-equality-satisfaction (range (count(players) ) 0 -1)
    [ [agent f_1a-points] -> ask agent [set borda-points borda-points + (w1 + f_1a-points)]])

  ; borda points for f_1b: the canon of equality
  (foreach t-equality-allocation (range (count(players) ) 0 -1)
    [ [agent f_1b-points] -> ask agent [set borda-points borda-points + (w1b + f_1b-points)]])

  ; borda points for f_2: the canon of need
  (foreach t-need (range (count(players) ) 0 -1)
    [ [agent f_2-points] -> ask agent [set borda-points borda-points + (w2 + f_2-points) ]])

  ; borda points for f_3: the canon of productivity
  (foreach t-productivity-avg-provision (range (count(players) ) 0 -1)
    [ [agent f_3-points] -> ask agent [set borda-points borda-points + (w3 + f_3-points) ]])

   ;borda points for f_4: the canon of effort (prosumer)
  (foreach t-effort (range (count(players) ) 0 -1)
    [ [agent f_4-points] -> ask agent [set borda-points borda-points + (w4 + f_4-points) ]])

  ; borda points for f_5: the canon of social utility (head)
  (foreach t-social-utility (range (count(players) ) 0 -1)
    [ [agent f_5-points] -> ask agent [set borda-points borda-points + (w5 + f_5-points) ]])

    ; borda points for f_6: the canon of supply-and-demand
  (foreach t-supply-and-demand (range (count(players) ) 0 -1)
    [ [agent f_6-points] -> ask agent [set borda-points borda-points + (w6 + f_6-points) ]])

  report sort-on [(- borda-points)] players

end



to-report borda-fixed-pitt

  let t-equality-satisfaction equality-satisfaction
  let t-equality-allocation equality-allocation
  let t-equality-rounds equality-rounds
  let t-need need
  let t-productivity-avg-provision productivity-avg-provision
  let t-effort effort
  let t-social-utility social-utility
  let t-supply-and-demand supply-and-demand


  ;##################################################################################
  ;######################## Institution vote ########################################
  ;##################################################################################

  ask players [set borda-points 0]

  ; borda points for f_1a: the canon of equality
  (foreach t-equality-satisfaction (range (count(players) ) 0 -1)
    [ [agent f_1a-points] -> ask agent [set borda-points borda-points + (w1 + f_1a-points)]])

  ; borda points for f_1b: the canon of equality
  (foreach t-equality-allocation (range (count(players) ) 0 -1)
    [ [agent f_1b-points] -> ask agent [set borda-points borda-points + (w1b + f_1b-points)]])

    ; borda points for f_1c: the canon of equality - rounds
  (foreach t-equality-rounds (range (count(players) ) 0 -1)
    [ [agent f_1c-points] -> ask agent [set borda-points borda-points + (w1 * f_1c-points)]])

  ; borda points for f_2: the canon of need
  (foreach t-need (range (count(players) ) 0 -1)
    [ [agent f_2-points] -> ask agent [set borda-points borda-points + (w2 + f_2-points) ]])

  ; borda points for f_3: the canon of productivity
  (foreach t-productivity-avg-provision (range (count(players) ) 0 -1)
    [ [agent f_3-points] -> ask agent [set borda-points borda-points + (w3 + f_3-points) ]])

   ;borda points for f_4: the canon of effort (prosumer)
  (foreach t-effort (range (count(players) ) 0 -1)
    [ [agent f_4-points] -> ask agent [set borda-points borda-points + (w4 + f_4-points) ]])

  ; borda points for f_5: the canon of social utility (head)
  (foreach t-social-utility (range (count(players) ) 0 -1)
    [ [agent f_5-points] -> ask agent [set borda-points borda-points + (w5 + f_5-points) ]])

    ; borda points for f_6: the canon of supply-and-demand
  (foreach t-supply-and-demand (range (count(players) ) 0 -1)
    [ [agent f_6-points] -> ask agent [set borda-points borda-points + (w6 + f_6-points) ]])

  report sort-on [(- borda-points)] players

end








to legitimate-claims-evolutionary

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Distributive Justice: legitimate claims;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of equality :

to-report equality-allocation  ;f_1a
   ;Rank the agents in increasing order of their average allocations
   ;Show [list who ( precision (total-allocation / ticks) 3)] of players
   report sort-on [precision (total-allocation / ticks) 3] players
end

to-report equality-satisfaction   ;f_1b
   ;show "-------equality-satisfaction------"
   ;Rank the agents in increasing order according to their trust-level
   ;show [list who satisfaction-level] of players
   ;show sort-on [satisfaction-level] players
   report sort-on [satisfaction-level] players
end


to-report equality-rounds   ;f_1c
   ;show "-------equality-satisfaction------"
   ;Rank the agents in increasing order according to their
   ;show [list who satisfaction-level] of players
   ;show sort-on [satisfaction-level] players
   report sort-on [total-rounds] players
end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of need : ;f_2

to-report need
   ;show "-------canon-need------"
   ;Rank the agents in increasing order of their average demands
   ;show [list who (precision (total-demand / ticks) 3 )] of players
   ;show sort-on [precision (total-demand / ticks) 3] players
   report sort-on [precision (total-demand / ticks) 3] players

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of productivity :

to-report productivity-avg-provision ;f_3a
   ;show "-------canon--productivity------"
   ;Rank the agents in decreasing order of their average provision
   ;show [list who precision (total-provision / ticks) 3] of players
   ;show sort-on [(- precision (total-provision / ticks) 3)] players
   report sort-on [(- precision (total-provision / ticks) 3)] players
end


to-report productivity-net-provision ;f_3b
   ;Rank the agents in decreasing order the difference between provision and allocation
   ;show [list who (precision ((total-provision - total-allocation) / ticks) 3 )] of players
   report sort-on [(- precision ((total-provision - total-allocation) / ticks) 3)] players
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of effort :

to-report effort ;f_4
   ;show "-------canon--effort------"
   ;Rank the agents in decreasing order according to the number of rounds they spent in the prosumer role
   ; The prosumer determines which agent is and is not a member of the institution
   ;show [list who total-rounds-as-prosumer] of players
   ;show sort-on [(- total-rounds-as-prosumer)] players
   report sort-on [(- total-rounds-as-prosumer)] players
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of social utility :

to-report social-utility ;f_5
    ;Rank the agents in decreasing order according to the number of rounds they spent in the head role
    ;The head is the role responsible for the resources allocation.
   ;show [list who total-rounds-as-head] of players
   report sort-on [(- total-rounds-as-head)] players
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The canon of supply and demand :
to-report supply-and-demand ;f_6
 ; Agents should only appropiate what is allocated r_i = a_i
 ;show [list who total-compliant-rounds] of players
 report sort-on [(- total-compliant-rounds)] players

end
@#$#@#$#@
GRAPHICS-WINDOW
321
50
625
371
-1
-1
16.444444444444443
1
12
1
1
1
0
1
1
1
0
17
-1
17
0
0
1
ticks
30.0

BUTTON
53
22
119
55
setup
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
133
22
199
55
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
45
72
208
105
players-number
players-number
0
100
40.0
1
1
NIL
HORIZONTAL

TEXTBOX
295
18
702
44
Prisoner's Dilemma  - Tragedy of the commons \n    
15
0.0
1

SLIDER
45
129
209
162
degeneration-rate
degeneration-rate
0
1
0.4
0.01
1
NIL
HORIZONTAL

PLOT
757
60
1192
292
Resources evolution
Rounds
proportion-resources
0.0
10.0
0.0
1.2
true
true
"" ""
PENS
"(Max )Resource-1" 1.0 0 -2139308 true "" "plot resource-2"
"Avg-resource-1" 1.0 0 -8732573 true "" "if (ticks > 0) [plot precision (total-resource-1 / ticks) 3]"

MONITOR
1090
10
1192
55
Avg-resource-1
precision (total-resource-1 / (ticks + 1)) 4
17
1
11

SLIDER
45
187
210
220
regeneration-rate
regeneration-rate
0
1
0.4
0.01
1
NIL
HORIZONTAL

PLOT
760
371
1193
602
w-evolution
NIL
NIL
0.0
10.0
0.0
0.5
true
true
"" ""
PENS
"w1" 1.0 0 -5298144 true "" "plot w1"
"w2" 1.0 0 -4079321 true "" "plot w2"
"w3" 1.0 0 -14439633 true "" "plot w3"
"w4" 1.0 0 -955883 true "" "plot w4"
"w5" 1.0 0 -13791810 true "" "plot w5"
"w6" 1.0 0 -1184463 true "" "plot w6"
"w1b" 1.0 0 -6917194 true "" ""

MONITOR
1065
312
1214
357
Total-Avg-satiscation
precision mean [satisfaction-level] of players 4
17
1
11

SLIDER
46
240
211
273
scarcity
scarcity
0.5
1.5
1.0
0.05
1
NIL
HORIZONTAL

CHOOSER
252
389
450
434
allocation-method
allocation-method
"legitimate-claims-S0" "legitimate-claims" "legitimate-claims-evolutionary" "legitimate-claims-Pitt" "ostrom-random-allocation" "greedy-random-allocation" "equal-allocation" "max-min-allocation" "legitimate-claims-Pitt" "legitimate-claims-fixed-pitt"
1

SLIDER
47
293
212
326
threshold
threshold
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
46
349
213
382
s-threshold
s-threshold
0
1
0.1
0.05
1
NIL
HORIZONTAL

MONITOR
932
10
1073
55
Fairness: Gini-index 
gini-index
4
1
11

SLIDER
46
408
214
441
beta-agent
beta-agent
0
0.5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
45
464
214
497
restore-equilibrium
restore-equilibrium
0
0.1
0.005
0.001
1
NIL
HORIZONTAL

SLIDER
44
517
215
550
no-compliant-agent
no-compliant-agent
0
15
3.0
1
1
NIL
HORIZONTAL

MONITOR
880
315
982
360
avg-utility-NC
mean( [utility ] of players with [cheat-on = true]) / ticks
4
1
11

MONITOR
760
315
861
360
avg-utility-C
mean( [utility ] of players with [cheat-on = false]) / ticks
4
1
11

CHOOSER
478
392
678
437
cheating-method
cheating-method
"demand" "appropriation"
1

PLOT
291
452
663
660
Players - Utility
NIL
NIL
0.0
1.5
0.0
100.0
true
true
"set-plot-x-range 0 1\nset-plot-y-range 0 count (players)\nset-histogram-num-bars 10\n" ""
PENS
"Utility" 1.0 1 -5825686 true "" ";if(ticks > 0) [histogram [total-appropriation] of players]\nif(ticks > 0) [histogram [utility / ticks] of players]\n"

MONITOR
638
51
733
96
S_c
mean( [satisfaction-level] of players with [cheat-on = false])
4
1
11

MONITOR
638
123
738
168
S_s
mean( [satisfaction-level] of players with [cheat-on = true])
4
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

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
12
Rectangle -5825686 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -5825686 true true 15 120 150 15 285 120
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

orbit 6
true
0
Circle -7500403 true true 116 11 67
Circle -7500403 true true 26 176 67
Circle -7500403 true true 206 176 67
Circle -7500403 false true 45 45 210
Circle -7500403 true true 26 58 67
Circle -7500403 true true 206 58 67
Circle -7500403 true true 116 221 67

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

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

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
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Scenario 2-PD-approriation" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;appropriation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 2-PD-demand" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 2-PD-demand (Voting)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 1 PD  (beta 0.4 - restore equ 0.020)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-demand - SO" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-approriation - SO" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;appropriation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  (beta 0.2)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  (beta 0.3)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  (beta 0.4)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  (beta 0.5)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  (beta 0.6)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4-PD-demand (Beta 0.2)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4-PD-demand (Beta 0.3)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.2)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.1)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.4)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.3)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.5)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD - SO (beta 0.6)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-S0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 4 PD  1000 (beta 0.6)" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (gini-index) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [utility / ticks] of players with [cheat-on = true])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = false])) 4</metric>
    <metric>precision (mean( [satisfaction-level] of players with [cheat-on = true])) 4</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-demand - fixed" repetitions="200" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-demand - pitt" repetitions="200" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-Pitt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-demand-pitt-fixed" repetitions="200" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;demand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-fixed-pitt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3-PD-appropiation-pitt-fixed" repetitions="200" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini-index</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = false])</metric>
    <metric>mean( [utility / ticks] of players with [cheat-on = true])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = false])</metric>
    <metric>mean( [satisfaction-level] of players with [cheat-on = true])</metric>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;appropiation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims-fixed-pitt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario 3 (changing number agents)-PD-demand - fixed" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision (total-resource-1 / (ticks + 1)) 4</metric>
    <enumeratedValueSet variable="regeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="s-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degeneration-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cheating-method">
      <value value="&quot;appropriation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="players-number">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-compliant-agent">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scarcity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-agent">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restore-equilibrium">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation-method">
      <value value="&quot;legitimate-claims&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="3"/>
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
