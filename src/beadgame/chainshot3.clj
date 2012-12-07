;; So here is the third iteration that involves no state to operate
;; Although we are going to have a single unit of state to make the game playaple it isn't required
;; and in fact not used for the AI algorithms

(ns beadgame.chainshot3
  (:use [criterium.core]
        [quil.core])
  (:gen-class))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


;; Simple atom to keep state.
(def graph-atom (atom nil))

(comment 
  (def sample-graph ^{:bound-limit 5} { 
                     4 { 0 "g" 1 "g" 2 "r" 3 "r" 4 "g" }
                     3 { 0 "g" 1 "g" 2 "r" 3 "r" 4 "g" }
                     2 { 0 "g" 1 "g" 2 "b" 3 "r" 4 "g" }
                     1 { 0 "r" 1 "g" 2 "g" 3 "r" 4 "g" }
                     0 { 0 "r" 1 "g" 2 "g" 3 "r" 4 "r" }})

  (def sample-pos ^"Position is [x y]" [0 4])
  (comment "First part pertains to the col no* and the second part pertains to row number")
  (= (get-in sample-graph sample-pos) "r")
  (print-graph sample-graph)

  (def bound-limit (:bound-limit (meta sample-graph) ))

  (reduce #(and %1 %2) (map pos? [0 1]))
  (ns beadgame.chainshot3)

  )

(comment 
  (def test8 (print-graph (read-file "testBoard8")))
  (def test5 (print-graph (read-file "test5")))
  bound-limit
  )

(defn read-file 
  "Reads a file and outputs a graph, also sets the global bound limit"
  [filename]
  (->
    (slurp filename)
    (clojure.string/split #"\n")
    (count)
    (#(def bound-limit %)))
    
  ( loop [ characters (->
                        (slurp filename)
                        (clojure.string/split #"\s"))
          graph (apply merge {} (for [x (range bound-limit)] [x {}]))
          positions (for [y (reverse (range bound-limit)) x (range bound-limit) ] [x y])]
    (if (seq characters)
      (recur 
        (rest characters)
        (assoc-in graph (first positions) (first characters))
        (rest positions))
      graph)))


(defn bounded? [bound-limit position] 
  "Returns the position if it is within the 0 < position < bound-limit
   Else returns false"
  (if (->>
        position
        (map #(and (<= 0%) (< % bound-limit)) )
        (reduce #(and %1 %2)))
    position
    false))

(defn goto-neighbor-with-limit
  [bound-limit position direction]
  (if-let[ new-position (bounded? 
                          bound-limit 
                          (condp = direction
                            :up (update-in position [0] inc)
                            :down (update-in position [0] dec)
                            :left (update-in position [1] dec)
                            :right (update-in position [1] inc)))]
    new-position
    nil))


(defn goto-neighbor 
  [graph position direction] 
  (goto-neighbor-with-limit bound-limit position direction))

(defn read-neighbor 
  [graph position direction]
  (get-in graph (goto-neighbor graph position direction)))

(defn pieces-left [graph]
  (->> 
    (vals graph)
    (map vals)
    (map count)
    (reduce +)))

(defn find-cluster 
  ([graph position]
    (let [value (get-in graph position)
          cluster (first (find-cluster graph position value [] []))]
      (if (< 2 (count cluster))
        cluster
        [])))

  ([graph position value cluster visited]
    (let [ directions [:up :down :left :right]
           neighbors (map #(goto-neighbor graph position % ) directions)]
      (if (and 
            (not (nil? position)) 
            (not (nil? value)) 
            (= value (get-in graph position))
            (not (some #{position} visited)))
        ;(conj (map #(find-cluster graph % value (conj visited position)) neighbors) position)
        (->> 
          [(conj cluster position) (conj visited position) ]
          (apply find-cluster graph (nth neighbors 0) value)
          (apply find-cluster graph (nth neighbors 1) value)
          (apply find-cluster graph (nth neighbors 2) value)
          (apply find-cluster graph (nth neighbors 3) value))


        [cluster (conj visited position)]))))


(defn shift-graph-changes 
  [graph nodes]
  ; Get the cols that have changed
  (let [changed-cols (map first nodes)]
    [(loop [graph graph changed-cols changed-cols]
      (if (seq changed-cols)
        (recur
          (->> 
            (get graph (first changed-cols))
            (#(remove nil? (map % (range bound-limit))))
            (zipmap (range bound-limit))
            (assoc graph (first changed-cols))) 
          (rest changed-cols))
        graph))
     nodes]))

(defn shift-graph-cols
  [graph nodes]
  [(->>
    (for [row (range bound-limit)] (graph row))
    (remove empty?)
    (zipmap (range bound-limit)))
   nodes])



(defn remove-nodes 
  ([graph nodes]
    [(loop [graph graph nodes nodes]
      (if (seq nodes)
        (recur
          (assoc-in graph (first nodes) nil)
          (rest nodes))
        graph))
     nodes]))

(map #(print " " %) (range bound-limit))

(defn print-graph
  ([graph]
  ;;print the top row first and work your way down
    (doseq [y (reverse (range bound-limit)) x (range bound-limit) :let [node (get-in graph [x y])] ]
      (if (zero? x) (do (println) (print (inc y))))
      (print " " (if (nil? node) " " node)))
    (println)
    (print " ")
    (doseq [ x (range bound-limit) ] (print " " (inc x)))
    (println)
    graph)
  ([graph & more] 
    (print-graph graph)
    ;; Echo back original args for chaining purposes
    (conj more graph)))
   
(defn calculate-points 
  [cluster]
  (if (> (count cluster) 2)
    (Math/pow (- (count cluster) 2) 2)
    0.0))

(defn choose-move
  [graph cluster]
  [ (->>
      [graph cluster]
      (apply remove-nodes)
      (apply shift-graph-changes)
      (apply shift-graph-cols)
      ((fn [[graph nodes]] graph)))
    (calculate-points cluster)])


    
(defn find-all-clusters
  [graph]
  (loop [ possible-moves (for [x (range bound-limit) y (range bound-limit)] [x y])
          cluster-positions #{}
          clusters []
          graphs   []
          points   [] ]
    (cond
      (nil? (seq possible-moves)) (map vector graphs clusters points)
      (nil? (get-in graph (first possible-moves)))
      (recur 
        (rest possible-moves)
        (conj cluster-positions (first possible-moves))
        clusters
        graphs
        points)
      (nil? (cluster-positions (first possible-moves)))
      ;; We haven't added this position as part of a previous cluster
      (let [new-cluster (find-cluster graph (first possible-moves))
            move (choose-move graph new-cluster)
            new-graph (first move)
            new-points (second move)]
        (def c new-cluster)
        ;;check to see if the new-cluster is invalid
        (if (nil? (seq new-cluster))
          (recur
            (rest possible-moves)
            cluster-positions
            clusters
            graphs
            points)
          (recur
            (rest possible-moves)
            (apply conj cluster-positions new-cluster) ;; Add the cluster to our set of visited nodes
            (conj clusters new-cluster)
            (conj graphs new-graph)
            (conj points new-points))))
      :else (recur 
              (rest possible-moves)
              cluster-positions
              clusters
              graphs
              points))))

(defn execute-moves
  "Simple way to view moves being executed"
  [graph moves graph-atom & {:keys [time-delay] :or {time-delay 1000}}]
  (loop [moves moves graph graph points 0]
    (Thread/sleep time-delay)
    (if (seq moves)
      (let [new-cluster (find-cluster graph (first moves))
            move (choose-move graph new-cluster)
            new-graph (first move)
            new-points (second move)]
        (print-graph new-graph)
        (reset! graph-atom new-graph)
        (println "Points: " (+ new-points points))
        (recur 
          (rest moves) 
          new-graph
          (+ points new-points)))
      nil)))

(defn filter-moves
  "If a move doesn't do anything we can filter it out
   Useful for outputting the final set of moves"
  [graph moves]
  (loop [graph graph moves moves filtered-moves []]
    (if (seq moves)
      (let [new-cluster (find-cluster graph (first moves))
            move (choose-move graph new-cluster)
            new-graph (first move)]
        (if (seq new-cluster)
          ;; We have a valid move
          (recur new-graph (rest moves) (conj filtered-moves (first new-cluster)))
          ;; We don't have a valid move
          (recur graph (rest moves) filtered-moves)))
      filtered-moves)))

(defn input-position-bounded? [[x y]]
  (if (bounded? bound-limit [x y])
    [x y]
    (do (println "Sorry the position is out of the range")   nil)))

(defn input-valid? [graph [x y]]
  (if (get-in graph [x y])
    [x y]
    (do (println "sorry there is nothing there" nil))))

(defn cluster-valid? [graph [x y]]
  (if (seq (find-cluster graph [x y]))
    [x y] 
    (do (println "The cluster is too small") nil)))

(defn valid-move? [graph position]
  (and (input-position-bounded? position)
       (input-valid? graph position)
       (cluster-valid? graph position)))

(defn prompt-move [graph]
  (println "Pick your move x y")
  (let [position (map dec (map read-string (clojure.string/split (read-line) #" ")))]
    (println position)
    (if (valid-move? graph position)
      (vec position)
      (prompt-move graph))))

(defn prompt-graph []
  (println "Type in the board-file")
  (read-file (read-line)))

(defn finished-game [points]
  (println "Congrats you finished the game with: " points "points!"))

(declare prompt-play-again)

(defn human-playing []
  (loop [graph (prompt-graph) points 0]
    (print-graph graph)
    (println "Points: " points)
    (reset! graph-atom graph)
    (let [move-position (prompt-move graph)
          cluster (find-cluster graph move-position)
          [new-graph new-points] (choose-move graph cluster)]
      (if (seq (find-all-clusters new-graph))
        (recur new-graph (+ points new-points))
        (do (finished-game (+ points new-points)) (prompt-play-again))))))

(defn prompt-play-again []
  (println "Play Again (y/n)")
  (condp = (read-line)
    "y" (do (println "playing again!") (human-playing))
    "n" (println "not playing again!")
    (do (println "Please choose either y or n") (prompt-play-again))))
    
        
(comment 
  test5
  (reset! graph-atom test5)
  (build-visual-aid graph-atom)
  (prompt-graph)
  (human-playing)
  (breadth-search [sample-graph [] 1])

  )



;; Genetic algorithms

(defn initialize
  "Returns randomly generated points that will be used to pick clusters
  Lets call it a strand"
  [length]
  (repeatedly length (fn [] [(rand-int bound-limit) (rand-int bound-limit)])))

(defn generate-strands
  [strand-count strand-length]
  (take strand-count ( repeatedly #( initialize strand-length ))))

(defn fitness
  "calculates the fitness of strand"
  [graph strand]
  (loop [strand strand graph graph points 0]
    (if (seq strand)
      (let [move (->>
                   (find-cluster graph (first strand))
                   (choose-move graph))
            new-graph (first move)
            points (+ points (second move))]
      (recur
        (rest strand)
        new-graph
        points))
      points)))


(defn crossover
  [strand1 strand2]
  (apply conj 
         (vec (take (rand-int (count strand1)) strand1))
         (take (inc (rand-int (dec (count strand2)))) strand2)))

(defn simple-mutate
  [strand]
  (if (< 0.70 (rand))
    (initialize 1)
    strand)) 

(defn roulette-wheel-selection
  [strands fitness-values times] 
  (let [strands (map vector strands fitness-values)
        sorted-strands (sort-by #(* -1 (second %)) strands) 
        sum (reduce + fitness-values)
        normalized-strands (map #(vector (first %) (/ (second %) sum)) sorted-strands)
        accumulated-strands (loop [normalized-strands normalized-strands accumulated-strands [] running-sum 0]
                              (if (seq normalized-strands) 
                                (recur 
                                  (rest normalized-strands)
                                  (conj accumulated-strands (update-in (first normalized-strands) [1] #(+ running-sum %)))
                                  (+ running-sum (second (first normalized-strands)) ))
                                accumulated-strands))]
    (take times (repeatedly #(first (filter (fn [strand] (let [r (rand)] (> (second strand) r))) accumulated-strands))))))
    
(defn breed-strands
  [strands children-count]
  (take children-count (repeatedly #(-> (crossover (rand-nth strands) (rand-nth strands))
                                        (simple-mutate)))))


(defn circle-of-life
  "Function that composes all the parts of the genetic algorithm
   
   population-count - How many different strands should exist
   strand-length - How long each strand should be ( a strand is a set of moves
   iteration-count - How many iterations to go through
   selection-ratio - Select what percentage of the initial population for future use

  "

  [graph & {:keys [population-count strand-length iteration-count selection-ratio] 
            :or {population-count 100 strand-length 20 iteration-count 20 selection-ratio 0.1 }}]
  (let [initial-population ( generate-strands population-count strand-length)]
    (loop [population initial-population iteration iteration-count max-strand [[] 0]]
      (println "iteration" iteration)
      (if (pos? iteration)
        (let [fitness-values (pmap (partial fitness graph) population)
              new-population (-> (map first (roulette-wheel-selection population fitness-values (int (* population-count selection-ratio))))
                                 (breed-strands population-count))  
              new-max-strand (apply max-key second (map vector population fitness-values))]
          (println "Fitness so far:" (take 5 fitness-values))
          (recur new-population (dec iteration) (max-key second new-max-strand max-strand)))
        max-strand))))



(defn build-visual-aid [graph-atom]

  (defn setup []
  (smooth)                      
  (frame-rate 10)                 
  (background 200))               
                                   
  (def color-map (zipmap (map str (map char (range 65 91) )) 
          (repeatedly #(map (fn [a] (rand-int 256)) (range 3)))))
  (def color-map (assoc color-map nil '(200)))

  (defn draw []
    (stroke 200)         
    (stroke-weight 10)  
    (fill (random 255))         
    (if (nil? @graph-atom)
      (background 200)

      (let [graph @graph-atom diam (/ 800 bound-limit)]
           (doseq [y (reverse (range bound-limit)) x (range bound-limit) :let [ [plot-x plot-y] (map #(+ (/ 800 bound-limit 2) (* (/ 800 bound-limit) %)) [x y])
                                                                                 node (get-in graph [x y])] ] 
             (apply fill (color-map node))
             (ellipse plot-x (- 800 plot-y) diam diam) ))))


  (defsketch example                  ;;Define a new sketch named example
    :title "Beadgame!"  ;;Set the title of the sketch
    :setup setup                      ;;Specify the setup fn
    :draw draw                        ;;Specify the draw fn
    :size [800 800]))



(defn demo-genetic-algorithm []

  (doseq [graph-name [
                      "test-boards/test1"
                      "test-boards/test2"
                      "test-boards/test3"
                      "test-boards/test4"
                      "test-boards/test5"
                      "test-boards/test6"
                      "test-boards/test7"
                      "test-boards/test8"
                      "test-boards/test9"
                      "test-boards/test10"
                      "test-boards/test11"]
          :let [graph (read-file graph-name)]]
    (println "Testing" graph-name)
    (let [[time-dur [moves points]] 
          (time-body
            (circle-of-life 
              graph 
              :population-count 50 
              :strand-length 150 
              :iteration-count 20 
              :selection-ratio 0.3))]


      (reset! graph-atom nil)
      (reset! graph-atom graph)

      (execute-moves graph (filter-moves graph moves) graph-atom :time-delay 10)
      (println "Points: " points)
      (println "Time: " time-dur)
      (println "Pieces left: " (pieces-left @graph-atom)))))




  

(comment 

  ;;clear the board if we are showing it
  (reset! graph-atom nil)

  (println "Testing test 11")
  (def test11 (print-graph (read-file "test11")))
  (def moves (circle-of-life test11 :population-count 50 :strand-length 50 :iteration-count 20 :selection-ratio 0.8))
  (println "Points for test 11" (second moves))
  (reset! graph-atom test11)
  (execute-moves test11 (filter-moves test11 (first moves))  graph-atom :time-delay 300)

  )


(defn benchmark-genetic-algorithm []
  (println "Benchmarking test 11")
  (def test5 (print-graph (read-file "test5")))
  (with-progress-reporting (bench 
                             (circle-of-life test5 100 40 20 0.3)))

  (println "Benchmarking test 5")
  (def test11 (print-graph (read-file "test11")))
  (with-progress-reporting (bench 
                             (circle-of-life test11 100 40 20 0.3))))

(comment
  
  ;; for use in repl

  (ns beadgame.chainshot3)
  (require 'beadgame.chainshot3)
  (use 'criterium.core)
  (use 'quil.core)


  (build-visual-aid graph-atom)
  (demo-genetic-algorithm)
  (reset! graph-atom nil)

  graph-atom
  (reset! graph-atom test5)

  )
;; Greedy depth first search


(defn greedy-depth-search
  ([[graph cluster score]]
   (loop [graph graph score score path []]
     (let [children (remove #(zero? (% 2)) (find-all-clusters graph))]
       (if (seq children)
         (let [biggest-child (apply max-key #(% 2) children)]
           (def b [children])
           (recur (first biggest-child) 
                  (+ score (biggest-child 2))
                  (conj path (first (second biggest-child)))))
         [path score])))))



(defn demonstrate-greedy-algorithm []
  (doseq [graph-name ["test5" "test11"] :let [graph (read-file graph-name)]]
    (let [[moves points] (greedy-depth-search [graph [] 0.0])]
      (reset! graph-atom nil)
      (println "Points: " points)
      (reset! graph-atom graph)
      (execute-moves graph moves graph-atom :time-delay 300))))

(comment 

  (demonstrate-greedy-algorithm)

  (use 'clojure.stacktrace)
  (print-stack-trace *e 50)
  (print-cause-trace *e 3)

  )


;(ns beadgame.breadth-search)

;; Naive breadth-search algorithm, reallly long time
(defn breadth-search 
  ([[graph cluster score]]
    (if (> 0 score)
      [0 '()]
      (let [children (map breadth-search (find-all-clusters graph))]
        (if (seq children)
          (->
            ;; Get the biggest value child
            (apply max-key first children)
            ((fn [[child-score path]] [ (+ child-score score) (conj path (first cluster) )])))
          [0 '()])))))



(defn guided-genetic   []
  (let [graph (prompt-graph)
        [moves points] 
        (circle-of-life 
          graph 
          :population-count 50 
          :strand-length 150 
          :iteration-count 20 
          :selection-ratio 0.3)]

    (println "Points: " points)
    (reset! graph-atom nil)
    (reset! graph-atom graph)
    (execute-moves graph (filter-moves graph moves) graph-atom)))

(defn guided-greedy    []
  (let [graph (prompt-graph)
        [moves points] (greedy-depth-search [graph [] 0.0])]
    (reset! graph-atom nil)
    (println "Points: " points)
    (reset! graph-atom graph)
    (execute-moves graph moves graph-atom :time-delay 300)))

(defn guided-breadth   []
  (let [graph (prompt-graph)
        [points moves] (breadth-search [graph [] 1])]
    (reset! graph-atom nil)
    (println "Points: " points)
    (reset! graph-atom graph)
    (execute-moves graph moves graph-atom :time-delay 300)))

(defn run-test-cases   []
  (println "This will run through all of the test cases provided" )
  (println "First it will calculate the necessary moves")
  (println "Then show them back to you at 1 move/s")
  (println "It's recommeneded to view it on the opened beadgame window, prettier that way" )
  (demo-genetic-algorithm))

(defn guided-benchmark []
  (let [graph (prompt-graph)]
    (with-progress-reporting (bench (circle-of-life 
                                      graph 
                                      :population-count 50 
                                      :strand-length 150 
                                      :iteration-count 20 
                                      :selection-ratio 0.3)))))

(time-body (Thread/sleep 1000))

(defn main-menu []
  (build-visual-aid graph-atom)
  (println "A pretty format of the graph is being drawn as well as the console version")
  (def stay-in (atom true))
  (while @stay-in
    (println "Choose an option:")
    (println "\t (1) Play game normally")
    (println "\t (2) Run genetic algorithms on board")
    (println "\t (3) Run greedy-depth search on board")
    (println "\t (4) Run breadth search on board (DANGER)")
    (println "\t (5) Run test cases")
    (println "\t (6) Benchmark the genetic-algorithm on a board")
    (println "\t (7) Exit")
    (condp = (read-line)
      "1" (human-playing)
      "2" (guided-genetic)
      "3" (guided-greedy)
      "4" (guided-breadth)
      "5" (run-test-cases)
      "6" (guided-benchmark)
      "7" (reset! stay-in false)
      (println "That's not an option"))))

(defn -main [] (main-menu))
