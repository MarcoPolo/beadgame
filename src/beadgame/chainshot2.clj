(ns beadgame.chainshot2
  (:gen-class))




(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))




(defn read-file 
  "Read the file starting board file"
  [filename]
  (let [filecontents (slurp filename)]
    (println "Reading file" filename)
    (map 
      (fn [row] (clojure.string/split row #" "))
      (->
        filecontents
        (clojure.string/split #"\n")
        (reverse)))))




(declare zip-graph)
(declare conj-array)
(declare read-graph)
(declare conj-graph)
(declare goto-neighbor)
(declare print-board-from-root)

(defn create-graph
  [parsed-file]
  (atom (map (fn [row] (map #(atom {:val %}) row)) parsed-file)))

(defn connect-rows! [graph]

  (doseq [row @graph
        :let [rev-row (reverse row)]]
    (conj-array row :right)
    (conj-array rev-row :left)))

(defn connect-cols! [graph]
  (for [x (range (dec (dbg (count @graph))))
        :let [ bottom-row (dbg (nth @graph x))
               top-row (nth @graph (inc x)) ]]
    (zip-graph bottom-row top-row)))

(defn find-cluster
  "Finds clusters starting from a specific node, clusters are connected nodes with the same value."
  ([graph starting-node]
   (find-cluster graph starting-node (@starting-node :val) ))
  ([graph starting-node value ]
    (let [ 
          right-node (goto-neighbor starting-node :right)]
      (println (@starting-node :val))
      (if (and 
            (= (@starting-node :val) value)
            (not (nil? starting-node)))
        (->> [starting-node]
          (apply conj (dbg (find-cluster graph right-node value))))
        []))))

(defn get-node-path
  "goes through the nodes using direction until it hits an end, returns it as a map
  Specify the starting position and the update function to update the position"

  ([root-node direction position position-update]
    (get-node-path root-node direction position position-update (fn [a] a) {}))

  ([root-node direction position position-update board-map]
     (loop [position position 
            current-node root-node
            board-map board-map]
       (if (nil? current-node)
         board-map
         (recur
           (position-update position)
           (goto-neighbor current-node direction)
           (assoc board-map position current-node ))))))

(goto-neighbor (get-root-node graph) :down)
           
(defn move-node-down!
  "Moves a node down a slot, by splicing the bottom nodes left and right neighbors to the top node's" 
  [node-to-move]
  ;; Check to see if there is a node above this one to move
  (if-not (nil? (goto-neighbor node-to-move :up))
    (move-node-down! (goto-neighbor node-to-move :up)))

  (let [right-node (-> node-to-move 
                     (goto-neighbor :down)
                     (goto-neighbor :right))
        left-node (-> node-to-move 
                     (goto-neighbor :down)
                     (goto-neighbor :left))]
    ;;atach the right node to the current node and vice versa
    (conj-graph :right node-to-move right-node)
    (conj-graph :left  right-node node-to-move)

    (conj-graph :left  node-to-move left-node)
    (conj-graph :right left-node node-to-move)))



(defn remove-node!
  "Function to remove a node and handle the change in other nodes
   It will first move the top node down, then splice the top nodes and the removee node's bottom together
   If it is removed and there is no top or bottom node then we splice the adjacent columns together"
  [removee-node]
  ;; There is something above this node so we need to bring it down
  (move-node-down! (goto-neighbor removee-node :up))

  (if (and (nil? (goto-neighbor removee-node :up)) (nil? (goto-neighbor removee-node :down)))
    ;;No bottom or top nodes, we need to splice the adjacent columns together
    nil
    ;; Else we need to splice the top and bottom together
    (let [ top-node (goto-neighbor removee-node :up)
           bottom-node (goto-neighbor removee-node :down)]
      (conj-graph :up   bottom-node top-node)
      (conj-graph :down top-node    bottom-node))))



(defn print-board-from-root
  [root-node]

  (let [root-row (get-node-path
                   root-node
                   :right 
                   [0 0]
                   #(update-in % [0] inc))
        full-board-map ( map (fn [[pos atm]]
                               (get-node-path
                                 atm
                                 :up
                                 pos
                                 #(update-in % [1] inc)))
                             root-row)
        asdf (def temp-col2 full-board-map)
        formatted-rows (for [y (reverse (range (count @graph)))]
                         (map 
                           (fn [a] (if (nil? a) " " (@a :val))) 
                           ( map (apply merge full-board-map) (for [x (range (count @graph))] [x y]))))]
    (doseq [row formatted-rows] (println row))))

(( get-node-path
   ((get-node-path
     root
     :right 
     [0 0]
     #(update-in % [0] inc)) [2 0])
   :up
   [2 0]
   #(update-in % [1] inc)) [2 1])

(def root (get-root-node graph))
(nth asdf 2)

(doseq [cols temp-col2]
  (doseq [[pos item] cols]
    (print " " pos " "))
  (println))

(second (first (first temp-col2)))

(def temp-col (get-node-path
    (traverse-graph root :x 3 :y 1)
     :up
     [ 0 0 ]
     #(update-in % [0] inc)))

(swap! ((nth temp-col2 2) [2 1]) #(assoc % :val "x"))
(map (fn [[pos a]] (@a :val)) temp-col)

(remove-node! (traverse-graph root :x 3 :y 2))
(-> 
   (traverse-graph root :x 3 :y 1)
   (goto-neighbor :left)
   (goto-neighbor :up)
   (goto-neighbor :right))
(print-board-from-root root)

 ( goto-neighbor (traverse-graph root :x 2 :y 5) :right)

(comment 
  (require 'beadgame.chainshot2)
  (ns beadgame.chainshot2)
(print-graph graph)
(def cls (find-cluster graph (get-root-node graph)))
  (map (fn [a] (@a :val)) cls)

(conj-graph :right root-node other-node2)

conj-graph

(def graph (create-graph (read-file "startBoard")))
  graph
(connect-cols! graph)
(connect-rows! graph)

  (for [x (range (dec (dbg (count @graph))))
        :let [ bottom-row (nth @graph x)
               top-row (nth @graph (inc x)) ]]
    (zip-graph bottom-row top-row))

  (print-graph graph)

(def rnode (goto-neighbor (get-root-node graph) :right))
  (@((@(get-root-node graph) :up)) :val)
  (@(trampoline @(get-root-node graph) :up) :val)
  (@(goto-neighbor (get-root-node graph) :up) :up)
  
  (@(-> root
    (goto-neighbor :right)
    (goto-neighbor :right)
    (goto-neighbor :up))
      :val)

  (@(-> (get-root-node graph)
    (goto-neighbor :right)
    (goto-neighbor :right)
    (goto-neighbor :up)
    (goto-neighbor :up)
    (goto-neighbor :right))
      :val)
  
  (@(get-root-node graph) :val)

(@(get-root-node graph) :up)

    (let [value "g"
          starting-node (goto-neighbor (get-root-node graph) :right 3)
          right-node (goto-neighbor starting-node :right)]
      (if (and 
            (= (dbg (@starting-node :val)) value)
            (not (nil? starting-node)))
        (->> [starting-node]
          (apply conj (find-cluster graph right-node value)))
        []))

  (zip-graph
    (second @graph)
    (nth @graph 3))

(@(traverse-graph graph :x 2 :y 3) :val)

(goto-neighbor (get-root-node graph) :down)
(find-cluster nil \g)

(@(goto-neighbor (first (first @graph)) :right 1) :val)

(@(@(@(first (first @graph)) :up) :down) :val)

(@(traverse-graph graph :x 2 :y 1) :val)

(traverse-graph graph :x 1 :y 3)
  )



(defn get-root-node [graph] 
  (first (first @graph)))


(defn goto-neighbor 
  ([parent-node direction] 
   (trampoline @parent-node direction))
  ([parent-node direction times]
   (loop [parent-node parent-node times times]
     (if (pos? times)
       (recur (trampoline @parent-node direction) (dec times))
       parent-node))))


(defn traverse-graph
  [root-node & position]
  (let [{:keys [x y]} position]
    (-> 
      root-node
      (goto-neighbor :right (dec x))
      (goto-neighbor :up (dec y)))))
    






(comment
  )

(defn print-graph
  ([graph]
  (let [ graph (read-graph graph)
        [first-row & rest-rows] graph]
    (print-graph first-row rest-rows)))
  ([first-row rest-rows]
    (if (seq rest-rows)
      (print-graph (first rest-rows) (rest rest-rows)))
    (println first-row)))

(nil? (get-root-node graph))

(defn read-graph 
  "Prints the cyclic graph"
  [graph]
  (map 
    (fn [row] (map 
                (fn [node] (:val @node)) 
                row))
    @graph))



(defn zip-graph
  [root-row upper-row]
  "Joins two rows by top and bottom"
  (println "zipping up!")
  (for [x (range (count root-row))
        :let [bottom-node (nth root-row x)
              top-node (nth upper-row x)]]
    (do 
      (conj-graph :up bottom-node top-node)
      (conj-graph :down top-node bottom-node)
      nil)))




(defn conj-array 
  "Connects an array of nodes together through left and right"
  [elements direction]
  (loop [elements elements]
    (if (seq (rest elements))
      (let [first-element (first elements)
             second-element (second elements)
             rest-elements (rest elements)]
        (conj-graph direction first-element second-element)
        (recur 
          rest-elements ))))
  nil)

(defn conj-graph
  "Connects two nodes, the parents to the child.
  If there is alread a node on the parents right it will be recursively placed
  Specify the direction as in :up"

  [direction parent child]
  (swap! parent #(assoc % direction (fn[] child)))
  parent)


