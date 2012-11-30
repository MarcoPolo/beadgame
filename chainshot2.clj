(ns beadgame.chainshot2)



(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))




(defn readFile 
  "Read the file starting board file"
  [filename]
  (let [filecontents (slurp filename)]
    (println "Reading file" filename)
    (def file filecontents)))




(comment

  The nodes wil work something like this


  {
   :right other-node
   :up nil}


  (def root-node
    { :right other-node
      :up nil})

  (def other-node2
    { :right nil
      :up nil})

  (def other-node
    { :right nil
      :up other-node2})

  (readFile "startBoard")
  (def file (reverse (clojure.string/split file #"\n")))

  (def file (map #(clojure.string/split % #" ") file))
   
  (def node-file (map (fn [row] (map #(assoc {:up nil :right nil} :val %) row)) file))

  ((first file) 0)
  (first node-file)

  (if (seq '(1))
    (println "truthy")
    (println "falsy"))


  (let [root-row (first node-file)]
    (loop [first-element (first root-row)
           rest-elements (rest root-row)]
      (if (seq  rest-elements)
        (recur 
          (conj-graph :right first-element (first rest-elements))
          (rest rest-elements))
        first-element)))


  (conj-array :right (first (first node-file)) (rest (first node-file)))




  )


(conj-graph :right root-node other-node2)

conj-graph

(def a [:up :right])

(defn conj-array 
  "conj-graphs a root node to an array of items in that order"
  [direction root-node rest-elements]
  (loop [first-element root-node
         rest-elements rest-elements]
    (if (seq  rest-elements)
      (recur 
        (conj-graph direction first-element (first rest-elements))
        (rest rest-elements))
      first-element)))

(defn conj-graph
  "Connects two nodes, the parents to the child.
  If there is alread a node on the parents right it will be recursively placed
  Specify the direction as in :up"

  [direction parent child]
  (if (nil? (parent direction))
    (assoc parent direction child)
    (assoc parent direction (conj-graph direction (parent direction) child))))


))))))))))
