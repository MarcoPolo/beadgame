(ns beadgame.chainshot
  (:gen-class))

(def rowLength 5)
(def colLength 5)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(defn getRow [boardState index]
  (vec (map #(try (%1 index) (catch Exception e " ")) boardState)))

(defn getCol [boardState index]
  (boardState index))

(defn printBoard [boardState]
  (doseq [ y (reverse (range rowLength)) ]
    (println (getRow boardState y))))

(defn getColor [boardState x y]
  ((getCol boardState y) x))

(defn removeRange [colorSeries start end]
  
  (reduce conj (subvec colorSeries 0 start) (subvec colorSeries (inc end))))

    
(defn forwardWalk [colorSeries startPosition color]
  (loop [position startPosition] 
    (if (not= (colorSeries position) color)
      (dec position)
      (if (< (inc position) (count colorSeries))
        (recur (inc position))
        position))))

(defn reverseWalk [colorSeries startPosition color]
  (- (count colorSeries) (inc (forwardWalk (vec (reverse colorSeries)) (- (count colorSeries) (inc startPosition)) color ))))

(defn findRange [colorSeries startPosition color]
  [(reverseWalk colorSeries startPosition color) (forwardWalk colorSeries startPosition color)])


(defn filterColumns [boardState startCol endCol x y color]
  (loop [currentCol startCol newCols (subvec boardState 0 startCol) ] 
    ( if (<= currentCol endCol)
      (recur 
        (inc currentCol)

        (let [colorSeries (getCol boardState currentCol) [removeStart removeEnd] (findRange colorSeries x color)]
          (conj newCols (removeRange colorSeries removeStart removeEnd ))))
      ( filter #(seq %)  
        (reduce conj newCols (subvec boardState (inc endCol)))))))

(defn chooseMove [boardState x y]
  "User's action on clicking the spot in the board"
  ; First we need to calculate which columns are affected
  ; then we will calculate which part of the columns are affected and modify the vector accordingly. this will modify each part of the row
  ; When we find an empty vector we get rid of it
  
  (try 
    (let [ color (getColor boardState x y) [startCol endCol] (findRange (getRow boardState x) y color) ] 
      (vec (filterColumns boardState startCol endCol x y color)))
    (catch Exception e boardState)))


(defn unskewInput [skewedBoard]
  (loop [board [] skewedBoard skewedBoard colNo 0]
    (if (>= colNo (count skewedBoard))
      board
      (recur (conj board (vec (reverse (map #(% colNo) skewedBoard)))) skewedBoard (inc colNo)))))

(defn readFile 
  "Read the file starting board file"
  [filename]
  (let [filecontents (slurp filename)]
    (println "Reading file" filename)
    (unskewInput 
      (vec 
        (map #(clojure.string/split % #"\s")
          (clojure.string/split-lines filecontents))))))


(defn piecesLeft [boardState] 
  ; count the pieces left
  (reduce #(+ %1 (count %2)) 0 boardState))


(defn calculatePoints [currentState nextState]
  (let [delta (- (piecesLeft currentState) (piecesLeft nextState))]
  (if (< 1 delta)
    (Math/pow (- delta 2) 2)
    0 )))

(defn keepPlaying [currentState]
  (let [startPieceCount (piecesLeft currentState)]
    (reduce #(or %1 %2) 
      (map #(< 1 %) 
        (try
          (map #(- startPieceCount (piecesLeft (apply chooseMove %)))
            (for [x (range colLength) y (range rowLength) ] [currentState x y]))
          (catch Exception e 0))))))
          
(defn printRules []
  (println "The rules are really difficult so you might want to take notes")
  (println "Specify your move in the format of: X Y")
  (println "Where X is the row counting from the bottom")
  (println "and Y is the column counting from the left")
  (println "You want to make a move somewhere with a group of colours, given that group is > 1 ")
  (println "After you pick a move I will calculate how many points you \"deserve\"")
  (println "After you have exhausted all the moves, the game is over and at least one of us will be dissapointed"))

(defn playGame []
  "Would you like to play a game?"
  (println "Would you like to play a game?")
  (println "Specify the initial board file")
  (def currentState (readFile (read-line)))
  (def rowLength (count (currentState 0)))
  (def colLength (count currentState ))
  (def currentPoints 0)
  (def moveHistory [])
  (printRules)
  (while (keepPlaying currentState)
    (printBoard currentState)
    (println "Current Points" currentPoints)
    (try 
      (let [ [x y] (map dec (read-string (str "[" (read-line) "]")) )
            nextState (chooseMove currentState x y) ]
        (println "The move was" (map inc [x y]))
        (def moveHistory (conj moveHistory (map inc [x y])))
        (def currentPoints (+ currentPoints (calculatePoints currentState nextState)))
        (if (< (piecesLeft nextState) (dec (piecesLeft currentState)))
          (def currentState nextState)))
      (catch Exception e (println "I'm sorry Dave, I'm afraid I can't do that"))))

  (printBoard currentState)
  (println "Your moves where" moveHistory)
  (cond
    (< 9000 currentPoints) (println "It's OVER 9000!!!!!!" currentPoints)
     :else (println "Game over! You failed with only" currentPoints "Points! Do you think your mom would be proud of that?"))
  (println "Play Again? (y/n)")
  (if (= "y" (read-line))
    (playGame)
    (println "Okay, I guess you weren't as cool as I thought you were...")))

(defn -main [] (playGame))
