(ns beadgame.helloworld
  (:gen-class))
(defn -main 
  []
  (println "starting!!")
  (while true
      (let [ x (read-line)]
          (println " hello World! " x))))


