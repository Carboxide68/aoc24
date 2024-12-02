(require '[clojure.string])
(defn trace [v] (first [v (println v)]))

(defn transpose [ns]
  (apply map vector ns))


(defn prep_line [s]
    (map sort 
      (transpose 
        (map (fn [ss] 
          (filter some? 
            (map parse-long (clojure.string/split ss #"\s")))) 
        s))))

(defn collapse [ns]
  (loop [m {}, ss ns]
    (if (empty? ss)
    m,
    (let [caret (first ss), len (count (take-while #(= caret %) ss))]
        (recur (assoc m caret len) (drop len ss))))))

(defn task2 []
  (let [s (prep_line (clojure.string/split (slurp "in.txt") #"\n")), 
        m (collapse (second s))]
    (reduce #(+ (* (get m %2 0) %2) %1) 0 (first s))))
 

(defn task1 []
  (let [s (prep_line (clojure.string/split (slurp "in.txt") #"\n"))]
    (reduce 
       #(+ (abs (- (first %2) (second %2))) %1)  
     0 (transpose s))))


(println (task1))
(println (task2))
