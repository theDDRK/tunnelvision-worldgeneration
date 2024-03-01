(ns connectfour.core)

(def field ["-", "X", "O"])

(def empty-board
  "Empty board 7x6"
  (vec (repeat 7 (vec (repeat 6 (field 0))))))

(defn get-top-y
  "Determines the first available y-coordinate for a given x-coordinate."
  [board x]
  (first (filter #(= (field 0) (get-in board [x %])) (range 5 -1 -1))))

(defn insert
  "Inserts symbol for given player (either 1 or 2) at specified x."
  [board x y player-num]
  (assoc-in board [x y] (field player-num)))

(defn print-board
  [board]
  (let [transposed-board (apply map vector board)]
    (doseq [row transposed-board]
      (println row)))
  (println (range 0 7) "\n"))

(defn win-vertically?
  "Check if the player has won the game vertically (a row of the board vector)."
  [board x y player]
  (let [column (get-in board [x])]
    (if (>= (count (filter #(= % (field player)) column)) 4)
      (let [sublist (subvec column y (+ y 4))]
        (every? #(= % (field player)) sublist))
      false)))

(defn win-horizontally?
  "Check if the player has won the game horizontally (a column of the board vector)."
  [board x y player]
  (let [row (map #(get % y) board)]
    (if (>= (count (filter #(= % (field player)) row)) 4)
      (let [sublist (subvec (vec row) (- x (min x 3)) (+ x (- 3 (min x 3))))]
        (every? #(= % (field player)) sublist))
      false)))

(defn win-diagonally?
  "Check if the player has won the game diagonally from top left to bottom right."
  [board x y player direction]
  (let [diag (for [i (range -6 6)
                   :let [x' (+ x i) y' (direction y i)]
                   :when (and (>= x' 0) (< x' 7) (>= y' 0) (< y' 6))]
              (get-in board [x' y']))]
    (if (>= (count (filter #(= % (field player)) diag)) 4) 
      (let [sublist (for [i (range (- (count diag) 3))
                          :let [k (nth diag i) l (nth diag (+ i 1)) m (nth diag (+ i 2)) n (nth diag (+ i 3))]]
                      (if (every? #(= % (field player)) [k l m n])
                          true
                          false))]
          (some true? sublist))
      false)))

(defn win?
  "Check if the player has won the game."
  [board x y player]
  (boolean (or
            (win-vertically? board x y player)
            (win-horizontally? board x y player)
            (win-diagonally? board x y player +)
            (win-diagonally? board x y player -))))


(defn -main
  []
  (println "Welcome to Connect Four!\n")
  (let [board (atom empty-board)]

    (print-board @board)

    (loop [player 1]
      (println (str "Player " player ", please enter a column number: "))
      (let [x (read) y (get-top-y @board x)]
        (if (or (< x 0) (> x 6) (every? #(not= (field 0) %) (get-in @board [x])))
          (do
            (println "Invalid column number. Please try again.")
            (recur player))
          (do
            (swap! board #(insert % x y player))
            (print-board @board)
            (if (win? @board x y player)
              (do
                (println (str "Player " player " wins!"))
                (System/exit 0))
              (recur (if (= player 1) 2 1)))))))))

(-main)