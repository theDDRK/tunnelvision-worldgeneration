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
      (let [sublist1 (when (>= x 3) (subvec (vec row) (- x 3) x))
            sublist2 (when (>= x 2) (subvec (vec row) (- x 2) (+ x 1)))
            sublist3 (when (>= x 1) (subvec (vec row) (- x 1) (+ x 2)))
            sublist4 (when (<= x (- (count row) 4)) (subvec (vec row) x (+ x 3)))]
        (or (every? #(= % (field player)) sublist1)
            (every? #(= % (field player)) sublist2)
            (every? #(= % (field player)) sublist3)
            (every? #(= % (field player)) sublist4)))
      false)))

(defn win-diagonally-left?
  "Check if the player has won the game diagonally from top left to bottom right."
  [board x y player]
  (let [diag (for [i (range (when (> (+ x y) 4) y 0) (+ x y))] (get-in board [(- x (- x i)) (- (+ y x) i)]))]
    (println diag)))

;; (defn win-diagonally?
;;   "Check if the player has won the game diagonally."
;;   [board x y player]
;;   (or (win-diagonally-left? board x y player)
;;       (win-diagonally-right? board x y player)))

(defn win?
  "Check if the player has won the game."
  [board x y player]
  (or (win-vertically? board x y player)
      (win-horizontally? board x y player)))


(defn -main
  []
  (println "Welcome to Connect Four!\n")
  (let [board (atom empty-board)]

    (print-board @board)

    (loop [player 1]
      (println (str "Player " player ", please enter a column number: "))
      (let [x (read) y (get-top-y @board x)]
        (if (or (< x 0) (> x 6))
          (do
            (println "Invalid column number. Please enter a number between 0 and 6.")
            (recur player))
          (do
            (swap! board #(insert % x y player))
            (print-board @board)
            (if (win-diagonally-left? @board x y player)
              (do
                (println (str "Player " player " wins!"))
                (System/exit 0))
              (recur (if (= player 1) 2 1)))))))))

(-main)