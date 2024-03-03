(ns connectfour.core)

(def field ["-", "X", "O"])
(def ai 3)

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

(defn check-four-in-a-row
  "Check if there are four in a row for a given player."
  [list player]
  (boolean (let [sublist (for [i (range (- (count list) 3))
                      :let [k (nth list i) l (nth list (+ i 1)) m (nth list (+ i 2)) n (nth list (+ i 3))]]
                  (if (every? #(= % (field player)) [k l m n])
                    true
                    false))]
    (some true? sublist))))

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
  (let [diag (for [i (range -6 7)
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

(defn draw?
  "Check if the game is a draw."
  [board]
  (every? #(not= (field 0) %) (flatten board)))

(defn get-possible-locations
  "Get all possible locations for the AI to move."
  [board]
  (let [locations (for [x (range 7)
                        :let [y (get-top-y board x)]
                        :when (not= y 6)]
                    [x y])]
    (shuffle locations)))

(defn score-window
  "Score a window for the AI."
  [window player]
  (let [opponent (if (= player 1) 2 1)]
    (cond
      (every? #(= % (field player)) window) 100,
      (and (count (filter #(= % (field player)) window)) 3 (count (filter #(= % (field 0)) window)) 1) 5,
      (and (count (filter #(= % (field player)) window)) 2 (count (filter #(= % (field 0)) window)) 2) 2,
      (and (count (filter #(= % (field opponent)) window)) 3 (count (filter #(= % (field 0)) window)) 1) -4)))


(defn score-board
  "Score the board for the AI."
  [board player]
  ;; make all possible windows of 4
  
  )

(defn minimax
  "AI algorithm to determine the best move."
  [board x y player depth maximising-player]
  (let [locations (get-possible-locations board)]
    (println locations)
    (if (or (= depth 0) (win? board x y player) (win? board x y (if (= player 1) 2 1)) (draw? board))
      (if (= depth 0)
        (score-board board player)
        (if (win? board x y player)
          (if (= player 1)
            (x -999999)
            (x 999999))
          (if (win? board x y (if (= player 1) 2 1))
            (if (= player 2)
              (x 999999)
              (x -999999))
            (x 0))))
      (if maximising-player
        (let [best-value -999999]
          (doseq [location locations]
            (let [value (minimax (insert board (first location) (second location) player) (first location) (second location) player (- depth 1) false)] 
              (set! best-value (max best-value (second value)))
              (println best-value)))
          best-value)
        (let [best-value 999999]
          (doseq [location locations]
            (let [value (minimax (insert board (first location) (second location) player) (first location) (second location) player (- depth 1) true)]
              (set! best-value (min best-value (second value)))
              (println best-value)))
          best-value)))))

(defn get-best-move
  "Get the best move for the AI."
  [board player]
  (let [x (minimax board 0 0 player ai true) y (get-top-y board x)]
    [x y]))

(defn -main
  []
  (println "Welcome to Connect Four!\n")
  (let [board (atom empty-board)]

    (print-board @board)

    (loop [player 1]
      (if (draw? @board)
        (do
          (println "It's a draw!")
          (System/exit 0))
        (if (= player 1)
          (do
            (println (str "Player " player ", please enter a column number: "))
            (let [x (read) y (get-top-y @board x)]
              (if (or (< x 0) (> x 6))
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
                    (recur (if (= player 1) 2 1)))))))
          (do
            (println "AI is thinking...")
            (let [[x y] (get-best-move @board player)]
                  (swap! board #(insert % x y player))
                  (print-board @board)
                  (if (win? @board x y player)
                    (do
                      (println (str "Player " player " wins!"))
                      (System/exit 0))
                    (recur (if (= player 1) 2 1))))))))))

(-main)