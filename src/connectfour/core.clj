(ns connectfour.core)

(def field ["-", "X", "O"])
(def ai 4)
(def debug false)

(def empty-board
  "Empty board 7x6"
  (vec (repeat 7 (vec (repeat 6 (field 0))))))

(defn get-possible-locations
  "Get all possible locations for the AI to move."
  [board]
  (let [locations (for [x (range 7)
                        :when (some #(= % (field 0)) (get-in board [x]))]
                    x)]
    locations))

(defn get-top-y
  "Determines the first available y-coordinate for a given x-coordinate."
  [board x]
  (if (contains? (set (get-possible-locations board)) x)
    (first (filter #(= (field 0) (get-in board [x %])) (range 5 -1 -1)))
    nil))

(defn insert
  "Inserts symbol for given player (either 1 or 2) at specified x."
  [board x player-num]
  (let [y (get-top-y board x)]
    (if (and (integer? x) (integer? y))
      (assoc-in board [x y] (field player-num))
      (throw (IllegalArgumentException. (str "x and y must be integers x:" x "y:" y))))))

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
  (if (nil? y)
    false
    (boolean (or
              (win-vertically? board x y player)
              (win-horizontally? board x y player)
              (win-diagonally? board x y player +)
              (win-diagonally? board x y player -)))))

(defn draw?
  "Check if the game is a draw."
  [board]
  (every? #(not= (field 0) %) (flatten board)))

(defn score-window
  "Score a window for the AI."
  [window player]
  (let [opponent (if (= player 1) 2 1)]
    (cond
      (every? #(= % (field player)) window) 100,
      (and (= (count (filter #(= % (field player)) window)) 3) (= (count (filter #(= % (field 0)) window)) 1)) 5,
      (and (= (count (filter #(= % (field player)) window)) 2) (= (count (filter #(= % (field 0)) window)) 2)) 2,
      (and (= (count (filter #(= % (field player)) window)) 1) (= (count (filter #(= % (field 0)) window)) 3)) 1,
      (and (= (field player) (first window)) (= (field opponent) (second window)) (= (field 0) (nth window 2)) (= (field 0) (nth window 3))) -5,
      (and (= (field player) (first window)) (= (field player) (second window)) (= (field opponent) (nth window 2)) (= (field 0) (nth window 3))) -5,
      :else 0))
  )

(defn score-board
  "Score the board for the AI."
  [board player]
  (let [vertical (for [x (range 4) y (range 3)]
                   (vec (map #(get-in board [x (+ y %)]) (range 4))))
        horizontal (for [x (range 4) y (range 6)]
                     (vec (map #(get-in board [(+ x %) y]) (range 4))))
        diagonal-right (for [x (range 4) y (range 3)]
                   (vec (map #(get-in board [(+ x %) (+ y %)]) (range 4))))
        diagonal-left (for [x (range 6 2 -1) y (range 3)]
                     (vec (map #(get-in board [(+ x %) (+ y %)]) (range -4 0 -1))))]
    (reduce + (map #(score-window % player) (concat vertical horizontal diagonal-left diagonal-right)))))

(defn minimax
  "Minimax algorithm to determine the best move."
  [board player x depth]
  (if (or (< x 0) (> x 6))
    (throw (IllegalArgumentException. (str "x must be in range of 0-6 x:" x)))(flush))
  (cond
    (zero? depth) (score-board board player)
    (win? board x (get-top-y board x) player) (if (= player 1) -99999 99999)
    (draw? board) 0
    :else
    (+ (- (apply max (pmap #(minimax
                            (insert board % (if (= player 1) 2 1))
                            (if (= player 1) 2 1) % (dec depth))
                          (get-possible-locations board))))
       (score-board board player))))

(defn get-best-column
  "Get the column with the highest score."
  [board, coll]
  (apply max-key second
        (filter (comp not nil? second) (filter #(contains? (set (get-possible-locations board)) (first %)) coll))))

(defn get-best-move
  "Get the best move for the AI."
  [board player]
  (let [scores (into {} (pmap #(vector % (minimax (insert board % player) player % ai)) (get-possible-locations board)))]
    (if (true? debug) (println scores)(flush))
    (first (get-best-column board scores))))

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
                  (swap! board #(insert % x player))
                  (print-board @board)
                  (if (win? @board x y player)
                    (do
                      (println (str "Player " player " wins!"))
                      (System/exit 0))
                    (recur (if (= player 1) 2 1)))))))
          (do
            (println "AI is thinking...")
            (let [x (get-best-move @board player) y (get-top-y @board x)]
              (println (str "AI chose: " x "\n"))
              (swap! board #(insert % x player))
              (print-board @board)
              (if (win? @board x y player)
                (do
                  (println (str "Player " player " wins!"))
                  (System/exit 0))
                (recur (if (= player 1) 2 1))))))))))

(-main)