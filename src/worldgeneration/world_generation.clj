(ns worldgeneration.world-generation
  (:require [clojure.data.priority-map :refer [priority-map-by]]))


(def worldseed 1)
;; (def chunk-width 50)
;; (def chunk-height 50)
(def min-room-width 4)
(def min-room-height 7)
(def max-room-width 14)
(def max-room-height 20)
(def total-wall-width-per-room 2)
(def max-rooms 5)
(def max-amount-of-attempts 5)

(def tile-types [:empty :corridor :door :room :wall-v :wall-h :wall-tl :wall-tr :wall-bl :wall-br])
(def side [:top :right :bottom :left])
(def tile-text-map
  {:empty    " "
   :corridor "#"
   :door     "/"
   :wall-v   "|"
   :wall-h   "-"
   :wall-tl  "┌"
   :wall-tr  "┐"
   :wall-bl  "└"
   :wall-br  "┘"
   :room     " "})

(defn empty-chunk [chunk-width chunk-height] 
  (vec (repeat chunk-height (vec (repeat chunk-width :empty)))))

(defn generate-chunkseed [worldseed x y]
  (hash [worldseed x y]))

(defn room-placeable? [chunk chunk-width chunk-height room-x room-y room-width room-height]
  (let [room-right  (+ (+ room-x 1) room-width)
        room-bottom (+ (+ room-y 1) room-height)
        room-x      (- room-x 1)
        room-y      (- room-y 1)]
    ;; (println "Checking if room can be placed at" room-x room-y "with width" room-width "and height" room-height)
    (if (or (< room-x 1)
            (< room-y 1)
            (>= room-right (- chunk-width 1))
            (>= room-bottom (- chunk-height 1)))
      (do
        ;; (println "Room can't be placed because it's out of bounds")
        false)
      (let [room-tiles (for [i (range room-x (+ room-right 1))
                             j (range room-y (+ room-bottom 1))]
                         (get-in chunk [i j]))]
        (if (some #(not= :empty %) room-tiles)
          (do
            ;; (println "Room can't be placed because there's something in the way")
            false)
          true)))))

(defn get-doors [chunk chunk-width chunk-height]
  (for [i (range chunk-width)
        j (range chunk-height)
        :let [tile (get-in @chunk [i j])]
        :when (= tile :door)]
    [i j]))

(defn generate-room-borders [chunk x y width height]
  ;; (println "Generating room borders")
  (let [chunk-with-vertical-walls-left     (reduce #(assoc-in %1 [(+ x %2) y] :wall-v) chunk (range width))
        chunk-with-vertical-walls-right    (reduce #(assoc-in %1 [(+ x %2) (+ y height)] :wall-v) chunk-with-vertical-walls-left (range width))
        chunk-with-horizontal-walls-top    (reduce #(assoc-in %1 [x (+ y %2)] :wall-h) chunk-with-vertical-walls-right (range height))
        chunk-with-horizontal-walls-bottom (reduce #(assoc-in %1 [(+ x width) (+ y %2)] :wall-h) chunk-with-horizontal-walls-top (range height))
        chunk-with-tl-wall                 (assoc-in chunk-with-horizontal-walls-bottom [x y] :wall-tl)
        chunk-with-bl-wall                 (assoc-in chunk-with-tl-wall [(+ x width) y] :wall-bl)
        chunk-with-tr-wall                 (assoc-in chunk-with-bl-wall [x (+ y height)] :wall-tr)
        chunk-with-br-wall                 (assoc-in chunk-with-tr-wall [(+ x width) (+ y height)] :wall-br)]
    chunk-with-br-wall))

(defn add-door [chunk x y side]
  ;; (println "Adding door at" x y "on the" side)
  (if (= side :top)
    (let [chunk-with-door (assoc-in chunk [x y] :door)
          door-l-side       (assoc-in chunk-with-door [(- x 1) y] :wall-br)
          door-r-side       (assoc-in door-l-side [(+ x 1) y] :wall-tr)]
      door-r-side)
    (if (= side :right)
      (let [chunk-with-door (assoc-in chunk [x y] :door)
            door-t-side       (assoc-in chunk-with-door [x (- y 1)] :wall-tr)
            door-b-side       (assoc-in door-t-side [x (+ y 1)] :wall-tl)]
        door-b-side)
      (if (= side :bottom)
        (let [chunk-with-door (assoc-in chunk [x y] :door)
              door-l-side       (assoc-in chunk-with-door [(- x 1) y] :wall-bl)
              door-r-side       (assoc-in door-l-side [(+ x 1) y] :wall-tl)]
          door-r-side)
        (if (= side :left)
          (let [chunk-with-door (assoc-in chunk [x y] :door)
                door-t-side       (assoc-in chunk-with-door [x (- y 1)] :wall-br)
                door-b-side       (assoc-in door-t-side [x (+ y 1)] :wall-bl)]
            door-b-side)
          chunk)))))

(defn generate-room-doors [chunk x y width height random]
  ;; (println "Generating room doors")
  (if (.nextBoolean random)
    (let [min (+ x total-wall-width-per-room)
          door-x (+ min (.nextInt random (- width (* 2 total-wall-width-per-room))))]
      (if (.nextBoolean random)
        (let [room (add-door chunk door-x y :top)]
          room)
        (let [room (add-door chunk door-x (+ y height) :bottom)]
          room)))
    (let [min (+ y total-wall-width-per-room)
          door-y (+ min (.nextInt random (- height (* 2 total-wall-width-per-room))))]
      (if (.nextBoolean random)
        (let [room (add-door chunk x door-y :left)]
          room)
        (let [room (add-door chunk (+ x width) door-y :right)]
          room)))))

  

(defn generate-room [chunk-atom x y width height random]
  ;; (println "Generating room at" x y "with width" width "and height" height)
  (swap! chunk-atom #(reduce (fn [acc i]
                               (reduce (fn [inner-acc j]
                                         (assoc-in inner-acc [(+ x i) (+ y j)] :room))
                                       acc
                                       (range height)))
                             %
                             (range width)))
  (swap! chunk-atom #(generate-room-borders % x y width height))
  (swap! chunk-atom #(generate-room-doors % x y width height random)))


(defn create-room [chunk random chunk-width chunk-height]
  (loop [attempts 0]
    (if (< attempts max-amount-of-attempts)
      (let [room-width  (+ (.nextInt random max-room-width) min-room-width total-wall-width-per-room)
            room-height (+ (.nextInt random max-room-height) min-room-height total-wall-width-per-room)
            room-x      (.nextInt random (- chunk-width room-width))
            room-y      (.nextInt random (- chunk-height room-height))]
        (if (room-placeable? @chunk chunk-width chunk-height room-x room-y room-width room-height)
          (do
            (generate-room chunk room-x room-y room-width room-height random)
            ;; (println "Room generated")
            @chunk)
          (recur (inc attempts))))
      (do
        ;; (println "Room couldn't be generated")
        @chunk))))

(defn manhatten-distance [start end]
  (let [dx (- (first end) (first start))
        dy (- (second end) (second start))]
    (+ (Math/abs dx) (Math/abs dy))))


(defn cost [curr start end]
  (let [g (manhatten-distance start curr)
        h (manhatten-distance curr end)
        f (+ g h)]
    [f g h]))

(defn get-possible-directions [chunk x y chunk-width chunk-height]
  (let [possible-directions [[-1 0] [1 0] [0 -1] [0 1]]
        filtered-directions (filter (fn [[dx dy]]
              (let [new-x (+ x dx)
                    new-y (+ y dy)
                    tile  (get-in @chunk [new-x new-y])]
                (and (>= new-x 0)
                     (>= new-y 0)
                     (< new-x chunk-width)
                     (< new-y chunk-height) 
                     (or (= :empty tile)
                         (= :corridor tile)
                         (= :door tile)))))
            possible-directions)]
    (if (not-empty (filter #(= :corridor (get-in @chunk [(+ x (first %)) (+ y (second %))])) filtered-directions))
      (filter #(= :corridor (get-in @chunk [(+ x (first %)) (+ y (second %))])) filtered-directions)
      filtered-directions)))

(defn reconstruct-path [came-from current]
  (loop [current current
         path []]
    (if (nil? current)
      (reverse path)
      (recur (get came-from current) (conj path current)))))


(defn search
  ([chunk width height start end]
   (let [open   (priority-map-by
                 (fn [x y]
                   (if (= x y)
                     0
                     (let [[f1 _ h1] x
                           [f2 _ h2] y]
                       (if (= f1 f2)
                         (if (< h1 h2) -1 1)
                         (if (< f1 f2) -1 1)))))
                 start (cost start start end))
         closed {}]
     (search chunk width height start end open closed {start 0})))
  
  ([chunk width height start end open closed g-score]
 (if (empty? open)
   (do
     (println "No path found")
     [])
   (let [[curr [f g h]] (peek open)
         open (pop open)]
     (if (= curr end)
       (reconstruct-path closed curr)
       (let [neighbors (get-possible-directions chunk (first curr) (second curr) width height)]
         (loop [neighbors neighbors
                open open
                closed closed
                g-score g-score]
           (if (empty? neighbors)
             (search chunk width height start end open closed g-score)
             (let [[dx dy] (first neighbors)
                   neighbor [(+ (first curr) dx) (+ (second curr) dy)]
                   tentative-g-score (+ (get g-score curr) 1)] ; Assuming all edges have the same weight
               (if (or (not (contains? g-score neighbor)) (< tentative-g-score (get g-score neighbor)))
                 (let [closed (assoc closed neighbor curr)
                       g-score (assoc g-score neighbor tentative-g-score)
                       f-score (+ tentative-g-score (manhatten-distance neighbor end))
                       open (assoc open neighbor [f-score tentative-g-score (manhatten-distance neighbor end)])]
                   (recur (rest neighbors) open closed g-score))
                 (recur (rest neighbors) open closed g-score)))))))))))

(defn generate-corridor [chunk path]
  (reduce (fn [acc [x y]]
            (assoc-in acc [x y] :corridor))
          @chunk
          path))

(defn generate-corridors-between-rooms [chunk chunk-width chunk-height doors]
  (println "Doors" doors)
  (doseq [door doors]
    (let [other-doors (remove #(= % door) doors)
          paths       (for [other-door other-doors]
                        (search chunk chunk-width chunk-height door other-door))]
      (doseq [path paths]
        (swap! chunk (fn [c] (generate-corridor (atom c) path))))))
  @chunk)


(defn generate-corridors-to-chunk-borders [chunk chunk-width chunk-height doors]
  (let [edges [[0 (/ chunk-height 2)]
               [(/ chunk-width 2) 0]
               [chunk-width (/ chunk-height 2)]
               [(/ chunk-width 2) chunk-height]]]
    (doseq [edge edges]
      (let [closest-door (first (sort-by (fn [[x y]] (manhatten-distance edge [x y])) doors))
            path         (search chunk chunk-width chunk-height edge closest-door)]
        (swap! chunk (fn [c] (generate-corridor (atom c) path)))))))

(defn reset-door-tiles [chunk doors]
  (doseq [door doors]
    (swap! chunk (fn [c] (assoc-in c door :door)))))

(defn generate-corridors [chunk chunk-width chunk-height]
  (let [doors (get-doors chunk chunk-width chunk-height)]
    (generate-corridors-between-rooms chunk chunk-width chunk-height doors)
    (generate-corridors-to-chunk-borders chunk chunk-width chunk-height doors)
    (reset-door-tiles chunk doors)
    )
  @chunk)

(defn generate-rooms [chunk chunk-width chunk-height random]
  (loop [rooms  0]
    (if (< rooms max-rooms)
      (do
        (swap! chunk (fn [c] (create-room (atom c) random chunk-width chunk-height)))
        (recur (inc rooms)))
      @chunk)))


(defn generate-chunk [chunk worldseed x y chunk-width chunk-height]
  (let [seed   (generate-chunkseed worldseed x y)
        random (java.util.Random. seed)]
    (println "Generating chunk with seed" seed)
    (swap! chunk (fn [c] (generate-rooms (atom c) chunk-width chunk-height random)))
    (swap! chunk (fn [c] (generate-corridors (atom c) chunk-width chunk-height))))
  @chunk)

