(ns worldgeneration.world-generation)

(def worldseed 1)
;; (def chunk-width 50)
;; (def chunk-height 50)
(def min-room-width 4)
(def min-room-height 7)
(def max-room-width 14)
(def max-room-height 20)
(def total-wall-width-per-room 2)
(def max-rooms 5)
(def max-rooms-per-corridor 3)
(def max-amount-of-attempts 5)

(def tile-types [:empty :corridor :door :room :wall-v :wall-h :wall-tl :wall-tr :wall-bl :wall-br])
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

(defn can-room-be-placed [chunk chunk-width chunk-height room-x room-y room-width room-height]
  (let [room-right  (+ total-wall-width-per-room (+ room-x room-width))
        room-bottom (+ total-wall-width-per-room (+ room-y room-height))]
    (println "Checking if room can be placed at" room-x room-y "with width" room-width "and height" room-height)
    (if (or (< room-x 1)
            (< room-y 1)
            (>= room-right (- chunk-width 1))
            (>= room-bottom (- chunk-height 1)))
      (do
        (println "Room can't be placed because it's out of bounds")
        false)
      (loop [i room-x j room-y]
        (if (and (< i room-right) (< j room-bottom))
          (if (not= (get-in chunk [i j]) :empty)
            (do
              (println "Room can't be placed because there's a tile at" i j)
              false)
            (recur (if (>= (inc j) room-bottom) (inc i) i)
                   (if (>= (inc j) room-bottom) room-y (inc j))))
          (do 
            (println "Room can be placed")
            true))))))

(defn generate-room-borders [chunk x y width height]
  (println "Generating room borders")
  (let [chunk-with-vertical-walls-left     (reduce #(assoc-in %1 [(+ x %2) y] :wall-v) chunk (range width))
        chunk-with-vertical-walls-right    (reduce #(assoc-in %1 [(+ x %2) (+ y height)] :wall-v) chunk-with-vertical-walls-left (range width))
        chunk-with-horizontal-walls-top    (reduce #(assoc-in %1 [x (+ y %2)] :wall-h) chunk-with-vertical-walls-right (range height))
        chunk-with-horizontal-walls-bottom (reduce #(assoc-in %1 [(+ x width) (+ y %2)] :wall-h) chunk-with-horizontal-walls-top (range height))
        chunk-with-tl-wall                 (assoc-in chunk-with-horizontal-walls-bottom [x y] :wall-tl)
        chunk-with-bl-wall                 (assoc-in chunk-with-tl-wall [(+ x width) y] :wall-bl)
        chunk-with-tr-wall                 (assoc-in chunk-with-bl-wall [x (+ y height)] :wall-tr)
        chunk-with-br-wall                 (assoc-in chunk-with-tr-wall [(+ x width) (+ y height)] :wall-br)]
    chunk-with-br-wall))

(defn generate-room [chunk x y width height]
  (println "Generating room at" x y "with width" width "and height" height)
  (let [updated-chunk      (reduce (fn [acc i]
                                     (reduce (fn [inner-acc j]
                                               (assoc-in inner-acc [(+ x i) (+ y j)] :room))
                                             acc
                                             (range height)))
                                   chunk
                                   (range width))
        chunk-with-borders (generate-room-borders updated-chunk x y width height)]
    chunk-with-borders))



(defn create-room [chunk random chunk-width chunk-height]
  (loop [attempts 0]
    (if (< attempts max-amount-of-attempts) 
      (let [room-width  (+ (.nextInt random (+ max-room-width total-wall-width-per-room)) min-room-width total-wall-width-per-room)
            room-height (+ (.nextInt random (+ max-room-height total-wall-width-per-room)) min-room-height total-wall-width-per-room)
            room-x      (.nextInt random (- chunk-width room-width))
            room-y      (.nextInt random (- chunk-height room-height))]
        (if (can-room-be-placed chunk chunk-width chunk-height room-x room-y room-width room-height)
          (let [chunk-with-room (generate-room chunk room-x room-y room-width room-height)]
            (println "Room generated")
            chunk-with-room)
          (recur (inc attempts))))
      (do 
        (println "Room couldn't be generated")
        chunk))))

(defn generate-chunk [chunk worldseed x y chunk-width chunk-height]
  (let [seed   (generate-chunkseed worldseed x y)
        random (java.util.Random. seed)]
    (println "Generating chunk with seed" seed)
    (loop [chunk chunk
           rooms  0]
      (if (< rooms max-rooms)
        (let [chunk-with-room (create-room chunk random chunk-width chunk-height)]
          (recur chunk-with-room (inc rooms)))
        chunk))))
