(ns erp12.cbgp-lite.benchmark.suite.art
  (:require [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [mikera.image.core :as mk]
            [image-grid.core :as ig]
            [image-grid.color :as color]))

(def global-encoding :hsv)

(def image-filename "images/box.jpg")

(def image
  (-> image-filename
      mk/load-image
      ig/mkimage->image-grid))

(defn global-cleaning-fn
  [out]
  (mod (/ out ;(+ out 100)
          500)
       1.0))

;; Ideas:
;; Note: can I just use down-sample lexicase?
;; It looks like the down-sample lexicase implementation on line 87 of ga.clj
;;    is broken -- you'll get different number of cases each time, instead of
;;    fixed number
;; TODO: One other interesting idea: the inputs don't just have to be the x-y coords
;;    of the pixel. You could use other coordinate systems (like circle)

;; new ideas:
;; - DSL might help


(defn clean-evolved-fns-OLD
  "Cleans up (evolved) fns so that they work with image-grid
   Parameters:
    - vector of 3 fns, each of which takes x and y as arguments and returns
      one component (either R, G, and B or H, S, and V) (which may be outside
      the range [0.0, 1.0]) 
    - the encoding type, which is either :rgb or :hsv
    - the cleaning-fn, which takes an output value float and ensures it falls
      in the range [0.0, 1.0]. Some examples:
      - using mod (default): #(mod % 1.0)
      - using thresholds: #(min 1.0 (max 0.0 %))
      - TODO: you could scale the output first, say [-100, 100] => [0, 1], before
        ensuring the range
   Return:
    - one function whose parameters are [x y] and [r g b] and returns
      a triple [r g b] of the new pixel"
  ([fns encoding]
   (clean-evolved-fns-OLD fns encoding #(mod % 1.0)))
  ([fns encoding cleaning-fn]
   (fn [[x y] _]
     (let [result-tuple (map #(cleaning-fn (% x y))
                             fns)]
       (case encoding
         :rgb result-tuple
         :hsv (color/hsv->rgb result-tuple))))))

(defn clean-evolved-fn
  "Cleans up (evolved) fn so that it works with image-grid
   Parameters:
    - fn that takes x and y as arguments and returns a tuple containing the
      3 components (either R, G, and B or H, S, and V) (which may be outside
      the range [0.0, 1.0]) 
    - the encoding type, which is either :rgb or :hsv
    - the cleaning-fn, which takes an output value float and ensures it falls
      in the range [0.0, 1.0]. Some examples:
      - using mod (default): #(mod % 1.0)
      - using thresholds: #(min 1.0 (max 0.0 %))
      - TODO: you could scale the output first, say [-100, 100] => [0, 1], before
        ensuring the range
   Return:
    - one function whose parameters are [x y] and [r g b] and returns
      a triple [r g b] of the new pixel"
  ([the-fn encoding]
   (clean-evolved-fn the-fn encoding #(mod % 1.0)))
  ([the-fn encoding cleaning-fn]
   (fn [[x y] _]
     (let [result-tuple (map cleaning-fn (the-fn x y))]
       (case encoding
         :rgb result-tuple
         :hsv (color/hsv->rgb result-tuple))))))


(defn sum-2-vals-case-generator
  "Produce a map of inputs and outputs.
   Works with any key generator function key-gen"
  [key-gen]
  (let [key1 (key-gen)
        key2 (key-gen)
        ; along with two guaranteed keys, this makes at most 50 kv pairs
        num-kvs (rand-int 49)
        the-keys (repeatedly num-kvs key-gen)
        the-vals (repeatedly num-kvs #(rand-int 1000))
        input-map (assoc (zipmap the-keys the-vals)
                         key1 (rand-int 1000)
                         key2 (rand-int 1000))]
    {:inputs [input-map key1 key2]
     :output (+ (get input-map key1) (get input-map key2))}))

(defn art-pixel-generator
  "Chooses a random pixel, and gets that pixel's RGB or HSV from the image."
  [encoding]
  (let [x (rand-int (ig/width image))
        y (rand-int (ig/height image))
        rgb (ig/get-pixel image [x y])
        hsv (color/rgb->hsv rgb)]
    {:inputs [(double x) (double y)]
     :output (case encoding
               :rgb rgb
               :hsv hsv)}))

(defn art-loss-fn
  "Given a component index (0, 1, or 2), and 
   actual and expected triples, computes loss after cleaning the data.
   Optional argument for cleaning-fn (see above)"
  ([component actual expected]
   (art-loss-fn component actual expected #(mod % 1.0)))
  ([component actual expected cleaning-fn]
   (bu/absolute-distance (cleaning-fn (nth actual component))
                         (nth expected component))))
   
   
  


(def art-problems
  {"sum-2-vals"
   {:description    (str "Given a map from strings to ints and two strings that are "
                         "keys of the map, look up the values associated with those keys "
                         "in the map and return their sum.")
    :input->type    {'input1 {:type :map-of, :key {:type 'string?}, :value {:type 'int?}}
                     'input2 {:type 'string?}
                     'input3 {:type 'string?}}
    :ret-type       lib/INT
    :other-type-ctors #{'boolean?}
    :extra-genes    [{:gene :lit, :val 0, :type {:type 'int?}}
                     {:gene :lit-generator,
                      :fn   (bu/string-generator 21),
                      :type {:type 'string?}}]
    :case-generator (fn sum-2-vals-gen []
                      (sum-2-vals-case-generator (bu/string-generator 10)))
    :loss-fns       [bu/absolute-distance]}

   "art"
   {:description    "Make some art!"
    :input->type    {'input1 lib/DOUBLE
                     'input2 lib/DOUBLE}
    :ret-type       (lib/tuple-of lib/DOUBLE lib/DOUBLE lib/DOUBLE)
    ;; added boolean for now, in case they're useful for if, but could remove
    :other-type-ctors #{'boolean?}
    :extra-genes    [{:gene :lit, :val 0.0, :type lib/DOUBLE}
                     {:gene :lit, :val 1.0, :type lib/DOUBLE}
                     {:gene :lit-generator, :fn rand, :type lib/DOUBLE}]
    ;; This isn't the final plan, which would use down-sampled lexicase, but allows picking
    ;; of fixed pixel locations
    :case-generator (partial art-pixel-generator global-encoding)
    :loss-fns       [#(art-loss-fn 0 %1 %2 global-cleaning-fn)
                     #(art-loss-fn 1 %1 %2 global-cleaning-fn)
                     #(art-loss-fn 2 %1 %2 global-cleaning-fn)]
    :problem-specific-report (fn [info]
                               (let [best (:best info)
                                     func (:func best)
                                     cleaned-func (clean-evolved-fn func global-encoding global-cleaning-fn)
                                     width (ig/width image)
                                     height (ig/height image)
                                     empty-image (ig/new-image-grid width height)
                                     img-grid (ig/map-image-grid cleaned-func empty-image)
                                     img (ig/image-grid->mkimage img-grid)
                                     filename (str "data/images/evo/generation" (:step info) ".png")]
                                 (mk/save img filename)))}})
   

(defn problems
  [{:keys [penalty]}]
  (update-vals art-problems
               ;; This adds nil penalties to all loss functions
               (fn [problem-map]
                 (update problem-map
                         :loss-fns
                         #(map (partial bu/penalize-nil-and-exception penalty) %)))))

(defn read-cases
  [{:keys [problem n-train n-test]}]
  (let [case-generator (get-in (problems nil)
                               [(name problem) :case-generator])]
    {:train (repeatedly n-train case-generator)
     :test  (repeatedly n-test case-generator)}))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  
  (ig/width image) 
  ;;=> 500
  (ig/height image)
  ;;=> 667

  (rand-int
   (ig/height image))
  

  ((partial rand-int 100))
  

  ;;  rgb, mod
  (let [cleaned (clean-evolved-fns [(fn [x y] (/ (+ x y) 200.0))
                                    (fn [x y] (lib/cos x))
                                    (fn [x y] (/ 1742.7 (+ x y 1.0)))]
                                   :rgb)
        width 500
        height 300
        empty-image (ig/new-image-grid width height)
        img-grid (ig/map-image-grid cleaned empty-image)
        img (ig/image-grid->mkimage img-grid)]
    (mk/save img "data/images/test-rgb.png") ;; save image as test.png 
    (mk/show img))           ;; show the image)
  
  (def cleaned-threshold (clean-evolved-fns [(fn [x y] (/ (+ x y) 200.0))
                                             (fn [x y] (lib/cos x))
                                             (fn [x y] (/ 1742.7 (+ x y 1.0)))]
                                            :rgb
                                            #(min 1.0 (max 0.0 %))))

  ;; rgb, threshold
  (let [cleaned (clean-evolved-fns [(fn [x y] (/ (+ x y) 200.0))
                                    (fn [x y] (lib/cos x))
                                    (fn [x y] (/ 1742.7 (+ x y 1.0)))]
                                   :rgb
                                   #(min 1.0 (max 0.0 %)))
        width 500
        height 300
        empty-image (ig/new-image-grid width height)
        img-grid (ig/map-image-grid cleaned empty-image)
        img (ig/image-grid->mkimage img-grid)]
    (mk/save img "data/images/test-threshold.png") ;; save image as test.png 
    (mk/show img))           ;; show the image)
  
  ;;  hsv, mod
  (let [cleaned (clean-evolved-fns [(fn [x y] (/ (+ x y) 200.0))
                                    (fn [x y] (lib/cos x))
                                    (fn [x y] (/ 1742.7 (+ x y 1.0)))]
                                   :hsv)
        width 500
        height 300
        empty-image (ig/new-image-grid width height)
        img-grid (ig/map-image-grid cleaned empty-image)
        img (ig/image-grid->mkimage img-grid)]
    (mk/save img "data/images/test-hsv.png") ;; save image as test.png 
    (mk/show img))           ;; show the image)
  
  ;;  hsv, mod - using single fn that returns a tuple
  (let [cleaned (clean-evolved-fn (fn [x y]
                                    (list (/ (+ x y) 200.0)
                                          (lib/cos x)
                                          (/ 1742.7 (+ x y 1.0))))
                                  :hsv)
        width 500
        height 300
        empty-image (ig/new-image-grid width height)
        img-grid (ig/map-image-grid cleaned empty-image)
        img (ig/image-grid->mkimage img-grid)]
    (mk/save img "data/images/test-hsv.png") ;; save image as test.png 
    (mk/show img))           ;; show the image)
  
;;;;;;;;;;
  (mk/show
   (let [width 500
         height 300
         empty-image (ig/new-image-grid width height)
         pixel-fn (fn [[x y] _]
                    [(/ y height) (/ x width) 0.3])
         img-grid (ig/map-image-grid pixel-fn empty-image)]
     (ig/image-grid->mkimage img-grid)))

  (let [img (-> (ig/new-image-grid 50 30)
                (ig/set-pixel [10 5] [0.8 0.0 1.0])
                (ig/set-pixel [25 20] [0.0 0.9 0.1])
                (ig/set-pixel [26 20] [0.0 0.9 0.1])
                (ig/set-pixel [27 20] [0.0 0.9 0.1])
                (ig/set-pixel [28 20] [0.0 0.9 0.1])
                (ig/set-pixel [29 20] [0.0 0.9 0.1])
                (ig/image-grid->mkimage))]
    (mk/save img "data/images/test.png") ;; save image as test.png 
    (mk/show img)))           ;; show the image
  
  