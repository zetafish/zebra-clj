(ns zebra-clj.core
  (:require [clojure.string :as str]
            [loco.constraints :refer :all]
            [loco.core :refer [solutions solution]])
  (:gen-class))


(def model
  [($in :x 1 6) ; x is in the domain ranging from 1 to 6, inclusive
   ($in :y 3 7) ; y is in the domain ranging from 3 to 7, inclusive
   ($= ($+ :x :y) 10)])


(def model
  [($in :x 0 1)
   ($in :_y 0 1)
   ($= ($+ :x :_y) 2)])

;; A + (B * C) = D
(def model
  [($in :a 1 5)
   ($in :b 1 5)
   ($in :c 3 5)
   ($in :d 1 5)
   ($= ($+ :a ($* :b :c)) :d)])

(def model
  [($in :a 1 5)
   ($in :b 3 4)
   ($= :a :b)])

(solutions model)

;; 1. There are five houses.
;; 2. The Englishman lives in the red house.
;; 3. The Spaniard owns the dog.
;; 4. Coffee is drunk in the green house.
;; 5. The Ukrainian drinks tea.
;; 6. The green house is immediately to the right of the ivory house.
;; 7. The Old Gold smoker owns snails.
;; 8. Kools are smoked in the yellow house.
;; 9. Milk is drunk in the middle house.
;; 10. The Norwegian lives in the first house.
;; 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
;; 12. Kools are smoked in the house next to the house where the horse is kept.
;; 13. The Lucky Strike smoker drinks orange juice.
;; 14. The Japanese smokes Parliaments.
;; 15. The Norwegian lives next to the blue house.


(def values
  {:color [:red :green :ivory :yellow :blue]
   :smoke [:old-gold :kools :chesterfields :lucky-strike :parliaments]
   :drink [:coffee :tea :milk :orange-juice :water]
   :nationality [:englishman :spaniard :ukrainian :norwegian :japanese]
   :pet [:dog :snails :fox :horse :zebra]
   :number [:1 :2 :3 :4 :5]})

(defn rule= [[k1 v1] [k2 v2]]
  ($=
   [k1 (.indexOf (values k1) v1)]
   [k2 (.indexOf (values k2) v2)]))

(defn rule-LR [[k1 v1] [k2 v2]]
  (let [i1 (.indexOf (values k1) v1)
        i2 (.indexOf (values k2) v2)]
    (apply $or
           (map (fn [i] ($and ($= [:number i] [k1 i1])
                              ($= [:number (inc i)] [k2 i2])))
                (range 4)))))

(defn rule-next-to [a b]
  ($or (rule-LR a b)
       (rule-LR b a)))

(defn gen-vars [k]
  (let [kws (map #(vec [k %]) (range 5))]
    (conj
     (map #($in % 0 4) kws)
     ($distinct kws))))

(defn gen-vars2 [kk]
  (mapcat gen-vars kk))

(defn model []
  (-> (gen-vars2 (keys values))
      (conj (rule= [:nationality :englishman] [:color :red])
            (rule= [:nationality :spaniard] [:pet :dog])
            (rule= [:drink :coffee] [:color :green])
            (rule= [:nationality :ukrainian] [:drink :tea])
            (rule-LR [:color :ivory] [:color :green])
            (rule= [:smoke :old-gold] [:pet :snails])
            (rule= [:smoke :kools] [:color :yellow])
            (rule= [:drink :milk] [:number :3])
            (rule= [:nationality :norwegian] [:number :1])
            (rule-next-to [:smoke :chesterfields] [:pet :fox])
            (rule-next-to [:smoke :kools] [:pet :horse])
            (rule= [:smoke :lucky-strike] [:drink :orange-juice])
            (rule= [:nationality :japanese] [:smoke :parliaments])
            (rule-next-to [:nationality :norwegian] [:color :blue]))))

(defn index->kw [[k i]]
  [k (get-in values [k i])])

(defn explain [solution]
  (->> solution
       (group-by second)
       (map (fn [[_ v]] (map first v)))
       (map (fn [x] (map #(index->kw %) x)))
       (map #(into {} %))))

(defn matches [record key val])

(defn select [s key val]
  (filter #(= val (key %)) s))

(defn solve [model]
  (explain (solution model)))

(defn show [s]
  (let [h (sort-by :number s)]
    (map println (map vals h))))

(solve model)
(select (solve model) :drink :water)
(select (solve model) :pet :zebra)
