(ns advent-of-code.core
  (:require [digest :as digest]))

;; #1^M
;;

(defn parens [coll]
  (let [m {\( inc \) dec}]
    (reduce #((fnil (get m %2) identity) %1) 0 coll)))

(defn find-basement-entry [paren-coll]
  (let [m {\( inc \) dec}]
    (loop [[p & r] paren-coll loc 0 pos 0]
      (cond (neg? loc) pos
            (nil? p) [loc pos]
            :death (recur r ((fnil (get m p) identity) loc) (inc pos))))))

;; #2

(defn box [l w h]
  {:l l :w w :h h
   :area (+ (* 2 l w) (* 2 l h) (* 2 w h))
   :volume (* l w h)
   :perimeters (vector (* 2 (+ l w)) (* 2 (+ l h)) (* 2 (+ w h)))
   :min-side (min (* l w) (* l h) (* w h))})

(defn wrapping-paper [{:keys [area min-side]}]
  (+ area min-side))

(defn parse-size
  "12x88x10 -> [12 88 10]"
  [size]
  (->> (re-seq #"(\d+)x(\d+)x(\d+)" size)
       (first)
       (drop 1)
       (map #(Integer. %))))

;; (reduce + (map (comp wrapping-paper (partial apply box) parse-size) sizes)))

(defn ribbon [{:keys [perimeters volume]}]
  (+ volume (apply min perimeters)))

;; #3

(def dir-map {\^ [-1 0] \> [0 1] \v [1 0] \< [0 -1]})
(def start-world {:world {[0 0] 1} :pos [0 0]})

(defn move [dir-map]
  (fn [pos dir]
    (vec (map + pos (get dir-map dir)))))

(def move-dir (move dir-map))
(defn move-in-world [{:keys [pos] :as state} dir]
  (assoc state :pos (move-dir pos dir)))

(defn entregar [{:keys [world pos] :as state}]
  (update-in state [:world pos] (fnil inc 0)))

(defn entregas [start-world ms]
  (reduce #(entregar (move-in-world %1 %2)) start-world ms))

;; count positions to which any delivery was made
;; (reduce-kv #(if ((complement zero?) %3) (inc %1) %1) 0 (:world (entregas start-world ">>>><vv<<^>>"))))

(defn every-other [coll]
  (reduce (fn [[pr rr :as p] [ep er]]
            (assoc p 0 (conj pr ep) 1 (conj rr er)))
          [[] []]
          (partition 2 coll)))

(defn robot-entregas [start-world delivery-lists]
  (loop [current-world start-world [p-list & r-lists] delivery-lists]
    (if (nil? p-list) current-world
      (let [new-world (entregas current-world p-list)]
        (recur (assoc new-world :pos [0 0]) r-lists)))))

;; #4

(defn leading-zeros-md5 [zeros s]
  (let [the-zeros (apply str (repeat zeros \0))]
    (ffirst
      (filter #(= the-zeros (apply str (take zeros (get % 1))))
              (map (fn [n]
                     (vector n (digest/md5 (str s n))))
                   (iterate inc 0))))))

(defn flzmd5-tail [s]
  (let [five-zeros (apply str (repeat 5 \0))]
    (loop [{:keys [n md5] :as thurk} {:n 0 :md5 "notehunt"}]
      (if (= five-zeros (apply str (take 5 md5)))
        n
        (recur (assoc thurk :n (inc n) :md5 (digest/md5 (str s ((fnil inc 0) n)))))))))

;; #5

(defn contains-chars? [cs n s]
  (<= n (count (keep (set cs) s))))

(defn any-repeated-letter? [s]
  ((complement nil?) (re-find #"(.)\1{1,}" s)))

(defn any-repeated-pair? [s]
  ((complement nil?) (re-find #"(\w\w).*\1" s)))

(defn one-repeat-over-valla? [s]
  ((complement nil?) (re-find #"(\w)\w\1" s)))

(defn contains-no-string? [ss s]
  (reduce #(and %1 (not (re-find (re-pattern %2) s))) true ss))

(defn is-nice? [s]
  (reduce #(and %1 %2) true
          (map #(%  s)
               [(partial contains-chars?  "aeiou" 3)
                any-repeated-letter?
                (partial contains-no-string? ["ab" "cd" "pq" "xy"])])))

(defn is-extremely-nice? [s]
  (reduce #(and %1 %2) true
          (map #(% s)
               [any-repeated-pair? one-repeat-over-valla?])))

;; #6A -- coordinates [x y]
;
(defn make-grid [tamano default]
  (vec (repeat tamano (vec (repeat tamano default)))))
(def star-grid (make-grid 1000 false))
(def star-grid-part-2 (make-grid 1000 0))
(def actions-part-1 {"turn on" (fn [_] true)
                     "turn off" (fn [_] false)
                     "toggle" not})
(def actions-part-2 {"turn on" inc
                     "turn off" (fn [n]
                                  (if-not (zero? n) (dec n) n))
                     "toggle" #(+ 2 %)})

(defn get-in-grid [grid [ci-x ci-y] [cd-x cd-y]]
  (->> (drop ci-y grid)
       (take (inc (- cd-y ci-y)))
       (vec)
       (map #(drop ci-x %))
       (map #(vec (take (inc (- cd-x ci-x)) %))))) ;

(defn update-in-grid [grid [ci-x ci-y] [cd-x cd-y] f]
  (reduce (fn [res [x y]]
            (update-in res [y x] f))
          grid
          (for [y (range ci-y (inc cd-y))
                x (range ci-x (inc cd-x))]
            [x y])))

(defn update-grid
  "Typical command: turn on 887,9 through 959,629"
  [grid actions command]
  (if-let [[p c ci-x ci-y cd-x cd-y] (first (re-seq #"([\w\s]+)\s(\d+),(\d+)\sthrough\s(\d+),(\d+)" command))]
    (do
      ;; (println p)
      (update-in-grid grid (vector (Integer. ci-x) (Integer. ci-y)) (vector (Integer. cd-x) (Integer. cd-y)) (get actions c)))
    grid))
