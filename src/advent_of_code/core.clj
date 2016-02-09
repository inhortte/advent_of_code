(ns advent-of-code.core
  (:require [digest :as digest]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code.combinatorics :as combo]
            [clojure.data.json :as json]))

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

;; #6

(def command-pattern (re-pattern #"^([\p{Lower}\d]+)\s+(\p{Upper}+)\s([\p{Lower}\d]+)\s->\s(\p{Lower}+)"))
(def assign-pattern (re-pattern #"^([\p{Lower}\d]+)\s->\s(\p{Lower}+)"))
(def not-pattern (re-pattern #"^(NOT)\s([a-z\d]+)\s->\s([a-z]+)"))
(def is-positive-integer? (comp empty? (partial drop-while #(Character/isDigit %))))

(defn wire-fn
  "wires is always a vector"
  [f wires wire-map]
  ;; (println (str wire-map))
  (when ((complement some) nil? (map (comp (partial get wire-map) keyword)
                                     (filter (complement is-positive-integer?) wires)))
    (let [b (apply f (map #(if (is-positive-integer? %)
                              (Integer. %)
                              (get wire-map (keyword %))) wires))]
      (if (neg? b) (+ 65536 b) b))))

(def ops {"AND" (partial wire-fn bit-and)
          "OR" (partial wire-fn bit-or)
          "NOT" (partial wire-fn bit-not)
          "LSHIFT" (partial wire-fn bit-shift-left)
          "RSHIFT" (partial wire-fn bit-shift-right)
          "ID" (partial wire-fn identity)})

(defn parse-circuit-line [line]
  (let [[op wires output] (or (when-let [[_ input output] (re-matches assign-pattern line)]
                                ["ID" [input] output])
                              (when-let [[_ in1 command in2 output] (re-matches command-pattern line)]
                                [command [in1 in2] output])
                              (when-let [[_ command input output] (re-matches not-pattern line)]
                                [command [input] output])
                              (throw (ex-info "bad times, vole" {:line line})))]
    {:op op
     :inputs wires
     :output output}))

(defn op-fn [{:keys [op inputs output]} wire-map]
  (if-let [res ((get ops op) inputs wire-map)]
    (assoc wire-map (keyword output) res)
    wire-map))

;; #8

(defn hex-string->dec [hs]
  (let [hex-map (zipmap 
                  (concat
                    (map (comp keyword str) (range 0 10))
                    (map (comp keyword str char) (range 97 103))
                    (map (comp keyword str char) (range 65 71)))
                  (concat (range 16) (range 10 16)))]
    ;; (println (str hex-map))
    (reduce #(+ (* 16 %1) (get hex-map ((comp keyword str) %2))) 0 hs)))

(defn decode-xs [s]
  (let [xs (map second (re-seq #"\\x([a-f0-9]{2})" s))
        x-map (zipmap xs (map (comp str char hex-string->dec) xs))]
    (reduce #(string/replace-first %1 (re-pattern (str "\\\\x" %2)) (string/re-quote-replacement (get x-map %2))) s xs)))

(defn decode-otros [s]
  (-> (string/replace s #"\\\"" "\"")
      (string/replace #"[\\]{2}" (str (char 92) (char 92)))))

;; #9

;; (def city-distances (string/split (slurp "resources/cities.txt") #"\n"))
;; (def city-map (reduce (fn [res line]
;;x                        (let [[_ city1 city2 d] (re-matches #"^(\w+)\sto\s(\w+)\s=\s(\d+)" line)]
;;                           (-> (update-in res [:cities] conj (keyword city1) (keyword city2))
;;                               (assoc-in [:distances (vector (keyword city1) (keyword city2))] (Integer. d))
;;                               (assoc-in [:distances (vector (keyword city2) (keyword city1))] (Integer. d)))))
;;                       {:cities #{} :distances {}}
;;                       city-distances))
;; (def city-perms (combo/permutations (:cities city-map)))

;; find minimal distance in a circuit through all cities
;; (apply min (map (comp (partial reduce + ) (fn [path] (map (comp (partial (:distances city-map)) vec) (partition 2 1 path)))) city-perms)))

;; 10

(defn look-and-say
  "n is a STRING!"
  [n]
  (letfn [(count-um-up [n]
            (map #(hash-map :el (first %) :count (count %))
                 (partition-by identity n)))]
    (->> (reduce #(conj %1 (:count %2) (:el %2)) [] (count-um-up n))
      (apply str))))

;; 11

(def three-consecutive?
  (partial re-seq (re-pattern (str "(" (clojure.string/join "|" (map #(str (char %) (char (inc %)) (char (+ 2 %))) (range 97 121))) ")"))))
(def i-o-or-l?
  (partial re-seq #"(i|o|l)"))
(def two-doubles?
  (partial re-seq #"([a-z])\1.*([a-z])\2"))

(defn valid-santa-password? [pwd]
  (every? (fn [x]
            (not (or (nil? x) (false? x)))) (map #(% pwd) [three-consecutive? (complement i-o-or-l?) two-doubles?])))

(defn inc-char
  "inicio and termino are integers or chars"
  [inicio termino s]
  (let [inicio (if (= java.lang.Character (class inicio))
                 (int inicio) inicio)
        termino (if (= java.lang.Character (class termino))
                  (int termino) termino)
        m (- (inc termino) inicio)]
    (loop [res []  [p & r] (reverse s) carry true]
      (if (nil? p)
        (apply str (reverse (map char (if carry (conj res inicio) res))))
        (let [v (if carry
                  (+ inicio (mod (- (inc (int p)) inicio) m))
                  (int p))]
          (recur (conj res v) r (and carry (= (int p) termino))))))))

(def inc-lc-char (partial inc-char 97 122))

(defn inc-valid-santa-char [s]
  (loop [s s]
    (if (valid-santa-password? s) s
      (recur (inc-lc-char s)))))

;; #12

(defn add-json-numbers [json]
  (cond (number? json) json
        (empty? json) 0
        (map? json) (apply + (concat (map add-json-numbers (keys json)) (map add-json-numbers (vals json))))
        (string? json) 0
        :otro (apply + (map add-json-numbers json))))


(defn add-non-red-json-numbers [json]
  (cond (number? json) json
        (empty? json) 0
        (map? json) (let [vs (vals json)]
                      (println (str json))
                      (if (some #{"red"} vs) 0
                        (apply + (concat (map add-non-red-json-numbers (keys json)) (map add-non-red-json-numbers vs)))))
        (string? json) 0
        :otro (apply + (map add-non-red-json-numbers json))))

;; #13

(def circular-archivo "resources/seating.txt")
(def ca-conmigo "resources/seating-with-me.txt")
(defn circle-map [archivo]
  (let [re #"^(\w+)\swould\s(gain|lose)\s(\d+)\shappiness\sunits\sby\ssitting\snext\sto\s(\w+)\."]
    (reduce (fn [cmap line]
              (if-let [[_ h1 gl c h2] (re-matches re line)]
                (-> (update-in cmap [:humanos] conj (keyword h1) (keyword h2))
                    (assoc-in [:rels (keyword h1) (keyword h2)] (* (if (= "lose" gl) -1 1) (Integer. c))))))
            {:humanos #{}} (string/split (slurp archivo) #"\n"))))

(defn happinesses [cmap]
  (let [cperms (map vec (combo/permutations (:humanos cmap)))]
    (map (fn [seating]
           (reduce (fn [res [p r]]
                     (+ res (get-in cmap [:rels p r])))
                   0
                   (concat (partition 2 1 (conj seating (first seating)))
                           (partition 2 1 (conj ((comp vec reverse) seating) (last seating))))))
         cperms)))

;; #14

(def reno-archivo "resources/renos.txt")

(defn reno [nombre velocidad segundos-de-movimiento segundos-de-reposo]
  (letfn [(movimiento [{:keys [s-mov s-rep s-rem dis] :as estado}]
            #(cond (zero? s-rem) estado
                   (zero? s-mov) (reposo (assoc estado :s-rep segundos-de-reposo))
                   :else (movimiento (assoc estado :s-mov (dec s-mov) :s-rem (dec s-rem) :dis (+ dis velocidad)))))
          (reposo [{:keys [s-mov s-rep s-rem dis] :as estado}]
            #(cond (zero? s-rem) estado
                   (zero? s-rep) (movimiento (assoc estado :s-mov segundos-de-movimiento))
                   :elso (reposo (assoc estado :s-rep (dec s-rep) :s-rem (dec s-rem)))))]
    (fn [duracion] (trampoline movimiento {:nombre nombre
                                           :s-mov segundos-de-movimiento
                                           :s-rep 0
                                           :s-rem duracion
                                           :dis 0}))))

(defn eval-renos [archivo duracion]
  (let [re #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\."
        renos (map (fn [rline]
                     (when-let [[_ n v m r] (re-matches re rline)]
                       ((reno n (Integer. v) (Integer. m) (Integer. r)) duracion)))
                   (string/split (slurp archivo) #"\n"))]
    renos))

(defn reindeer [nombre velocidad s-mov s-rep]
  {:nombre nombre
   :vel velocidad
   :s-mov-left s-mov
   :s-mov s-mov
   :s-rep-left 0
   :s-rep s-rep
   :dis 0
   :state :movimiento})

(defn next-state-reindeer [{:keys [vel s-mov-left s-mov s-rep-left s-rep dis state] :as rd}]
  (case state
    :movimiento (if (zero? s-mov-left)
                  (next-state-reindeer (assoc rd :s-rep-left s-rep :state :reposo))
                  (assoc rd :s-mov-left (dec s-mov-left) :dis (+ dis vel)))
    :reposo (if (zero? s-rep-left)
              (next-state-reindeer (assoc rd :s-mov-left s-mov :state :movimiento))
              (assoc rd :s-rep-left (dec s-rep-left)))
    rd))

(defn init-reindeers [archivo]
  (map (fn [rline]
         (when-let [[_ n v m r] (re-matches #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\." rline)]
           (reindeer n (Integer. v) (Integer. m) (Integer. r))))
       (string/split (slurp archivo) #"\n")))

(defn move-reindeers [rds]
  (let [next-state (sort #(- (:dis %2) (:dis %1)) (map next-state-reindeer rds))
        win-score (:dis (first next-state))
        winners (take-while (comp (partial = win-score) :dis) next-state)
        guppies (drop-while (comp (partial = win-score) :dis) next-state)]
    (concat (map #(update-in % [:wins] (fnil inc 0)) winners) guppies)))

;; #15

(def ingr-archivo "resources/ingr-list.txt")
(defn ingr-parse [archivo]
  (reduce (fn [ingrs ingr]
            (let [[_ n cap d f t cal] (re-matches 
                                        #"^(\w+): capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories ([-\d]+)$"
                                        ingr)]
              (assoc ingrs (keyword n) [(Integer. cap) (Integer. d) (Integer. f) (Integer. t) (Integer. cal)])))
          {} (string/split (slurp archivo) #"\n")))

(defn apply-ingrs
  "ingr-keys because it has to remain the same each time"
  [ingrs ingrs-keys v]
  (hash-map
    :calories (reduce + (map (fn [key qty]
                               (* (get (get ingrs key) 4) qty))
                             ingrs-keys v))
    :score (reduce *
                   (map (fn [val]
                          (if (neg? val) 0 val))
                        (map (fn [index]
                               (reduce + (map (fn [key qty]
                                                (* (get (get ingrs key) index) qty))
                                              ingrs-keys v)))
                             (range (count v)))))))

(defn ingr-combs [ingrs combs]
  (let [ingrs-keys (keys ingrs)]
    (map (fn [comb]
           (apply-ingrs ingrs ingrs-keys comb))
         combs)))

;; #16

(def sue-criteria
  {:children {:c 3 :f =}
   :cats {:c 7 :f >}
   :samoyeds {:c 2 :f =}
   :pomeranians {:c 3 :f <}
   :akitas {:c 0 :f =}
   :vizslas {:c 0 :f =}
   :goldfish {:c 5 :f <}
   :trees {:c 3 :f >}
   :cars {:c 2 :f =}
   :perfumes {:c 1 :f =}})

(def sue-archivo "resources/sues.txt")
(defn sues [archivo]
  (map (fn [line]
         (let [[_ id r] (re-matches #"^(.+?): (.+)$" line)
               possessions (string/split r #",\s+")]
           (println (str possessions))
           (assoc (reduce (fn [m p]
                            (let [[_ k n] (re-matches #"^(\w+): (\d+)$" p)]
                              (assoc m (keyword k) (Integer. n))))
                          {} possessions)
                  :id id)))
       (string/split (slurp archivo) #"\n")))

(defn has-criteria? [criteria questionable]
  (reduce (fn [res k]
            (and res
                 (or (nil? (get questionable k))
                     ((get-in criteria [k :f]) (k questionable) (get-in criteria [k :c])))))
          true (keys criteria)))

;; #17

(def eggnog-archivo "resources/eggnog.txt")
(defn eggnog-first-solution [archivo qty]
  (let [eggy (map-indexed (fn [i n] (hash-map i (Integer. n))) (string/split (slurp eggnog-archivo) #"\n"))]
    (count (filter (comp (partial = qty) (partial reduce +) (partial map (comp first vals))) (combo/subsets eggy)))))

(defn eggnog-second-solution [archivo qty]
  (let [eggy (map-indexed (fn [i n] (hash-map i (Integer. n))) (string/split (slurp eggnog-archivo) #"\n"))
        [p & _ :as huevos] (sort-by count (filter (comp (partial = qty) (partial reduce +) (partial map (comp first vals))) (combo/subsets eggy)))
        c (count p)]
    (count (take-while (comp (partial = c) count) huevos))))

;; #18

(def luzmatriz-archivo "resources/matriz_de_luz.txt")
(defn luzmatriz [archivo]
  (reduce (fn [mat line]
            (conj mat (mapv (partial = "#") (next (string/split line #"")))))
          [] (string/split (slurp archivo) #"\n")))
(def las-luces
  "Just for fun, I'm using an atom"
  (atom (luzmatriz luzmatriz-archivo)))
(def stuck-luces
  (atom (-> (assoc-in @las-luces [0 0] true)
            (assoc-in [0 99] true)
            (assoc-in [99 99] true)
            (assoc-in [99 0] true))))

(defn vecino-locs [size [y x]]
  (let [locs [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]]]
    (filterv (partial every? #(and ((complement neg?) %) (< % size))) (mapv #(mapv (partial +) % [y x]) locs))))
(defn vecinos [luces [y x]]
  (map (partial get-in luces) (vecino-locs (count luces) [y x])))
(defn d-state [vs state]
  (let [c (count (filter true? vs))]
    (if state
      (or (= 2 c) (= 3 c))
      (= 3 c))))

(defn next-luz-estado [stuck luces]
  (let [stuck-locs [[0 0] [0 (dec (count (first luces)))] [(dec (count luces)) (dec (count (first luces)))] [(dec (count luces)) 0]]]
    (reduce (fn [matriz y]
              (conj matriz
                    (reduce (fn [row x]
                              (conj row
                                    (if (and stuck (some #{[y x]} stuck-locs))
                                      true
                                      (d-state (vecinos luces [y x]) (get-in luces [y x])))))
                            [] (range (count (first luces))))))
            [] (range (count luces)))))

;; #19

(def rudolph-archivo "resources/sick-rudolph.txt")
(defn molecule-transforms [archivo]
  (vec
   (keep (fn [line]
           (when-let [[_ from to] (re-matches #"^(\w+)\s=>\s(\w+)" line)]
             (vector from to)))
         (string/split (slurp archivo) #"\n"))))

(defn divvy [el trel s]
  (loop [s s frontal "" res []]
    (let [[h e t] (drop 1 (first (re-seq (re-pattern (str "^(\\w*?)(" el ")(.*)")) s)))]
      ;; (println (str [h e t]))
      (if (nil? h)
        res
        (recur t (concat frontal h e) (conj res (apply str (concat frontal h trel t))))))))

(defn next-molecule-states [transforms molecules]
  (let [molecules (if (seq? molecules) molecules (list molecules))]
    ;; (println (str molecules))
    (mapcat (fn [m]
              (mapcat (fn [[el trel]]
                        (divvy el trel m))
                      transforms))
            molecules)))

(defn states-to-molecule [transforms goal molecule]
  (let [c (count goal)
        trans-map (reduce (fn [rts [t1 t2]] (assoc rts (apply str (reverse t2)) (apply str (reverse t1)))) {} transforms)
        re (re-pattern (string/join #"|" (keys trans-map)))]
    (loop [mole (apply str (reverse molecule)) pasos 0]
      (if-let [el (re-find re mole)]
        (recur (string/replace-first mole el (get trans-map el)) (inc pasos))
        pasos))))

;; #20

(defn divisors [n]
  (let [b (filter (comp zero? (partial mod n)) (range 1 (inc (Math/floor (Math/sqrt n)))))]
    (set (concat b (map (partial quot n) b)))))
(def gifts (comp (partial reduce #(+ %1 (* 10 %2)) 0) divisors))
(defn gifts2 [limit n]
  (let [trunc-divs (filter #(>= limit (quot n %)) (divisors n))]
    (reduce #(+ %1 (* 11 %2)) 0 trunc-divs)))

;; #21 & 22

(defrecord Player [name price hp damage armour]
  Object
  (toString [this]
    (str name " tiene " (:hp this) " puntas de pelea residual.\nSu cantidad de daño es "
         (:damage this) " y de blindaje " (:armour this) ".\n"
         "Pagó " price " de oro.")))

(def equipment
  {:weapons {:dagger {:price 8 :damage 4 :armour 0}
             :shortsword {:price 10 :damage 5 :armour 0}
             :warhammer {:price 25 :damage 6 :armour 0}
             :longsword {:price 40 :damage 7 :armour 0}
             :greataxe {:price 74 :damage 8 :armour 0}}
   :armour {:leather {:price 13 :damage 0 :armour 1}
            :chainmail {:price 31 :damage 0 :armour 2}
            :splintmail {:price 53 :damage 0 :armour 3}
            :bandedmail {:price 75 :damage 0 :armour 4}
            :platemail {:price 102 :damage 0 :armour 5}
            :nada {:price 0 :damage 0 :armour 0}}
   :rings {:d+1 {:price 25 :damage 1 :armour 0}
           :d+2 {:price 50 :damage 2 :armour 0}
           :d+3 {:price 100 :damage 3 :armour 0}
           :a+1 {:price 20 :damage 0 :armour 1}
           :a+2 {:price 40 :damage 0 :armour 2}
           :a+3 {:price 80 :damage 0 :armour 3}}})

(def combos-of-equipment (combo/cartesian-product (:weapons equipment) (:armour equipment)
                                                  (filter (comp (partial >= 2) count) (combo/subsets (seq (:rings equipment))))))

(defn eq->pl [name hp [[_ weapon] [_ armour] rings]]
  (let [price (+ (:price weapon) (:price armour) (reduce #(+ %1 (:price (second %2))) 0 rings))
        ring-damage (reduce #(+ %1 (:damage (second %2))) 0 rings)
        ring-armour (reduce #(+ %1 (:armour (second %2))) 0 rings)]
    (->Player name price hp (+ ring-damage (:damage weapon)) (+ ring-armour (:armour armour)))))

(defn rpg-fight [boss player]
  (let [{p-damage :damage p-armour :armour} player
        {b-damage :damage b-armour :armour} boss]
    (letfn [(dead-check [{p-hp :hp :as player}
                         {b-hp :hp :as boss}]
              (cond (<= p-hp 0) boss
                    (<= b-hp 0) player
                    :girder nil))
            (p-fight [player boss]
              #(if-let [winner (dead-check player boss)]
                 (do
                   ;; (println (str winner))
                   false)
                 (b-fight player (assoc boss :hp (let [damage (- p-damage b-armour)]
                                                   (- (:hp boss) (if (< damage 1) 1 damage)))))))
            (b-fight [player boss]
              #(if-let [winner (dead-check player boss)]
                 (do
                   ;; (println (str winner))
                   true)
                 (p-fight (assoc player :hp (let [damage (- b-damage p-armour)]
                                              (- (:hp player) (if (< damage 1) 1 damage))))
                          boss)))]
      (trampoline p-fight player boss))))

(defrecord Wizard [name mana max-hp hp spent-mana damage armour effects]
  Object
  (toString [this]
    (str name " tiene " (:hp this) " puntas de pelea residual.\n"
         "Tiene " (:mana this) " puntas de mana residual.\n" 
         "Su cantidad de daño es "
         (:damage this) " y de blindaje " (:armour this) ".\n"
         "Utilizó " spent-mana " de mana.")))
(defn wizard
  ([name & otro]
   (wizard (merge {:name name} (zipmap [:mana :max-hp :hp :spent-mana :damage :armour] otro))))
  ([{:keys [name mana max-hp hp spent-mana damage armour effects] :or {mana 500 max-hp 50 hp 50 spent-mana 0 damage 0 armour 0 effects {}}}]
   (->Wizard name mana max-hp hp spent-mana damage armour effects)))

(def spells
  {:magic-missle {:price 53 :effect {:d 1 :f #(update-in % [:damage] (partial + 4))
                                     :rf #(update-in % [:damage] (partial + -4))}}
   :drain {:price 73 :effect {:d 1 :f #(-> (update-in % [:damage] (partial + 2))
                                           (assoc :hp (if (< (- (:max-hp %) 2) (:hp %)) (:max-hp %) (+ (:hp %) 2))))
                              :rf #(update-in % [:damage] (partial + -2))}}
   :shield {:price 113 :effect {:d 6 :f #(update-in % [:armour] (partial + 7))
                                :rf #(update-in % [:armour (partial + -7)])}}
   :poison {:price 173 :effect {:d 6 :f #(update-in % [:damage] (partial + 3))
                                :rf #(update-in % [:damage] (partial + -3))}}
   :recharge {:price 229 :effect {:d 5 :f #(update-in % [:mana] (partial + 101))}}})

(defn mortify-effects [{:keys [effects] :as w}]
  (reduce #(update-in %1 [:effects %2 :d] dec)
          w (keys effects)))

(defn reset-attributes [p]
  (assoc p :damage 0 :armour 0))

(defn clean-effects [{:keys [effects] :as w}]
  (reduce (fn [w ef-kw]
            (if (zero? (get-in effects [ef-kw :d]))
              (-> w 
                  (assoc :effects (dissoc (:effects w) ef-kw)))
              w))
          w (keys effects)))

(defn new-effects [[spell-kw {:keys [effect]}] w]
  (when-not (get-in w [:effects spell-kw])
    (assoc-in w [:effects spell-kw] ((fnil identity {:d 0}) effect))))

(defn merge-effects [{:keys [effects] :as w}]
  (when w
    (reduce (fn [temp-w effect-key]
              ((:f (effect-key effects)) temp-w))
            w (keys effects))))

(defn cast-spell [[_ {:keys [price]} :as spell] w]
  (when-let [w ((comp merge-effects (partial new-effects spell) clean-effects reset-attributes) w)]
    (-> (update-in w [:mana] (partial + (- price)))
        (update-in [:spent-mana] (partial + price)))))

(defn out-of-mana? [{:keys [mana]}]
  (neg? mana))
(defn is-dead? [{:keys [hp]}]
  ((complement pos?) hp))

(defn attack [{:keys [boss wizard status]} spell hard]
  (letfn [(p-turn [boss wizard spell]
            (let [w (if hard (update-in wizard [:hp] (partial + -1)) wizard)]
              (if (or (out-of-mana? w) (is-dead? w))
                {:wizard w
                 :boss boss
                 :status {:turn nil :win false}}
                (if-let [{w-damage :damage :as w} (cast-spell spell w)]
                  (do
                    ;;(println (str (:name w) " attacking..." (first spell)))
                    {:wizard (mortify-effects w)
                     :boss (assoc boss :hp (let [damage (- w-damage 0)]
                                             (- (:hp boss) damage)))
                     :status {:turn :boss}})
                  ;; If a spell is cast that alread yexists in 'effects, nothing occurs.
                  (do
                    ;;(println (str "Spell not implemented - " (first spell)))
                    {:wizard wizard :boss boss :status {:turn :player}})))))
          (b-turn [boss w]
            (if (is-dead? boss)
              {:wizard w
               :boss boss
               :status {:turn nil :win true}}
              (let [w ((comp merge-effects clean-effects reset-attributes) (if hard (update-in w [:hp] (partial + -1)) w))
                    {w-damage :damage w-armour :armour :as w} w
                    {b-damage :damage :as boss} boss]
                ;;(println "boss attacking...")
                {:wizard (mortify-effects
                           (assoc w :hp (let [damage (- b-damage w-armour)]
                                          (- (:hp w) (if (< damage 1) 1 damage)))))
                 :boss (assoc boss :hp (- (:hp boss) w-damage))
                 :status {:turn :player}})))]
    (case (:turn status)
      :player (p-turn boss wizard (seq spell))
      :boss (b-turn boss wizard)
      status)))

(defn toros [hard boss wizard spell-coll]
  (reduce (fn [{:keys [status] :as estado} spell]
            (if (some? (get status :win))
              (reduced estado)
              (attack estado spell hard)))
          {:wizard wizard :boss boss :status {:turn :player}}
          spell-coll))

;; #23

(def instr-archivo "resources/instructions.txt")
(def mem23 (atom {}))
(defn reset-mem23! [& code] (reset! mem23 {:code code :ptr 0 :a 0 :b 0}))

(defn jump [offset]
  (println (str "jumping ... " offset))
  (swap! mem23 update-in [:ptr] (partial + offset)))

(defn inc-ptr []
  (swap! mem23 update-in [:ptr] inc))

(def instructions
  {:hlf (fn [r] (inc-ptr) (swap! mem23 update-in [r] #(quot % 2)))
   :tpl (fn [r] (inc-ptr) (swap! mem23 update-in [r] #(* % 3)))
   :inc (fn [r] (inc-ptr) (swap! mem23 update-in [r] inc))
   :jmp (fn [offset] (jump offset))
   :jie (fn [r offset] (if (even? (get @mem23 r))
                         (jump offset)
                         (inc-ptr)))
   :jio (fn [r offset] (if (= 1 (get @mem23 r))
                         (jump offset)
                         (inc-ptr)))})

(defn execute []
  (let [[instr args] (nth (:code @mem23) (:ptr @mem23))]
    (when-let [f (get instructions instr)]
      (println (str "instr: " instr "  args: " args))
      (apply f args))))

(defn docode []
  (let [end (count (:code @mem23))]
    (loop [continue true]
      (println (dissoc @mem23 :code))
      (if-not (and continue (< (:ptr @mem23) end)) :finis
        (recur (execute))))))

(def instr-args-xform
  (map #(cond (re-find #"^[ab]$" %) (keyword %) 
              (re-find #"^[0-9+-]+" %) (Integer. %)
              :pylorus %)))

(def mem23-xform-de-arch
  (keep (fn [line]
          (when-let [[_ instr args] (re-matches #"^(\w+)\s(.+)$" line)]
            (vector (keyword instr) (into [] instr-args-xform (string/split args #", ")))))))

(defn load-code [code]
  (swap! mem23 assoc :code (into [] mem23-xform-de-arch code)))

(defn load-archivo [archivo]
  (load-code (string/split-lines (slurp archivo))))

(defn day23 []
  (reset-mem23!)
  (load-archivo instr-archivo)
  (docode)
  (dissoc @mem23 :code))

(defn day23-p2 []
  (reset-mem23!)
  (load-archivo instr-archivo)
  (swap! mem23 assoc :a 1)
  (docode)
  (dissoc @mem23 :code))

;; # 24

(def quantum-archivo "resources/quantum_packages.txt")
(def pesos-xform (map #(Integer. %)))
(def pesos-de-paquetes (into [] pesos-xform (string/split-lines (slurp quantum-archivo))))


