(ns recbackt-maze.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [clojure.set]))

(def maze-size 15)
(def unit 30)
(def cells-set (set (for [r (range maze-size) c (range maze-size)] [r c])))

(defn neighbour [cell dir]
  (let [[x y] cell
        n (cond (= dir 'west) [(- x 1) y]
                (= dir 'east) [(+ x 1) y]
                (= dir 'north) [x (- y 1)]
                (= dir 'south) [x (+ y 1)])]
    (if (contains? cells-set n) n)))

(defn neighbours [cell]
  (let [n (map #(neighbour cell %) '(north south east west))]
    (filter #(not (nil? %)) n)))

(defn point [cell]
  (vec (map #(* % unit) cell)))

(defn eastwall-pp [cell]
  (let [n (neighbour cell 'east)]
    (if n (let [[x y] n
                y' (inc y)]
            (list (point n) (point [x y']))))))

(defn westwall-pp [cell]
  (let [n (neighbour cell 'west)]
    (if n (let [[x y] cell
                y' (inc y)]
            (list (point cell) (point [x y']))))))

(defn northwall-pp [cell]
  (let [n (neighbour cell 'north)]
    (if n (let [[x y] cell
                x' (inc x)]
            (list (point cell) (point [x' y]))))))

(defn southwall-pp [cell]
  (let [n (neighbour cell 'south)]
    (if n (let [[x y] n
                x' (inc x)]
            (list (point n) (point [x' y]))))))

(defn eastwalls []
  (filter #(not (nil? %)) (map eastwall-pp cells-set)))

(defn southwalls []
  (filter #(not (nil? %)) (map southwall-pp cells-set)))

(defn walls []
  (set (concat (eastwalls) (southwalls))))

(defn next-dir [cell next]
  (let [[cx cy] cell
        [nx ny] next]
    (cond (> nx cx) 'east
          (< nx cx) 'west
          (> ny cy) 'south
          (< ny cy) 'north)))

(defn wall-to-remove [cell dir]
  (cond (= dir 'west) (westwall-pp cell)
        (= dir 'east) (eastwall-pp cell)
        (= dir 'north) (northwall-pp cell)
        (= dir 'south) (southwall-pp cell)))

(defn setup []
  (q/frame-rate 10)
  {:visited #{[0 0]}
   :cell [0 0]
   :track '([0 0])
   :walls (walls)})

(defn next-cell [state]
  (let [{:keys [cell visited]} state
        nbs (difference (set (neighbours cell)) visited)]
    (if (empty? nbs) nil
        (rand-nth (seq nbs)))))

(defn draw-track [l]
  (if (not (empty? l))
    (doall (map #(let [[x y] %] (q/rect (* x unit) (* y unit) unit unit)) l))))

(defn backtrack [state]
  (let [{:keys [track cell]} state]
    (if (empty? track) state
        (assoc state :cell (first track) :track (rest track) ))))

(defn update-state [state]
  (let [next (next-cell state)
        {:keys [visited cell track walls]} state]
    (if (nil? next) (backtrack state)
        (let [dir (next-dir cell next)]
          {:visited (conj visited next)
           :track (conj track next)
           :walls (disj walls (wall-to-remove cell dir))
           :cell next}))))

(defn draw-state [state]
  (q/background 240) 
  (q/translate 25 25)
  (q/fill 220 200 255)
  (q/no-stroke)
  (draw-track (:track state))
  (q/stroke 0)
  (doall (map #(apply q/line %) (:walls state)))
  (q/no-fill)
  (q/rect 0 0 (* maze-size unit) (* maze-size unit)))

(q/defsketch recbackt-maze
  :title "Recursive Backtracking Maze Generator"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])