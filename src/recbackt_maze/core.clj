(ns recbackt-maze.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [clojure.set]))

(def size 30)
(def unit 15)

(defn bounds-check [[x y]]
  (if (and (>= x 0) (< x size) (>= y 0) (< y size)) [x y]))

(defn adjacent [cell dir]
  (let [[x y] cell]
    (bounds-check (cond (= dir 'west) [(- x 1) y]
                        (= dir 'east) [(+ x 1) y]
                        (= dir 'north) [x (- y 1)]
                        (= dir 'south) [x (+ y 1)]))))

(defn cells-adjacent-to [cell]
  (filter #(not (nil? %)) (map #(adjacent cell %) '(north south east west))))

(defn point [cell]
  (vec (map #(* % unit) cell)))

(defn eastwall-pp [cell]
  (let [n (adjacent cell 'east)]
    (if n (let [[x y] n]
            (list (point n) (point [x (inc y)]))))))

(defn westwall-pp [cell]
  (let [n (adjacent cell 'west)]
    (if n (let [[x y] cell]
            (list (point cell) (point [x (inc y)]))))))

(defn northwall-pp [cell]
  (let [n (adjacent cell 'north)]
    (if n (let [[x y] cell]
            (list (point cell) (point [(inc x) y]))))))

(defn southwall-pp [cell]
  (let [n (adjacent cell 'south)]
    (if n (let [[x y] n]
            (list (point n) (point [(inc x) y]))))))

(defn init-walls []
  (let [cells (for [x (range size) y (range size)] [x y])]
    (set (filter #(not (nil? %))
                 (concat (map eastwall-pp cells) (map southwall-pp cells))))))

(defn next-cell [state]
  (let [{:keys [cell visited]} state
        cells (difference (set (cells-adjacent-to cell)) visited)]
    (if (empty? cells) nil (rand-nth (seq cells)))))

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

(defn draw-track [l]
  (if (not (empty? l))
    (doall (map #(let [[x y] %] (q/rect (* x unit) (* y unit) unit unit)) l))))

(defn backtrack [state]
  (let [{:keys [track cell]} state]
    (if (empty? track) state
        (assoc state :cell (first track) :track (rest track) ))))

(defn setup []
  (q/frame-rate 30)
  {:visited #{[0 0]}
   :cell [0 0]
   :track '([0 0])
   :walls (init-walls)})

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
  (q/rect 0 0 (* size unit) (* size unit)))

(q/defsketch recbackt-maze
  :title "Recursive Backtracking Maze Generator"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
