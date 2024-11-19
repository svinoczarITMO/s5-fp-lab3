(ns lab3.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-s" "--step STEP" "Шаг интерполяции" :parse-fn #(Double/parseDouble %)]
   ["-a" "--algorithm ALGORITHM" "Алгоритм интерполяции (linear, lagrange, both)"
    :validate [#{"linear" "lagrange" "both"} "Алгоритм должен принимать одно из следующих значений: 'linear', 'lagrange' или 'both'"]]
   ["-h" "--help"]])

(defn parse-line [line]
  (let [[x y] (str/split line #"[;\t\s]+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn linear-interpolation [points step]
  (let [[p1 p2] points
        [x1 y1] p1
        [x2 y2] p2
        x-values (range x1 (+ step x2) step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             [(double x) (+ y1 (* t (- y2 y1)))]))
         x-values)))

(defn lagrange-basis-polynomial [points x i]
  (let [[xi _] (nth points i)]
    (reduce (fn [li j]
              (if (not= i j)
                (let [[xj _] (nth points j)]
                  (if (not= xi xj)
                    (* li (/ (- x xj) (- xi xj)))
                    li))
                li))
            1
            (range (count points)))))

(defn lagrange-polynomial [points x]
  (reduce (fn [acc i]
            (let [[_ yi] (nth points i)]
              (+ acc (* (lagrange-basis-polynomial points x i) yi))))
          0
          (range (count points))))

(defn lagrange-interpolation [points step start-x end-x]
  (let [x-values (range start-x (+ end-x step) step)]
    (map (fn [x] [x (lagrange-polynomial points x)]) x-values)))

(defn format-output [x-values y-values]
  (let [x-str (str/join "\t" (map #(format "%.2f" %) x-values))
        y-str (str/join "\t" (map #(format "%.2f" %) y-values))]
    (str "X: " x-str "\nY: " y-str "\n")))

;; =============== OUTPUT ===============
(defn sort-points [points]
  (if (not (apply <= (map first points)))
    (do
      (println "Данные не отсортированы по X. Производится сортировка...")
      (sort-by first points))
    points))

(defn process-linear-interpolation [points step]
  (when (>= (count points) 2)
    (let [window-points (take-last 2 points)
          interp (linear-interpolation window-points step)
          x-values (map first interp)
          y-values (map second interp)]
      (println (format "Линейная интерполяция (X от %.3f до %.3f):"
                       (first x-values) (last x-values)))
      (println (format-output x-values y-values)))))

(defn process-lagrange-interpolation [points step window-size]
  (let [lagrange-windows (partition window-size 1 points)]
    (doseq [window lagrange-windows]
      (let [start-x (first (map first window))
            end-x (last (map first window))
            interp (lagrange-interpolation window step start-x end-x)
            x-values (map first interp)
            y-values (map second interp)]
        (println (format "Интерполяция полиномом Лагранжа (X от %.3f до %.3f):"
                         start-x end-x))
        (println (format-output x-values y-values))))))

(defn read-point [prompt]
  (println prompt)
  (let [line (read-line)]
    (if (or (nil? line) (= (str/trim line) "exit"))
      (do
        (println "Завершение работы.")
        (System/exit 0))
      (if (empty? line)
        (do
          (println "Пустая строка. Попробуйте снова.")
          (recur prompt))
        (parse-line line)))))

(defn process-input [step algorithm]
  (let [window-size 5]
    (loop [points [] n 1]
      (let [p (read-point (format "Задайте точку P%d в формате: X Y" n))]
        (if (= (str/trim (str p)) "exit")
          (do
            (println "Завершение работы.")
            (System/exit 0))
          (let [points (conj points p)]
            (when (or (= algorithm "linear") (= algorithm "both"))
              (process-linear-interpolation (sort-points points) step))
            (if (= algorithm "both")
              (when (>= (count points) window-size)
                (process-lagrange-interpolation (drop (- (count points) window-size) (sort-points points)) step window-size))
              nil)
            (recur points (inc n))))))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)]
    (if (:help options)
      (println "Использование:
lein run <...>
-s --step —— размер шага интерполяции
-a --algorithm —— алгоритм интерполяции (linear, lagrange, both)
-h --help —— вывод информации о команде")
      (if errors
        (do
          (doseq [error errors]
            (println error))
          (System/exit 1))
        (let [step (or (:step options) 1.0)
              algorithm (or (:algorithm options) "both")]
          (process-input step algorithm))))))
