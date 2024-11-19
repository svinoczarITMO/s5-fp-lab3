(ns lab3.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [lab3.core :refer [linear-interpolation lagrange-polynomial format-output lagrange-interpolation parse-line sort-points]]))

(deftest test-linear-interpolation
  (testing "Linear interpolation between two points"
    (let [points [[2.0 2.0] [4.0 4.0]]
          step 1.0
          expected [[2.0 2.0] [3.0 3.0] [4.0 4.0]]]
      (is (= expected (vec (linear-interpolation points step)))))

    (let [points [[0.0 0.0] [5.0 10.0]]
          step 5.0
          expected [[0.0 0.0] [5.0 10.0]]]
      (is (= expected (vec (linear-interpolation points step)))))))

(deftest test-lagrange-polynomial
  (let [points [[0 0] [1 1] [2 4] [3 9]]]
    (is (= (lagrange-polynomial points 1) 1))
    (is (= (lagrange-polynomial points 2) 4))
    (is (= (lagrange-polynomial points 0) 0))
    (is (= (lagrange-polynomial points 3) 9))
    (is (= (lagrange-polynomial [[-1 1] [0 0] [1 1]] -1) 1N))))

(deftest test-format-output
  (let [x-values [2.0 4.0 6.0]
        y-values [1.0 3.0 5.0]
        expected "X: 2.00\t4.00\t6.00\nY: 1.00\t3.00\t5.00\n"]
    (is (= (format-output x-values y-values) expected))))

(deftest test-lagrange-interpolation
  (let [points [[2 1] [4 3] [6 5]]
        step 0.5
        start-x 2
        end-x 6
        expected [[2.0 1.0]
                  [2.5 1.5]
                  [3.0 2.0]
                  [3.5 2.5]
                  [4.0 3.0]
                  [4.5 3.5]
                  [5.0 4.0]
                  [5.5 4.5]
                  [6.0 5.0]]]
    (is (= (map #(map double %) (lagrange-interpolation points step start-x end-x)) expected))

    (let [points [[1 1] [2 4] [3 9] [4 16]]
          step 1.0
          start-x 1
          end-x 4
          expected [[1.0 1.0]
                    [2.0 4.0]
                    [3.0 9.0]
                    [4.0 16.0]]]
      (is (= (map #(map double %) (lagrange-interpolation points step start-x end-x)) expected)))))

(deftest test-parse-line
  (testing "Parsing a line with coordinates"
    (is (= [2.0 3.0] (parse-line "2 3")))
    (is (= [6.0 7.0] (parse-line "6;7")))
    (is (= [8.0 9.0] (parse-line "8\t9")))
    (is (= [0.0 1.0] (parse-line "0.0 1.0")))
    (is (= [0.1 0.2] (parse-line "0.1 0.2")))
    (is (= [-1.0 1.0] (parse-line "-1.0 1.0")))))

(deftest test-sort-points
  (testing "Sorting points by x-coordinate"
    (let [points [[3 4] [1 2] [5 0] [4 3]]
          expected [[1 2] [3 4] [4 3] [5 0]]]
      (is (= expected (sort-points points))))))

(run-tests)
