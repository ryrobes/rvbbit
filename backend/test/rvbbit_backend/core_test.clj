
(ns rvbbit-backend.core-test
  (:require
   [clojure.test        :refer [deftest is testing]]
   [rvbbit-backend.rabbit-script :refer :all]
   [rvbbit-backend.core :as core]))


(def test-data2
  [{:id 1 :sales 100 :cost 80 :category "A" :tax 0.2 :discount 0.1}
   {:id 2 :sales 120 :cost 85 :category "A" :tax 0.2 :discount 0.15}
   {:id 3 :sales 90  :cost 70 :category "A" :tax 0.2 :discount 0.05}
   {:id 4 :sales 200 :cost 150 :category "B" :tax 0.15 :discount 0.2}
   {:id 5 :sales 180 :cost 140 :category "B" :tax 0.15 :discount 0.1}
   {:id 6 :sales 100 :cost 80 :category "A" :tax 0.2 :discount 0.1}
   {:id 7 :sales 120 :cost 85 :category "A" :tax 0.2 :discount 0.15}
   {:id 8 :sales 90  :cost 70 :category "A" :tax 0.2 :discount 0.05}
   {:id 9 :sales 200 :cost 150 :category "B" :tax 0.15 :discount 0.2}
   {:id 10 :sales 180 :cost 140 :category "B" :tax 0.15 :discount 0.1}])

(deftest simple-references
  (testing "Simple column reference"
    (let [results (evaluate-formula "=sales" test-data2)
          rres (mapv :result results)]
      (is (= [100 120 90 200 180 100 120 90 200 180]
             rres)))))

(testing "Basic arithmetic"
  (let [results (evaluate-formula "=sales * 2" test-data2)
        rres (mapv :result results)]
    (is (= [200.0 240.0 180.0 400.0 360.0 200.0 240.0 180.0 400.0 360.0]
           (mapv :result results)))))

(testing "Field references"
  (let [results1 (evaluate-formula "=sales.2" test-data2)
        results2 (evaluate-formula "=sales.2 * sales.4" test-data2)
        rres1 (mapv :result results1)
        rres2 (mapv :result results2)]
    (is (= [90 90 90 90 90 90 90 90 90 90]
           rres1))
    (is (= [16200 16200 16200 16200 16200 16200 16200 16200 16200 16200]
           rres2))))

(deftest aggregation-tests
  (testing "Entire column aggregation"
    (let [results (evaluate-formula "=SUM(sales)" test-data2)
          rres (mapv :result results)]
      (is (= [1380 1380 1380 1380 1380 1380 1380 1380 1380 1380]
             rres))))

    (testing "Entire column aggregation 2"
    (let [results (evaluate-formula "=COUNTD(sales)" test-data2)
          rres (mapv :result results)]
      (is (= [5 5 5 5 5 5 5 5 5 5]
             rres))))

  (testing "Range aggregation"
    (let [results (evaluate-formula "=SUM(sales[1:3])" test-data2)
          rres (mapv :result results)]
      (is (= [410 410 410 410 410 410 410 410 410 410]
             rres)))))

(deftest conditional-tests
  (testing "Simple IF"
    (let [results (evaluate-formula "=IF(sales >= 150, cost * 1.1, cost * 2.9)" test-data2)
          rres (mapv :result results)]
      (is (= [232.0 246.5 203.0 165.0 154.0 232.0 246.5 203.0 165.0 154.0]
             rres))))

  (testing "Complex IF with aggregation"
    (let [results (evaluate-formula "=IF(sales <= AVERAGE(sales), cost * 1.1, cost * 0.9)" test-data2)
          rres (mapv :result results)]
      (is (= [88.0 93.50000000000001 77.0 135.0 126.0 88.0 93.50000000000001 77.0 135.0 126.0]
             rres)))))

(deftest nested-function-tests
  (testing "Nested functions"
    (let [results (evaluate-formula "=IF(AVERAGE(sales) > SUM(cost)/5, sales * 1.1, sales * 0.9)" test-data2)
          rres (mapv :result results)]
      (is (= [90.0 108.0 81.0 180.0 162.0 90.0 108.0 81.0 180.0 162.0]
             rres))))

  (testing "Multiple conditions"
    (let [results (evaluate-formula "=IF(sales > AVERAGE(sales), IF(cost < AVERAGE(cost), sales * 1.2, sales * 1.1), sales * 0.9)" test-data2)
          rres (mapv :result results)]
      (is (= [90.0 108.0 81.0 220.00000000000003 198.00000000000003 90.0 108.0 81.0 220.00000000000003 198.00000000000003]
             rres)))))

(deftest range-reference-tests
  (testing "Range references with functions"
    (let [results (evaluate-formula "=IF(sales > AVERAGE(sales[1:3]), cost * 1.1, cost * 0.9)" test-data2)
          rres (mapv :result results)]
      (is (= [72.0 76.5 63.0 165.0 154.0 72.0 76.5 63.0 165.0 154.0]
             rres))))

  (testing "Multiple ranges"
    (let [results (evaluate-formula "=IF(SUM(sales[1:2]) > SUM(sales[3:4]), cost * 3.1, cost * 1.9)" test-data2)
          rres (mapv :result results)]
      (is (= [152.0 161.5 133.0 285.0 266.0 152.0 161.5 133.0 285.0 266.0]
             rres)))))

(deftest complex-arithmetic-tests
  (testing "Multiple aggregations"
    (let [results (evaluate-formula "=(SUM(sales) + AVERAGE(cost)) / 2" test-data2)
          rres (mapv :result results)]
      (is (= [742.5 742.5 742.5 742.5 742.5 742.5 742.5 742.5 742.5 742.5]
             rres))))) (testing "Complex arithmetic"
                         (let [results (evaluate-formula "=(sales * (1 + tax)) - (cost * (1 - discount))" test-data2)
                               rres (mapv :result results)]
                           (is (= [48.0 71.75 41.5 109.99999999999997 80.99999999999997 48.0 71.75 41.5 109.99999999999997 80.99999999999997]
                                  rres))))

(deftest window-function-tests
  (testing "OFFSET function"
    (let [results (evaluate-formula "=sales / OFFSET(sales, -1)" test-data2)
          rres (mapv :result results)]
      (is (= [nil 1.2 0.75 2.222222222222222 0.9 0.5555555555555556 1.2 0.75 2.222222222222222 0.9]
             rres)))))

;; (testing "FILTER with CURRENT"
;;   (let [results (evaluate-formula "=SUM(FILTER(sales, category = CURRENT(category)))" test-data2)
;;         rres (mapv :result results)]
;;     (is (= [620 620 620 760 760 620 620 620 760 760]
;;            rres))))

(deftest bool-tests
  (testing "boolean literals mixed - literal returns"
    (let [results (evaluate-formula "=IF(AND(category = 'B', cost > 30), true, false)" test-data2)
          rres (mapv :result results)]
      (is (= [false false false true true false false false true true]
             rres))))
  (testing "boolean literals - literal conditions"
    (let [results (evaluate-formula "=IF(false, 213, 23)" test-data2)
          rres (mapv :result results)]
      (is (= [23.0 23.0 23.0 23.0 23.0 23.0 23.0 23.0 23.0 23.0]
             rres)))))

(deftest stringy-tests
  (testing "CONCAT function"
    (let [results (evaluate-formula "=CONCAT(\"$\", cost, \" for \", category )" test-data2)
          rres (mapv :result results)]
      (is (= ["$80 for A" "$85 for A" "$70 for A" "$150 for B" "$140 for B" "$80 for A" "$85 for A" "$70 for A" "$150 for B" "$140 for B"]
             rres)))))

(testing "FILTER with CURRENT"
  (let [results (evaluate-formula "=SUM(FILTER(sales, category = CURRENT(category)))" test-data2)
        rres (mapv :result results)]
    (is (= [620 620 620 760 760 620 620 620 760 760]
           rres))))

(deftest nested-range-tests
  (testing "Nested ranges with conditional logic"
    (let [results (evaluate-formula "=SUM(IF(sales > 100, cost[1:2], cost[3:4]))" test-data2)
          rres (mapv :result results)]
      (is (= [290 155 290 155 155 290 155 290 155 155]
             rres))))

  (testing "Average with nested ranges"
    (let [results (evaluate-formula "=AVERAGE(IF(sales > 100, cost[1:2], cost[3:4]))" test-data2)
          rres (mapv :result results)]
      (is (= [145 155/2 145 155/2 155/2 145 155/2 145 155/2 155/2]
             rres))))

  (testing "Conditional averages with ranges"
    (let [results (evaluate-formula "=IF(sales > 100, AVERAGE(cost[1:2]), AVERAGE(cost[3:4]))" test-data2)
          rres (mapv :result results)]
      (is (= [145 155/2 145 155/2 155/2 145 155/2 145 155/2 155/2]
             rres)))))

;; (deftest special-function-tests-IFS
;;   (testing "IFS function"
;;     ;; Returns first true condition's result
;;     (let [results (evaluate-formula "=IFS(sales > 150, \"High\", sales > 100, \"Medium\", TRUE, \"Low\")" test-data2)]
;;       (is (= ["Low" "Medium" "Low" "High" "High" "Low" "Medium" "Low" "High" "High"]
;;              (mapv :result results))))))

  ;; (deftest special-function-tests-SWITCH
  ;;   (testing "SWITCH function"
  ;;   ;; Like a CASE statement
  ;;   (let [results (evaluate-formula "=SWITCH(category, \"A\", sales * 1.1, \"B\", sales * 0.9, sales)" test-data2)]
  ;;     (is (= [110.0 132.0 99.0 180.0 162.0 110.0 132.0 99.0 180.0 162.0]
  ;;            (mapv :result results))))))

  ;; (deftest special-function-tests-CHOOSE
  ;;   (testing "CHOOSE function"
  ;;   ;; Select from options based on index
  ;;   (let [results (evaluate-formula "=CHOOSE(IF(sales > 150, 1, 2), sales * 1.1, sales * 0.9)" test-data2)]
  ;;     (is (= [90.0 108.0 81.0 220.0 198.0 90.0 108.0 81.0 220.0 198.0]
  ;;            (mapv :result results))))))

  ;; (deftest special-function-tests-MAP
  ;;   (testing "MAP function"
  ;;   ;; Apply calculation to each element
  ;;   (let [results (evaluate-formula "=MAP(sales, sales * tax)" test-data2)]
  ;;     (is (= [20.0 24.0 18.0 30.0 27.0 20.0 24.0 18.0 30.0 27.0]
  ;;            (mapv :result results))))))

  (deftest special-function-tests-REDUCE
    (testing "REDUCE function"
    ;; Accumulate values
    (let [results (evaluate-formula "=REDUCE(sales, 0, accumulator + current_value * (1 - discount))" test-data2)]
      (is (= [1199.0 1199.0 1199.0 1199.0 1199.0 1199.0 1199.0 1199.0 1199.0 1199.0]
             (mapv :result results))))))

  ;; (deftest special-function-tests-LAMBDA
  ;;   (testing "LAMBDA function"
  ;;   ;; Define and use inline function
  ;;   (let [results (evaluate-formula "=MAP(sales, LAMBDA(x, x * (1 + tax)))" test-data2)]
  ;;     (is (= [120.0 144.0 108.0 230.0 207.0 120.0 144.0 108.0 230.0 207.0]
  ;;            (mapv :result results))))))

  ;; (deftest special-function-tests-LET
  ;;   (testing "LET function"
  ;;   ;; Use local variables
  ;;   (let [results (evaluate-formula "=LET(tax_rate, 0.2, base_cost, cost * 1.1, base_cost * (1 + tax_rate))" test-data2)]
  ;;     (is (= [105.6 112.2 92.4 198.0 184.8 105.6 112.2 92.4 198.0 184.8]
  ;;            (mapv :result results))))))

