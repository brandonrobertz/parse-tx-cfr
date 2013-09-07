(ns parse-cfr.core-test
  (:require [clojure.test :refer :all]
            [parse-tx-cfr.core :refer :all]))

;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-config
  "This is a placeholder config. For use with data/test.pdf page 10"
  []
  {:recs-per-pg 5
   :config-page 10
   :cfg [["Full name of contributor"    "Byron, Bruce"]
         ["Contributor address"         "5801 Tom Wooten Drive "]
         ["Contributor address"         "Ste. 300"]
         ["Contributor address"         "Houston, TX 77056"]
         ["Amount of contribution ($)"  "$350.00"]
         ["Principal occupation / Job title (See Instructions)"
          "McQueary Henry Bowles Troy"]
         ["Employer (See Instructions)" "Portfolio Accountant"]]})

(def pg-10
  (list ["\n Byron, Bruce\n"      "\n 5801 Tom Wooten Drive\n"
         "\n Austin, TX 78731\n"  "\n"
         "\n $100.00 |\n"         "\n" "\n"]
        ["\n Carmichael, Keith\n" "\n 521 Brandon Way\n"
         "\n Austin, TX 78733\n"  "\n"
         "\n $300.00 |\n"         "\n McQueary Henry Bowles Troy\n"
         "\n President, Austin Region\n"]
        ["\n Carnes, Melanie\n"   "\n 1115West10thStreet,Apt200\n"
         "\n Austin, TX 78703\n"  "\n"
         "\n $350.00 |\n"         "\n Prophet Capital Asset Management\n"
         "\n Portfolio Accountant\n"]
        ["\n CDMPAC (Camp Dresser McKee PAC)\n" "\n 3050 Post Oak Blvd.\n"
         "\n Ste. 300\n"          "\n Houston, TX 77056\n"
         "\n $350.00 |\n"         "\n" "\n"]
        ["\n Chan, Grace\n"       "\n 1605 Churchwood Cove\n"
         "\n Austin, TX 78746\n"  "\n"
         "\n $250.00 |\n"         "\n None\n" "\n Homemaker\n"]))

(defn delta10
  []
  (list {:txt "Full name of contributor"
         :dy 10.44000244140625
         :dx 117.720001}
        {:txt "Contributor address"
         :dy 7.91998291015625
         :dx 117.720001}
        {:txt "Contributor address"
         :dy 15.839996337890625
         :dx 118.440002}
        {:txt "Contributor address"
         :dy 23.400009155273438
         :dx 118.440002}
        {:txt "Amount of contribution ($)"
         :dy 36.3599853515625
         :dx 408.959991}
        {:txt "Principal occupation / Job title (See Instructions)",
         :dy 9.3599853515625,
         :dx 321.477844}
        {:txt "Employer (See Instructions)"
         :dy 9.3599853515625
         :dx 62.639999}))

(defn make-delta []
  (batch-deltas (test-config)
                (get-pg stream (:config-page (test-config)))))

(defn test-ex-pg [n]
  (scrape-page (get-pg stream n) (test-config) (make-delta)))

;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest dist-test
  (testing "Make sure that our distance fn penalizes y distance harder
            than x. We want to make sure that our code is favoring a
            shorter distance y over x."
    (is (> (dist [100 200] [50 220]) (dist [100 200] [100 210])))))

(deftest test-matching
  (testing "Make sure that our matcher does fuzzy matching correctly."
    (is (= "Amount of      |            In-kind contribution"
           (re-find #"Amount of.*contribution"
                    (:txt (first (find-by-str-fuzzy
                                  (get-pg stream 10)
                                  "Am0unt 0f\tcontr1but10n" 1))))))))

(deftest test-delta-x
  (testing "Make sure the delta x values are correct."
    (is (= 1265.3978424072266 (reduce + (sort (map :dx (make-delta))))))))

(deftest test-delta-y
  (testing "Make sure the delta y values are correct."
    (is (= 0.0 (reduce + (map - (sort (map :dy (make-delta)))
                                (sort (map :dy (delta10)))))))))

(deftest example-pg-test
  (testing "Make sure that pg 10 (starting at 0) of our test
            pdf (data/test.pdf) parses correctly."
    (is (= (test-ex-pg 10) pg-10))))
