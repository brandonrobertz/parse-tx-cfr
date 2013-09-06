(ns parse-tx-cfr.similarity
  (:require [clojure.set :refer [intersection]]))

(defn strip-chrs
  "Strip the unwanted characters from the string we're going to
   make n-shingles from. Also, casting to lower case."
  [^String s]
  (filter #(re-find #"[a-z0-9\$]" (str %)) (.toLowerCase s)))

(defn two-shingles
  "Take a string and separate it into a set of 2-shingles."
  [s]
  (into #{} (let [s (strip-chrs s)]
              (for [x (range (count s))
                    :when (< (inc x) (count s))]
                (apply str [(nth s x) (nth s (inc x))])))))

(defn similarity
  "Returns the Dice similarity of two sets, as defined by two times
   the count of the intersection of the sets divided by the sum of
   the count of  both sets."
  [s1 s2]
  (let [sh1 (two-shingles s1)
        sh2 (two-shingles s2)]
    (/ (* 2.0 (count (intersection sh1 sh2)))
       (+ (count sh1) (count sh2)))))
