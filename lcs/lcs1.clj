;;; Longest Common Subsequence
;;; Solutions in Clojure


;; *** Matrix operations ***********************************

;; Creates an integer n x m matrix
(defn make-matrix [n m]
  (make-array Integer/TYPE n m))

(defn matrix-get [mat i j]
  (aget mat i j))

(defn matrix-set [mat i j val]
  (aset mat i j val))



;; *** LCS *************************************************

(defn lcs [s1 s2]
  "")

(defn lcs-span [s1 s2 n m]
  (if (= (nth s1 n) (nth s2 m))
    ""
    ""))

(defn lcs-cost-rec [s1 s2 n m]
  (if (or (< n 0) (< m 0))
    0
    (if (= (nth s1 n) (nth s2 m))
      (inc (lcs-cost-rec s1 s2 (dec n) (dec m)))
      (max (lcs-cost-rec s1 s2 (dec n) m)
           (lcs-cost-rec s1 s2 n (dec m))))))

