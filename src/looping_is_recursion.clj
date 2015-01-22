(ns looping-is-recursion)

(defn power [base exp]  (let [helper (fn [n result] (if (zero? n)
                                                      result
                                                      (recur (dec n) (* result base))))]
                          (helper exp 1)))

(defn last-element [a-seq] (if (or (= 1 (count a-seq)) (= 0 (count a-seq)))
                             (first a-seq)
                             (recur (rest a-seq))))

(defn seq= [seq1 seq2] (if (or (empty? seq1) (empty? seq2))
                         (if (and (empty? seq1) (empty? seq2))
                           true
                           false)
                         (if (= (first seq1) (first seq2))
                           (recur (rest seq1) (rest seq2))
                           false)))

(defn find-first-index [pred a-seq] (loop [n (count a-seq) s a-seq]
                                      (if (empty? s)
                                        nil
                                        (if (pred (first s))
                                          (- (count a-seq) n)
                                          (recur (dec n) (rest s))))))

(defn avg [a-seq] (loop [s a-seq sum 0]
                    (if (empty? s)
                      (/ sum (count a-seq))
                      (recur (rest s) (+ sum (first s))))))

(defn parity [a-seq] (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
                       (loop [gathered #{} s a-seq]
                         (if (empty? s)
                           gathered
                           (recur (toggle gathered (first s)) (rest s))))))

(defn fast-fibo [n] (loop [n_current 0 n_minus 0 n_minus_2 0]
                      (if (= n n_current)
                        (if (or (= 1 n) (= 0 n))
                          n
                          (+ n_minus n_minus_2))
                        (if (or (= n_current 0) (= n_current 1))
                          (if (= n_current 0)
                            (recur (+ n_current 1) 0 0)
                            (recur (+ n_current 1) 1 0))
                          (recur (+ n_current 1) (+ n_minus n_minus_2) n_minus)))))

(defn cut-at-repetition [a-seq] (loop [gathered [] left a-seq]
                                  (if (or (empty? left) (contains? (set gathered) (first left)))
                                    gathered
                                    (recur (conj gathered (first left)) (rest left)))))

