(ns looping-is-recursion)

(defn power [base exp]
  (if (= 0 exp) 1
    (let [helper (fn [base exp acc]
                   (if (= exp 1) acc
                      (recur base (dec exp) (* base acc))))]
      (helper base exp base))))

(defn last-element [a-seq]
  (if (empty? a-seq) nil
    (let [helper (fn [s]
                   (if (empty? (rest s)) (first s)
                     (recur (rest s))))]
    (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond (and (empty? s1) (empty? s2)) true
                       (or (empty? s1) (empty? s2)) false
                       :else (if (= (first s1) (first s2))
                         (recur (rest s1) (rest s2))
                         false)))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         n 0]
    (cond (empty? s) nil
          (pred (first s)) n
          :else (recur (rest s) (inc n)))))

(defn avg [a-seq]
  (if (empty? a-seq) 0
    (loop [acc (first a-seq)
           s (rest a-seq)
           n 1]
      (if (empty? s) (/ acc n)
        (recur (+ acc (first s)) (rest s) (inc n))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         s a-seq]
    (if (empty? s) a-set
      (recur (toggle a-set (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [i n
         a 0
         b 1]
    (if (= i 0) a (recur (- i 1) (+ a b) a))))


(defn cut-at-repetition [a-seq]
  (loop [checked-set #{(first a-seq)}
         s (rest a-seq)
         v [(first a-seq)]]
    (let [fst (first s)]
      (cond (empty? s) v
            (contains? checked-set fst) v
            :else (recur (conj checked-set fst) (rest s) (conj v fst))))))

