(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         i 0]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (rest s) (inc i)))))

(defn avg [a-seq]
  (loop [total 0
         items 0
         s a-seq]
    (if (empty? s)
      (/ total items)
      (recur (+ total (first s)) (inc items) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [par-set #{}
         s a-seq]
    (if (empty? s)
      par-set
      (recur (toggle par-set (first s)) (rest s)))))
    

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [fn 1
           fn-1 0
           num 1]
      (if (= num n)
        fn
        (recur (+ fn fn-1) fn (inc num))))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         seen #{}
         s a-seq]
    (if (or (empty? s) (contains? seen (first s)))
      acc
      (recur (conj acc (first s)) (conj seen (first s)) (rest s)))))

