(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev e] (cons e rev)) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce
      (fn [[lo hi] e] [(min lo e) (max hi e)])
      [##Inf ##-Inf]
      a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n '())
    (loop [acc '() xs sorted-seq]
      (cond
        (empty? xs) (concat acc [n])
        (< n (first xs)) (concat acc [n] xs)
        :else (recur (concat acc [(first xs)]) (rest xs))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set e]
  ((if (contains? a-set e) disj conj) a-set e))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and [& preds]
  (fn [e]
    (loop [ps preds]
      (cond (empty? ps) true
            (not ((first ps) e)) false
            :else (recur (rest ps))))))

(defn my-map [f & seqs]
  (if (empty? seqs)
    '()
    (loop [acc '() xss seqs]
      (if (some empty? xss)
        acc
        (recur
          (concat acc [(apply f (for [xs xss] (first xs)))])
          (for [xs xss] (rest xs)))))))
