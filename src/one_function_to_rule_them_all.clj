(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (reduce (fn [a b] (str a " " b))
          a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc el] (conj acc x el))
            []
            a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc el] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[n1 n2] el]
            [(min n1 el)
             (max n2 el)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq)
            (insert (rest sorted-seq)
                    n)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params
  [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (my-* x y) more)))


(defn pred-and
  ([] (fn [elem] true))
  ([f] (fn [elem] (and (f elem) true)))
  ([f g] (fn [elem] (and (f elem) (g elem))))
  ([f g & more]
   (reduce pred-and (pred-and f g) more)))

(defn my-map
  ([f s] (reduce (fn [e1 e2] (conj e1 (f e2))) [] s))
  ([f s1 s2] (reduce (partial f) [] (my-map f s1))))

(defn my-map
  ([f coll] (seq (reduce (fn [e1 e2] (conj e1 (f e2))) [] coll)))
  ([f coll & colls]
    (let [colls (cons coll colls)]
      (my-map (partial apply f)
              (partition (count colls)
                         (apply interleave colls))))))
