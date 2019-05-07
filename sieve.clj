(ns p1.sieve)
(defn update-comps->facs
  "Returns an updated map of composites to a list of their known factors."
  [n factor comps->facs]
  (update-in comps->facs [(+ n factor)] conj factor))

(defn gen-primes
  "A generator which produces a lazy, infinite sequence of prime numbers via
  a sieve method."
  ([]
   (gen-primes 2 {}))
  ([n comps->facs]
   (if-let [factors (get comps->facs n)]
     (recur (inc n)
            (reduce
              #(update-comps->facs n %2 %1) (dissoc comps->facs n) factors))
     (lazy-seq
       (cons n  ;; N is prime because it's not in the composites.
             (gen-primes (inc n) (assoc comps->facs (* n n) (list n))))))))

(def lazy-primes (gen-primes))
