(ns hotrod.utils)

(defn ^:private maybe-map [f x]
  (if (seq? x) (map f x) (f x)))

(defn ^:private lowercase-forms [clauses]
  (map (fn [test? x] (if test? (maybe-map (fn [x] (.toLowerCase x)) x) x))
            (cycle [true false]) clauses))

(defmacro case-insensitive [test & clauses]
  `(case (.toLowerCase ~test)
     ~@(if (even? (count clauses))
         (lowercase-forms clauses)
         (concat (lowercase-forms (butlast clauses))
                 [(last clauses)]))))
