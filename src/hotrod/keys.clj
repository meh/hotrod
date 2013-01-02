(ns hotrod.keys
  (:require [hotrod.utils :refer [case-insensitive]]))

(defrecord Key
  [name ctrl shift alt win through?])

(defn ^:private first-char
  ([string] (first-char string 0))
  ([string position] (when (< position (count string))
                       [(nth string position) [string position]])))

(defn ^:private next-char
  [[_ [string position]]]
  (when (< position (count string))
    (let [position (inc position)]
      [(nth string position) [string position]])))

(defn ^:private expand-key [name]
  (case-insensitive name
    "Space"             :space
    ("Enter" "Return")  :enter
    ("Esc" "Escape")    :esc
    ("Backspace" "BS")  :backspace
    ("Delete" "Del")    :delete
    ("Insert" "Ins")    :insert
    "Home"              :home
    "End"               :end
    ("PageUp" "PgUp")   :page-up
    ("PageDown" "PgDn") :page-down
    "Up"                :up
    "Down"              :down
    "Left"              :left
    "Right"             :right
    "ScrollLock"        :scroll-lock
    "CapsLock"          :caps-lock
    "NumLock"           :num-lock

    (throw (IllegalArgumentException. (str name " is an unknown key")))))

(defn ^:private parse-key [string position]
  (loop [current (first-char string position), result (map->Key {}), side true]
    (when current
      (let [[ch [_ position]] current]
        (case ch
          \< (recur (next-char current) result :left)
          \> (recur (next-char current) result :right)
          \~ (recur (next-char current) (assoc result :through? true) true)

          \^ (recur (next-char current) (assoc result :ctrl side) true)
          \+ (recur (next-char current) (assoc result :shift side) true)
          \! (recur (next-char current) (assoc result :alt side) true)
          \# (recur (next-char current) (assoc result :win side) true)

          \{ (loop [current (next-char current), name ""]
               (when current
                 (let [[ch [_ position]] current]
                   (if (= ch \})
                     [(assoc result :name (expand-key name)) (inc position)]
                     (recur (next-char current) (str name ch))))))

          [(assoc result :name (keyword (str ch))) (inc position)])))))

(defn parse-keys [string]
  (loop [current (parse-key string 0), result []]
    (if-not current result
      (let [[key position] current]
        (recur (parse-key string position) (conj result key))))))
