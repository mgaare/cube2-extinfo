(ns cube2-extinfo.deserialise
  (:require [clojure.string :as str])
  (:refer-clojure :exclude (flush repeat))
  (:import [java.nio ByteBuffer ByteOrder]))

;; General idea here:

;; a deserialiser is a function:
;; 0-arg, called when out of bytes; can optionally flush state
;; 1-arg, takes a byte and returns [value next-deserialiser]

;; value and next-deserialiser can both be nil.

;; let's put a protocol around this

;; we can also add middlewares that have the same signature

;; to get a deserialiser, call one of the constructor functions with
;; constructor-specific initialization values

(defprotocol Deserialiser
  (deserialise [this b] "Returns [value next-deserialiser]")
  (flush [this] "Flushes any state and returns value."))

(extend-protocol Deserialiser
  clojure.lang.Fn
  (deserialise [f b]
    (f b))
  (flush [f]
    (f)))

(defn return-value
  "Helper function to return a value with no next."
  [v]
  [v nil])

(defn return-next
  "Helper function to return the next deserialiser function."
  [next-d]
  [nil next-d])

(defn return-nothing
  "Helper function to return an empty value and next deserialiser."
  []
  [nil nil])

(defn failure?
  "True if passed a deserialiation failure."
  [[v d]]
  (and (nil? v) (nil? d)))

;; 3 states
;; - Needs to call n-d until a numeric value is returned to find the
;; number of repeats (will handle this with RepeaterD)
;; - Has n repeats of d
;; - Repeat d forever

(defrecord Repeater
    [d n cur-d]
  Deserialiser
  (deserialise [this b]
    (let [[v next-d] (deserialise cur-d b)]
      (if next-d
        [v (assoc this :cur-d next-d)]
        (if (some-> n dec zero?)
          (return-value v)
          [v (cond-> (assoc this :cur-d d)
               n (update :n dec))]))))
  (flush [this]
    (flush cur-d)))

(defn repeater
  "Repeats deserialiser d n times, or forever if no n passed."
  ([d]
   (repeater nil d))
  ([n d]
   (->Repeater d n d)))

(defrecord RepeaterD
    [d nd cur-nd]
  Deserialiser
  (deserialise [this b]
    (let [[v next-d] (deserialise cur-nd b)]
      (cond (some? v)
            (if (integer? v)
              (return-next (repeater v d))
              (throw (ex-info (str "value returned by repeat-d must be integer, got " v)
                              {:repeater-d this})))
            next-d
            (return-next (assoc this :cur-nd next-d))
            :else
            (return-nothing))))
  (flush [this]
    (flush cur-nd)))

(defn repeater-d
  "Uses deserialiser n-d to determine how many times to repeat
   deserialiser d. n-d must return an integer."
  [n-d d]
  (->RepeaterD d n-d n-d))

(defn repeat
  "Repeats deserialiser d. If n is a deserialiser, calls it with byte
   inputs to determine the number of repeats. If n is numeric, repeat
   n times. If no n passed, repeat forever."
  ([d]
   (repeater d))
  ([n d]
   (cond (satisfies? Deserialiser n)
         (repeater-d n d)
         (or (nil? n) (integer? n))
         (repeater n d)
         :else (ex-info (str "Invalid n arg " n)
                        {:d d
                         :n n}))))

;; list of deserialisers to try, in order. If deserialiser returns nil
;; next-d before returning a value, tries the next deserialiser.

(defn- catch-up-backtrack
  [bs d]
  (reduce (fn [d b]
            (let [[v d'] (deserialise d b)]
              (cond (some? v)
                    (reduced (return-value v))
                    (not d')
                    (reduced (return-nothing))
                    :else d')))
          d bs))

(defn- to-d
  [[v d]]
  (if (some? v)
    (reify Deserialiser
      (deserialise [_ _] v)
      (flush [_] v))
    d))

(defrecord BacktrackingAlts
    [ds cur-d consumed]
  Deserialiser
  (deserialise [this b]
    (let [[v d'] (deserialise cur-d b)]
      (cond (and d' (some? v))
            [v d']

            d'
            (return-next
             (-> this
                 (assoc :cur-d d')
                 (update :consumed conj b)))

            (some? v)
            (return-value v)

            ;; Here's the complicated case - the current deserialiser
            ;; has failed. The following section (lazily) applies the
            ;; consumed bytes to all the deserialisers, filtering out
            ;; failures, and moves on to the next one.
            :else
            (if (seq ds)
              (let [[[v next-d] & rem-ds]
                    (->> ds
                         (map (partial catch-up-backtrack consumed))
                         (remove failure?))]
                (cond (some? v)
                      (return-value v)
                      next-d
                      (return-next
                       (assoc this
                              :cur-d next-d
                              :ds (map to-d rem-ds)
                              :consumed []))
                      :else (return-nothing)))
              (return-nothing)))))
  (flush [this]
    (flush cur-d)))

(defn backtracking-alts
  "Takes one or more deserialisers as args. Returns a deserialiser
   that will attempt to deserialise with the first deserialiser until
   it fails or returns a value. If it fails, it will re-try the next
   deserialiser with the same bytes sent to the first, an so forth
   until one of the deserialisers returns a value or all of them
   fail."
  [& ds]
  (->BacktrackingAlts (rest ds) (first ds) []))

(def byte-order
  {:le `ByteOrder/LITTLE_ENDIAN
   :be `ByteOrder/BIG_ENDIAN
   :ne `(ByteOrder/nativeOrder)})

(defmacro fixed-length-byte-deserialiser
  "Returns a byte parser taking length bytes, retrieving a value from
   a length-sized byte-array with getter, transforming the value with
   xform, reading with specified byte order."
  [length getter xform bo]
  (assert (pos? length) "length must be positive.")
  (if (= 1 length)
    `(fn []
       (fn
         ([b#]
          (return-value (~xform b#)))
         ([]
          nil)))
    (let [bo (get byte-order bo)]
      `(letfn [(df# [carry#]
                 (fn
                   ([b'#]
                    (let [carry'# (conj carry# b'#)]
                      (if (= ~length (count carry'#))
                        (return-value
                         (.. (ByteBuffer/wrap (byte-array carry'#))
                             (order ~bo)
                             (~getter)))
                        (return-next (df# carry'#)))))
                   ([]
                    nil)))]
         (fn [] (df# []))))))

(def long-d
  (fixed-length-byte-deserialiser 8 getLong identity :be))

(def long-le-d
  (fixed-length-byte-deserialiser 8 getlong identity :le))

(def int-d
  (fixed-length-byte-deserialiser 4 getInt identity :be))

(def int-le-d
  (fixed-length-byte-deserialiser 4 getInt identity :le))

(def short-d
  (fixed-length-byte-deserialiser 2 getShort identity :be))

(def short-le-d
  (fixed-length-byte-deserialiser 2 getShort identity :le))

(def byte-d
  (fixed-length-byte-deserialiser 1 get identity :be))

(def unsigned-byte-d
  (fixed-length-byte-deserialiser 1 get (partial bit-and 0xff) :be))

(defn cstring
  "Deserialises null-terminated strings."
  []
  (letfn [(df [carry]
            (fn
              ([b]
               (if (zero? b)
                 (return-value (str/join carry))
                 (return-next (df (conj carry (char b))))))
              ([]
               carry)))]
    (df [])))

(defn cube-compressed-int
  "Deserialiser for cube compressed ints."
  []
  (fn
    ([b]
     (case b
       -128 (return-next (short-le-d))
       -127 (return-next (int-le-d))
       (return-value (int b))))
    ([]
     nil)))

(defn- update-val
  [current-v new-v]
  (cond (nil? current-v)   new-v
        (coll? current-v)  (conj current-v new-v)
        :else              (conj [current-v] new-v)))

(defrecord HMap
    [k d ds m]
  Deserialiser
  (deserialise [this b]
    (let [[v d' :as result] (deserialise d b)
          m' (cond-> m
               (some? v) (update k update-val v))]
      (cond (failure? result)
            (return-nothing)
            d'
            (return-next (assoc this :d d' :m m'))
            :else
            (if (seq ds)
              (let [[k d] (first ds)]
                (return-next (assoc this :k k :d d :m m' :ds (rest ds))))
              (return-value m')))))
  (flush [this]
    (if-let [flush-v (flush d)]
      (update m k update-val flush-v)
      m)))

(defn hmap
  "Deserialises bytes into a map. Takes keyvals where the vals are deserialisers.

   eg. \"(map-deserialiser :a (cstring) :b (int-d))\""
  [& d-kvs]
  (assert (even? (count d-kvs))
          "an even number of key deserialiser pairs is required")
  (let [ds (partition 2 d-kvs)
        [k d] (first ds)]
    (->HMap k d (rest ds) {})))

(defn sequential
  "Returns a deserialiser which will deserialise using deserialisers
   ds in sequence."
  [& ds]
  (letfn [(df [d ds]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)]
                 (cond (and (some? v) d')
                       [v (df d' ds)]
                       d'
                       (return-next (df d' ds))
                       (some? v)
                       (if-let [next-d (first ds)]
                         [v (df next-d (rest ds))]
                         [v nil])
                       :else [nil nil])))
              ([]
               (flush d))))]
    (df (first ds) (rest ds))))

(defn merge-hmaps
  [& hmaps]
  (letfn [(df [collect d]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)]
                 (cond (and v d')
                       (return-next (df (conj collect v) d'))
                       d'
                       (return-next (df collect d'))
                       (some? v)
                       (return-value (apply merge (conj collect v)))
                       :else
                       (return-value (apply merge collect)))))
              ([] (return-value (apply merge collect)))))]
    (df [] (apply sequential hmaps))))

(defn wrap-enum
  "Takes an deserialiser and a function (including a map). Calls the
   deserialiser until it returns a value, returns the result of
   calling value-f on the deserialised value."
  [d value-f]
  (letfn [(df [d]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)]
                 (if (some? v)
                   (return-value (value-f v))
                   (return-next d'))))
              ([]
               (flush d))))]
    (df d)))

(defn wrap-enum-value
  "Takes a deserializer and a function (including a map). Calls the
   deserialiser until it returns a value, which is returned along with
   a next-d that is the result of calling value-f on the deserialised
   value."
  [d value-f]
  (letfn [(df [d]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)]
                 (if (some? v)
                   [v (value-f v)]
                   (return-next d'))))
              ([]
               (flush d))))]
    (df d)))

(defn wrap-xform
  "Takes a deserialiser and an xform function. Calls the deserialiser
   until it stops returning a next-d, collecting the values and
   passing all the collected values (in a collection) to xform."
  [d xform]
  (letfn [(df [collect d]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)
                     collect (cond-> collect
                                (some? v) (conj v))]
                 (if d'
                   (return-next (df collect d'))
                   (return-value (xform collect)))))
              ([]
               (xform collect))))]
    (df [] d)))

(defrecord WrapRequired
    [d required]
  Deserialiser
  (deserialise [this b]
    (let [[v d'] (deserialise d b)]
      (cond (some? v)
            (if (= v required)
              (return-value v)
              (return-nothing))
            d'
            (return-next (assoc this :d d'))
            :else (return-nothing))))
  (flush [this]
    (flush d)))

(defn wrap-required
  "Takes a deserialiser and a required value. Calls deserialiser until
   it returns a value, and fails out if the returned value doesn't
   match the required value."
  [d required]
  (->WrapRequired d required))

(defn alternative
  "Takes a deserialiser and keyvals in the form of value, deserialiser.
   Returns a deserialiser that calls the alt-d until it returns a
   value, and then returns the deserialiser corresponding with that
   value. The value is not returned."
  [alt-d & value-ds]
  (assert (even? (count value-ds))
          "an even number of value deserialiser pairs is required")
  (let [d-map (apply hash-map value-ds)]
    (letfn [(df [d]
              (fn
                ([b]
                 (let [[v d'] (deserialise d b)]
                   (if (some? v)
                     (if-let [vd (get d-map v)]
                       (return-next vd)
                       [nil nil])
                     (return-next (df 'd)))))
                ([]
                 (flush d))))]
      (df alt-d))))

(defn passthru-alternative
  "Like alternative, except that it passes the bytes including the
   ones sent to alt-d to the value deserialiser.

   Note if the value deserialiser returns a value from fewer bytes
   than alt-d consumed, the remaining bytes that alt-d consumed won't
   be sent to the value deserialiser."
  [alt-d & value-ds]
  (assert (even? (count value-ds))
          "an even number of value deserialiser pairs is required")
  (let [d-map (apply hash-map value-ds)]
    (letfn [(df [bs d]
              (fn
                ([b]
                 (let [bs' (conj bs b)
                       [v d'] (deserialise d b)]
                   (if (some? v)
                     (if-let [vd (get d-map v)]
                       (let [d-old-bs
                             (reduce (fn [d b]
                                       (let [[v d'] (deserialise d b)]
                                         (cond (and v d')
                                               (reduced [v d'])
                                               (some? v)
                                               (reduced (return-value v))
                                               d' d'
                                               :else (reduced (return-nothing)))))
                                     vd bs')]
                         (if (reduced? d-old-bs)
                           d-old-bs
                           (return-next d-old-bs)))
                       [v nil])
                     (return-next (df bs' d')))))))]
      (df [] alt-d))))

(defn byte-seq
  "Takes a deserialiser and seq of bytes, and returns lazy seq of
   deserialised objects."
  [d bs]
  (lazy-seq
   (if (seq bs)
     (let [[v d'] (deserialise d (first bs))]
       (if (some? v)
         (cons v (when d' (byte-seq d' (rest bs))))
         (when d' (byte-seq d' (rest bs)))))
     (when-let [flush-v (flush d)]
       (list flush-v)))))
