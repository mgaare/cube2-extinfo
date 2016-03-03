(ns cube2-extinfo.deserialise
  (:require [clojure.string :as str])
  (:refer-clojure :exclude (flush))
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
                (fn [b'#]
                  (let [carry'# (conj carry# b'#)]
                    (if (= ~length (count carry'#))
                      (return-value
                       (.. (ByteBuffer/wrap (byte-array carry'#))
                           (order ~bo)
                           (~getter)))
                      (return-next (df# carry'#))))))]
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
       -128 (return-next short-le-d)
       -127 (return-next int-le-d)
       (return-value (int b))))
    ([]
     nil)))

(defn hmap
  "Deserialises bytes into a map. Takes keyvals where the vals are deserialisers.

   eg. \"(map-deserialiser :a (cstring-d) :b (int-d))\""
  [& d-kvs]
  (assert (even? (count d-kvs))
          "an even number of key deserialiser pairs is required")
  (let [ds (partition 2 d-kvs)
        add-v (fn [v]
                (fn [cur] (cond (nil? cur)
                                v
                                (vector? cur)
                                (conj cur v)
                                :else (conj [cur] v))))
        [first-k first-d] (first ds)]
    (letfn [(df [cur-k cur-d rest-ds carry]
              (fn
                ([b]
                 (let [[v d'] (deserialise cur-d b)
                       carry' (when v (update carry cur-k (add-v v)))]
                   (cond (and v d')
                         (return-next
                          (df cur-k d' rest-ds carry'))
                         d'
                         (return-next
                          (df cur-k d' rest-ds carry))
                         v
                         (let [[next-k next-d] (first rest-ds)]
                           (if (seq rest-ds)
                             (return-next (df next-k next-d (rest rest-ds) carry'))
                             (return-value carry')))
                         :else (return-value 'carry))))
                ([]
                 (if-let [flush-v (flush cur-d)]
                   (update carry cur-k (add-v flush-v))
                   carry))))]
      (df first-k first-d (rest ds) {}))))

(defn sequential
  "Returns a deserialiser which will deserialise using deserialisers
   ds in sequence."
  [& ds]
  (letfn [(df [d ds]
            (fn
              ([b]
               (let [[v d'] (deserialise d b)]
                 (cond (and v d')
                       [v (df d' ds)]
                       d'
                       (return-next (df d' ds))
                       v
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
                       v
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
               (let [[v d'] (d b)]

                 )
               )
              )
            )])
  )

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
                   (if v
                     (if-let [vd (get d-map v)]
                       (return-next vd)
                       [nil nil])
                     (return-next (df 'd)))))
                ([]
                 (flush d))))]
      (df alt-d))))

(defn byte-seq
  "Takes a deserialiser and seq of bytes, and returns lazy seq of
   deserialised objects."
  [d bs]
  (lazy-seq
   (if (seq bs)
     (let [[v d'] (deserialise d (first bs))]
       (if v
         (cons v (when d' (byte-seq d' (rest bs))))
         (when d' (byte-seq d' (rest bs)))))
     (when-let [flush-v (flush d)]
       (list flush-v)))))
