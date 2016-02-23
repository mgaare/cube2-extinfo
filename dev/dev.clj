(ns dev
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.tools.namespace.repl :refer :all]))


;; current work

(require '[aleph.udp :as udp]
         '[gloss.core :as gloss]
         '[gloss.io :as gio]
         '[manifold.stream :as ms]
         '[gloss.core.protocols :as p]
         '[gloss.data.primitives :as prim]
         '[gloss.data.bytes :as gbytes])
(import [java.nio
         ByteBuffer
         ByteOrder])

(def mode-flags
  {:m-team (bit-shift-left 1 0)
   :m-noitems (bit-shift-left 1 1)
   :m-noammo (bit-shift-left 1 2)
   :m-insta (bit-shift-left 1 3)
   :m-efficiency (bit-shift-left 1 4)
   :m-tactics (bit-shift-left 1 5)
   :m-capture (bit-shift-left 1 6)
   :m-regen (bit-shift-left 1 7)
   :m-ctf (bit-shift-left 1 8)
   :m-protect (bit-shift-left 1 9)
   :m-hold (bit-shift-left 1 10)
   :m-overtime (bit-shift-left 1 10)



   }
  )

(def demophobia {:host "144.76.176.131" :port 28786})

(def pisto {:host "pisto.horse" :port 1025})

;; things we could use
;; to-byte (gloss.data.primitives)
;; with-byte-order (gloss.data.primitives)
;; with-buffer (gloss.core.protocols)
;; byte-count (gloss.data.bytes)
;; drop-bytes (gloss.data.bytes)
;; take-bytes (gloss.data.bytes)
;; take-contiguous-bytes (gloss.data.bytes)
;; rewind-bytes (gloss.data.bytes)


(def cube-compressed-int
  (reify
    p/Reader
    (read-bytes [_ b]
      (let [[success v buf :as res] (p/read-bytes (:byte prim/primitive-codecs) b)]
        (if success
          (case v
            -128 (p/read-bytes (:int16-le prim/primitive-codecs) buf)
            -127 (p/read-bytes (:int32-le prim/primitive-codecs) buf)
            res)
          res)))
    p/Writer
    (sizeof [_]
      nil)
    (write-bytes [_ buf v]
      (cond
        (> 128 v -127)
        (p/write-bytes (:byte prim/primitive-codecs) buf v)
        (> 32768 v -32768)
        (p/with-buffer [buf 3]
          (.put ^ByteBuffer buf (prim/to-byte -128))
          (prim/with-byte-order [buf :le]
            (.putShort ^ByteBuffer buf (short v))))
        :else
        (p/with-buffer [buf 5]
          (.put ^ByteBuffer buf (prim/to-byte -127))
          (prim/with-byte-order [buf :le]
            (.putInt ^ByteBuffer buf (int v))))))))

(gloss/defcodec flag (gloss/enum :byte {false 0 true 1}))

(gloss/defcodec gamemode
  (gloss/enum :byte
              {:ffa 0
               :coop-edit 1
               :teamplay 2
               :instagib 3
               :instagib-team 4
               :efficiency 5
               :efficiency-team 6
               :tactics 7
               :tactics-team 8
               :capture 9
               :regen-capture 10}))

(gloss/defcodec mastermode
  (gloss/enum :byte
              {:mm-auth -1
               :mm-open 0
               :mm-veto 1
               :mm-locked 2
               :mm-private 3
               :mm-password 4}))

(def base-si-attrs
  [:protocol-version cube-compressed-int
   :gamemode gamemode
   :remaining-time cube-compressed-int
   :maxclients cube-compressed-int
   :mastermode mastermode])

(gloss/defcodec si-attrs
  (apply gloss/ordered-map base-si-attrs))

(gloss/defcodec si-extended-attrs
  (apply gloss/ordered-map (concat base-si-attrs
                                   [:gamepaused flag
                                    :gamespeed cube-compressed-int])))

(gloss/defcodec server-info
  (gloss/ordered-map
   :command-prefix 1 ;; handling this in the header dispatch
   :numclients cube-compressed-int
   :attrs (gloss/header :byte {5 si-attrs 7 si-extended-attrs} count)
   :mapname (gloss/string :iso-8859-1 :delimiters [0])
   :description (gloss/string :iso-8859-1 :delimiters [0])))

(gloss/defcodec ext-type
  (gloss/enum :byte {:uptime 0 :playerstats 1 :teamscore 2
                     :playerstats-resp-ids -10 :playerstats-resp-stats -11}))

(gloss/defcodec ext-status (gloss/enum :byte {:ext-ack -1 :ext-no-error 0 :ext-error 1}))

(gloss/defcodec ext-uptime
  (gloss/ordered-map
   :ext-type :uptime
   :ext-status ext-status
   :ext-version :byte
   :uptime cube-compressed-int))

(gloss/defcodec ext-playerstats
  (gloss/ordered-map
   :ext-type :playerstats
   :cn :byte
   :ext-status ext-status
   :ext-version :byte
   :error flag))

(gloss/defcodec ext-playerstats-ids
  (gloss/ordered-map
   :ext-type :playerstats-resp-ids))

(gloss/defcodec ext-playerstats-cn
  (gloss/ordered-map
   :cn cube-compressed-int))

(gloss/defcodec ext-playerstats-stats
  (gloss/ordered-map
   :ext-type :playerstats-resp-stats
   :pid :byte
   :ping cube-compressed-int
   :name (gloss/string :iso-8859-1 :delimiters [0])
   :team (gloss/string :iso-8859-1 :delimiters [0])
   :frags cube-compressed-int
   :flags cube-compressed-int
   :deaths cube-compressed-int
   :teamkills cube-compressed-int
   :accuracy cube-compressed-int
   :health cube-compressed-int
   :armour cube-compressed-int
   :gunselect cube-compressed-int
   :privilege cube-compressed-int
   :state cube-compressed-int
   :ip (gloss/repeated :byte)))

(gloss/defcodec ext-team
  (gloss/ordered-map
   :team (gloss/string :iso-8859-1 :delimiters [0])
   :score cube-compressed-int))

(gloss/defcodec ext-teamscore
  (gloss/ordered-map
   :ext-type :teamscore
   :ext-status ext-status
   :ext-version cube-compressed-int
   :error flag
   :game-mode cube-compressed-int
   :remaining-time cube-compressed-int))

(gloss/defcodec ext-response
  (gloss/header ext-type
                {:uptime ext-uptime
                 :playerstats ext-playerstats
                 :teamscore ext-teamscore
                 :playerstats-resp-ids ext-playerstats-ids
                 :playerstats-resp-stats ext-playerstats-stats}
                :ext-type))

(def command-prefixes
  {0 0
   1 1
   :serverinfo 1
   :extinfo 0})

(gloss/defcodec info-response
  (gloss/header cube-compressed-int
                #(if (zero? %)
                   server-info
                   ext-response)
                #(or (get command-prefixes (:command-prefix %))
                     1)))

(def commands (gloss/enum :byte {:extinfo 0}))

(def ext-commands (gloss/enum :byte {:uptime 0 :playerstats 1 :teamscore 2}))

(def codec (gloss/compile-frame
            [commands ext-commands]))



;; things I'd like to have

;; - laziness
;; - stateful logic
;; - use sequences
;; - easier debugging
;; - don't really care about bidirectional

;; transducers seem pretty good actually... hrm

(defn parse-byte
  []
  (fn [b]
    (long b)))

(defmacro fixed-length-byte-parser
  "Returns a byte parser taking length bytes, retrieving a value from
   a length-sized byte-array with getter, and transforming the value
   with optional xform."
  [length getter & [xform]]
  (assert (pos? length) "length must be positive.")
  (let [xform (or xform identity)
        declength (dec length)]
    (if (= 1 length)
      `(fn []
         (let [hold# (volatile! nil)]
           (fn [b#]
             (if-let [prev# @hold#]
               (do
                 (vreset! hold# nil)
                 (~xform (~getter (ByteBuffer/wrap (byte-array [prev# b#])))))
               (do (vreset! hold# b#)
                   nil)))))
      `(fn []
         (let [hold# (volatile! [])]
           (fn [b#]
             (if (= (count @hold#) ~declength)
               (let [ba# (conj @hold# b#)]
                 (vreset! hold# [])
                 (~xform (~getter (ByteBuffer/wrap (byte-array ba#)))))
               (do (vswap! hold# conj b#)
                   nil))))))))

(def parse-short
  (fixed-length-byte-parser 2 .getShort identity))

;; need to handle byte order (for sauer)

(defn byte-xf
  "Takes a byte transformer function, and returns a transducer."
  [f]
  (fn [xf]
    (let [completed? (volatile! false)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if @completed?
           (xf result input)
           (if-let [r (f input)]
             (do
               (vreset! completed? true)
               (conj result r))
             result)))))))



(defn echoing-xf
  "Tranducer tester"
  [xf]
  (fn
    ([] (xf))
    ([result] (do (println "just result: " result)
                  (xf result)))
    ([result input] (do (println "result: " result " and input: " input)
                        (xf result input)))))

(def test-ducer
  (comp (map inc)
        echoing-xf))



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
    `(fn [b#]
       [(~xform (byte b#))
        nil])
    (let [bo (get byte-order bo)]
      `(letfn [(df# [carry#]
                (fn [b'#]
                  (let [carry'# (conj carry# b'#)]
                    (if (= ~length (count carry'#))
                      [(.. (ByteBuffer/wrap (byte-array carry'#))
                           (order ~bo)
                           (~getter))
                       nil]
                      [nil
                       (df# carry'#)]))))]
         (df# [])))))

(def deserialise-long
  (fixed-length-byte-deserialiser 8 getLong identity :be))

(def deserialise-long-le
  (fixed-length-byte-deserialiser 8 getlong identity :le))

(def deserialise-int
  (fixed-length-byte-deserialiser 4 getInt identity :be))

(def deserialise-int-le
  (fixed-length-byte-deserialiser 4 getInt identity :le))

(defn functional-byte-deserialiser
  "Takes a collection of deserialisers, and returns a function that,
   when passed a byte array or collection of bytes, will return a lazy
   seq of deserialised objects."
  [ds]
  (letfn [(deserialise [ds d bs]
            (lazy-seq
             (when (and (seq bs) d)
               (let [[d-obj d'] (d (first bs))]
                 (if d-obj
                   (cons d-obj (deserialise (rest ds) (first ds) (rest bs)))
                   (deserialise ds d' (rest bs)))))))]
    (partial deserialise (rest ds) (first ds))))
