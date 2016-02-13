(ns cube2-extinfo.master
   "Functions that deal with interacting with the master server."
   (:require [clojure.string :as str]
             [aleph.tcp :as tcp]
             [gloss.core :as gloss]
             [gloss.io :as io]
             [manifold.deferred :as d]
             [manifold.stream :as s]))


;; Master server commands

;; Master server is on sauerbraten.org port 28787

;; 'list' command returns newline-separated list of servers, in form of:
;;   `addserver <ip> <port>'

(def default-master "sauerbraten.org:28787")

(def protocol
  (gloss/compile-frame
   (gloss/string :ascii :delimiters ["\n" "\r\n"])))

(defn wrap-duplex-stream
  [prot s]
  (let [out (s/stream)]
    (s/connect
     (s/map #(io/encode prot %) out)
     s)
    (s/splice
     out
     (io/decode-stream s prot))))

(defn client
  [host port]
  (d/chain (tcp/client {:host host :port port})
           #(wrap-duplex-stream protocol %)))

(defn parse-server
  [server-str]
  (let [[_ server port] (str/split server-str #"\s")]
    {:server server
     :port (Long/parseLong port)}))

(defn server-list
  ([master]
   (let [[host port] (str/split master #":")]
     (server-list host (Long/parseLong port))))
  ([host port]
   @(d/let-flow [s (client host port)]
      (s/put! s "list")
      (->> (s/stream->seq s)
           (map parse-server)))))
