(ns cube2-extinfo.master
   "Functions that deal with interacting with the master server."
   (:require [clojure.java.io :as io]
             [clojure.string :as str])
   (:import java.net.Socket))

;; Master server commands

;; Master server is on sauerbraten.org port 28787

;; 'list' command returns newline-separated list of servers, in form of:
;;   `addserver <ip> <port>'

(def default-master "sauerbraten.org:28787")

(defn socket
  [host port]
  (Socket. host port))

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
   (with-open [socket (socket host port)
               r (io/reader socket)
               w (io/writer socket)]
     (.write w "list")
     (.newLine w)
     (.flush w)
     (->> (line-seq r)
          (doall)
          (drop-last)
          (map parse-server)))))
