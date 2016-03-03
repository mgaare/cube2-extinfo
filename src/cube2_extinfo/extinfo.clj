(ns cube2-extinfo.extinfo
  (:require [cube2-extinfo.deserialise :as d]))

(def modes [:ffa
            :coop-edit
            :teamplay
            :instagib
            :insta-team
            :efficiency
            :effic-team
            :tactics
            :tac-team
            :capture
            :regen-capture
            :ctf
            :insta-ctf
            :protect
            :insta-protect
            :hold
            :insta-hold
            :effic-ctf
            :effic-protect
            :effic-hold
            :collect
            :insta-collect
            :effic-collect])

;; serverinfo

;; command - num greater than 0
;; number of clients
;; number of attrs
;; attrs (5 or 7)
;; mapname - string
;; description - string

;; 5 attrs
;; protocol version
;; gamemode
;; remaining-time
;; maxclients
;; mastermode (enum 0-4)

;; 7 attrs = 5+
;; gamepaused 0/1 flag
;; gamespeed

(defn flag
  []
  (d/wrap-enum (d/cube-compressed-int)
               {0 false
                1 true}))

(defn mastermode
  []
  (d/wrap-enum (d/cube-compressed-int)
               {-1 :mm-auth
                0 :mm-open
                1 :mm-veto
                2 :mm-locked
                3 :mm-private
                4 :mm-password}))

(defn gamemode
  []
  (d/wrap-enum (d/cube-compressed-int)
               (partial get modes)))

(defn server-info
  []
  (d/merge-hmaps
   (d/hmap :command-id (d/cube-compressed-int)
           :clients (d/cube-compressed-int))
   (let [base-attrs [:protocol-version (d/cube-compressed-int)
                     :gamemode (gamemode)
                     :remaining-time (d/cube-compressed-int)
                     :maxclients (d/cube-compressed-int)
                     :mastermode (mastermode)]]
     (d/alternative (d/cube-compressed-int)
                    5 (apply d/hmap base-attrs)
                    7 (apply d/hmap (conj base-attrs
                                          :paused? (flag)
                                          :gamespeed (d/cube-compressed-int)))))
   (d/hmap :mapname (d/cstring)
           :description (d/cstring))))
