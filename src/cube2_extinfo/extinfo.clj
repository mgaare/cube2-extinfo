(ns cube2-extinfo.extinfo
  (:require [cube2-extinfo.deserialise :as d]))

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
   :m-overtime (bit-shift-left 1 11)
   :m-edit (bit-shift-left 1 12)
   :m-demo (bit-shift-left 1 13)
   :m-local (bit-shift-left 1 14)
   :m-lobby (bit-shift-left 1 15)
   :m-dmsp (bit-shift-left 1 16)
   :m-classicsp (bit-shift-left 1 17)
   :m-slowmo (bit-shift-left 1 18)
   :m-collect (bit-shift-left 1 19)})

(defn flags
  [& flags]
  (let [m-flags (map mode-flags flags)]
    (if (> (count flags) 1)
      (apply bit-or m-flags)
      (first m-flags))))

(def modes
  {:sp (flags :m-local :m-classicsp)
   :dmsp (flags :m-local :m-dmsp)
   :demo (flags :m-demo :m-local)
   :ffa (flags :m-lobby)
   :coop-edit (flags :m-edit)
   :teamplay (flags :m-team)
   :instagib (flags :m-noitems :m-insta)
   :insta-team (flags :m-noitems :m-insta :m-team)
   :efficiency (flags :m-noitems :m-efficiency)
   :effic-team (flags :m-noitems :m-efficiency :m-team)
   :tactics (flags :m-noitems :m-tactics)
   :tac-team (flags :m-noitems :m-tactics :m-team)
   :capture (flags :m-noammo :m-tactics :m-capture :m-team)
   :regen-capture (flags :m-noitems :m-capture :m-regen :m-team)
   :ctf (flags :m-ctf :m-team)
   :insta-ctf (flags :m-noitems :m-insta :m-ctf :m-team)
   :protect (flags :m-ctf :m-protect :m-team)
   :insta-protect (flags :m-noitems :m-insta :m-ctf :m-protect :m-team)
   :hold (flags :m-ctf :m-hold :m-team)
   :insta-hold (flags :m-noitems :m-insta :m-ctf :m-hold :m-team)
   :effic-ctf (flags :m-noitems :m-efficiency :m-ctf :m-team)
   :effic-protect (flags :m-noitems :m-efficiency :m-ctf :m-protect :m-team)
   :effic-hold (flags :m-noitems :m-efficiency :m-ctf :m-hold :m-team)
   :collect (flags :m-collect :m-team)
   :insta-collect (flags :m-noitems :m-insta :m-collect :m-team)
   :effic-collect (flags :m-noitems :m-efficiency :m-collect :m-team)})


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

(defn server-info
  []
  (d/merge-hmaps
   (d/hmap :command-id (d/cube-compressed-int)
           :clients (d/cube-compressed-int))
   (let [base-attrs [:protocol-version (d/cube-compressed-int)
                     :gamemode (d/cube-compressed-int)
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
