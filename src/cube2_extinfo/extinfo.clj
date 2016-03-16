(ns cube2-extinfo.extinfo
  (:require [clojure.string :as str]
            [cube2-extinfo.deserialise :as d]))

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


(defn command
  []
  (d/wrap-enum (d/cube-compressed-int)
               {-10 :ext-playerstats-resp-ids
                -11 :ext-playerstats-resp-stats
                0   :ext-uptime
                1   :ext-playerstats
                2   :ext-teamscore}))

(defn ack
  []
  (d/wrap-required (d/wrap-enum (d/cube-compressed-int)
                                {-1 :ext-ack})
                   :ext-ack))

(defn uptime
  []
  (d/hmap :prefix (d/cube-compressed-int)
          :ext-command (d/wrap-required (command) :ext-uptime)
          :ack (ack)
          :ext-version (d/cube-compressed-int)
          :uptime (d/cube-compressed-int)))

(defn player-stats-response-stats
  []
  (d/hmap :ext-command (d/wrap-required (command)
                                        :ext-playerstats-resp-stats)
          :cn (d/cube-compressed-int)
          :ping (d/cube-compressed-int)
          :name (d/cstring)
          :team (d/cstring)
          :frags (d/cube-compressed-int)
          :flags (d/cube-compressed-int)
          :deaths (d/cube-compressed-int)
          :teamkills (d/cube-compressed-int)
          :acc (d/cube-compressed-int)
          :health (d/cube-compressed-int)
          :armour (d/cube-compressed-int)
          :gun (d/cube-compressed-int)
          :privilege (d/cube-compressed-int)
          :state (d/cube-compressed-int)
          :ip (d/wrap-xform
               (d/sequential
                (d/unsigned-byte-d) (d/unsigned-byte-d) (d/unsigned-byte-d))
               (fn [ips] (str (str/join "." ips) ".255")))))

(defn player-stats
  []
  (d/hmap :prefix (d/wrap-required (d/cube-compressed-int) 0)
          :ext-command (d/wrap-required (command) :ext-playerstats)
          :cn (d/cube-compressed-int)
          :ack (ack)
          :ext-version (d/cube-compressed-int)
          :error? (flag)))

(defn player-stats-response-ids
  []
  (d/hmap :ext-command (d/wrap-required (command)
                                        :ext-playerstats-resp-ids)
          :ids (d/repeat (d/wrap-non-negative (d/cube-compressed-int)))))
