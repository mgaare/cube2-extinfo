(ns cube2-extinfo.extinfo-test
  (:require [clojure.test :refer :all]
            [cube2-extinfo.deserialise :as d])
  (:use cube2-extinfo.extinfo))

(def server-info-response
  (byte-array '(1 27 5 -128 3 1 12 -128 56 1 32 1 108 95 99 116 102 0 12 55 58
                58 32 80 97 115 116 97 108 97 110 100 32 58 58 0)))

(def uptime-response
  (byte-array '(0 0 -1 105 -127 -44 -99 0 0)))

(def player-stats-response
  (byte-array '(0 1 -1 -1 105 0 -11 2 21 115 105 110 115 99 104 0 103 111 111
                100 0 13 0 6 3 36 1 0 0 0 0 -39 -32 0)))

(def team-score-response
  '(0 2 -1 105 0 12 -128 101 1 101 118 105 108 0 0 -1 103 111 111 100 0 0 -1))

(deftest server-info-test
  (is (= (d/byte-seq (server-info) server-info-response)
         [{:command-id 1, :clients 27, :protocol-version 259,
           :gamemode :insta-ctf, :remaining-time 312, :maxclients 32,
           :mastermode :mm-veto, :mapname "l_ctf",
           :description "\f7:: Pastaland ::"}])))

(deftest uptime-test
  (is (= (d/byte-seq (uptime) uptime-response)
         [{:prefix 0, :ext-command :ext-uptime, :ack :ext-ack, :ext-version 105,
           :uptime 40404}])))

(deftest player-stats-test
  (is (= (d/byte-seq (player-stats) player-stats-response))
      [{:prefix 0, :ext-command :ext-playerstats, :cn -1, :ack :ext-ack,
        :ext-version 105}]))

(deftest player-stats-response-stats-test
  (is (= (d/byte-seq (player-stats-response-stats)
                     (drop 6 player-stats-response))
         [{:ping 21, :frags 13, :ip "217.224.0.255", :gun 0, :acc 36,
           :name "sinsch", :deaths 6, :ext-command :ext-playerstats-resp-stats,
           :teamkills 3, :state 0, :team "good", :health 1, :flags 0,
           :armour 0, :cn 2, :privilege 0}])))

(deftest team-scores-test
  (is (= (d/byte-seq (team-scores)
                     team-score-response))
      [{:prefix 0, :ext-command :ext-teamscore, :ack :ext-ack,
        :ext-version 105, :error? false, :gamemode :insta-ctf,
        :remaining-time 357,
        :teams [{:team "evil", :score 0} {:team "good", :score 0}]}]))

(deftest info-test
  (is (= (d/byte-seq (info)
                     player-stats-response)
         [{:prefix 0, :ext-command :ext-playerstats, :cn -1,
           :ack :ext-ack, :ext-version 105, :error? false}
          {:ping 21, :frags 13, :ip "217.224.0.255", :gun 0, :acc 36,
           :name "sinsch", :deaths 6, :teamkills 3, :state 0, :team "good",
           :health 1, :ext-command :ext-playerstats-resp-stats, :flags 0,
           :armour 0, :cn 2, :privilege 0}])))
