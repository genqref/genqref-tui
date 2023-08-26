(ns genqref-tui.core
  (:require [hawk.core :as hawk]
            [lanterna.screen :as s]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tick.core :as t]
            [tick.timezone]
            [tick.alpha.interval :as t.i])
  (:gen-class))

;; TODO: add lein-bin-plus

(defn- indexed [coll]
  (map-indexed #(assoc %2 :index %1) coll))

(defn- ship-index [ship]
  (if-let [sym (:symbol ship)]
    (as-> sym %
      (str/split % #"-")
      (last %)
      (str "0x" %)
      (read-string %))
    999))

(defn cooldown [ship]
  (if-let [exp (-> ship :cooldown :expiration)]
    (t/seconds (t/between (t/now) (t/instant exp)))
    0))

(defn arrival [ship]
  (if-let [arrival (-> ship :nav :route :arrival)]
    (t/seconds (t/between (t/now) (t/instant arrival)))
    0))

(defn wait-time [ship]
  (max (cooldown ship) (arrival ship)))

(defn cooldown? [ship]
  (when-let [result (cooldown ship)]
    (pos? result)))

(defn arrival? [ship]
  (when-let [result (arrival ship)]
    (pos? result)))

(defn now-ts []
  (quot (System/currentTimeMillis) 1000))

(defn active? [{:keys [heartbeat]}]
  (when heartbeat
    (> 5 (- (now-ts) heartbeat))))

(defn ship-color [ship]
  (cond
    (active? ship) :yellow
    (pos? (wait-time ship)) :green
    :else :red))

(defn update-tui [scr _key _ref _old {:keys [local agent ships] :as new}]
  ;; TODO: maybe `(clojure.data/diff old new)`
  (s/clear scr)
  (s/put-string scr 0 0 (str (t/now)
                             " Token-key: " (str/join "/" (:token-key new))
                             " Phase: " (:phase new)
                             " Credits: " (-> new :agent :credits)))
  (s/put-string scr 0 1 (str "Contracts: " (->> new :contracts vals (filter (comp not :accepted)) count)
                             "/" (->> new :contracts vals (filter #(and (:accepted %) (not (:fulfilled %)))) count)
                             "/" (->> new :contracts vals (filter #(and (:accepted %) (:fulfilled %))) count)
                             " Waypoints: " (-> new :waypoints count)
                             " Markets: " (-> new :markets count)
                             " Shipyards: " (-> new :shipyards count)
                             " Surveys: " (->> new :surveys vals (apply concat) count)))
  (s/put-string scr 0 2 (str "Ships: " (-> new :ships count)
                             "/" (->> new :ships vals (filter active?) count)
                             "/" (->> new :ships vals (filter cooldown?) count)
                             "/" (->> new :ships vals (filter arrival?) count)))
  (let [[x y] (map dec (s/get-size scr))
        lines (- y 5)
        ships (indexed (sort-by ship-index (vals (:ships new))))
        groups (partition lines lines [] ships)]
    (doseq [[column group] (map-indexed (fn [index group] [index group]) groups)]
      (doseq [{:keys [index] :as ship} group]
        (s/put-string scr
                      (* column (/ x (count groups))) ;; x
                      (+ (- index (* column lines)) 4) ;; y
                      (format
                       "%-10s %-13s %5s"
                       (:symbol ship)
                       (:assignment ship)
                       (wait-time ship))
                      {:fg (ship-color ship)})))
    (s/move-cursor scr 2 y)
    (s/put-string scr 0 y (str (:page local) "/" (:pages local) " > " (-> new :local :key))))
  (s/redraw scr))

(defn file-to-atom [file state]
  (let [payload (-> file io/reader (json/parse-stream true))]
    (reset! state (assoc payload :local (:local @state)))))

(defn handler [state ctx {:keys [file] :as event}]
  ;; (println "event:" event)
  (file-to-atom file state)
  ctx)

(defn quit [{:keys [screen watcher]}]
  (s/stop screen)
  (println "\nStopped screen.")
  (println "Stopping watcher...")
  (hawk/stop! watcher)
  (println "Bye.")
  (System/exit 0))

(defn start-input-handler [{:keys [screen state] :as opts}]
  (future
    (while true
      (let [k (s/get-key-blocking screen)]
        (case k
          :escape (quit opts)
          \q (quit opts)
          :page-down (swap! state update-in [:local :page] inc)
          :page-up (swap! state update-in [:local :page] dec)
          (swap! state assoc-in [:local :key] k))))))

(defn -main [& args]
  (let [state (atom {})
        updater (future
                  (while true
                    (swap! state assoc :time (t/time))
                    (Thread/sleep 1000)))
        screen (s/get-screen :text)
        watcher (hawk/watch! [{:paths args
                               :handler (partial handler state)}])]
    (s/start screen)
    (start-input-handler {:state state
                          :screen screen
                          :watcher watcher})
    (add-watch state :update-tui (partial update-tui screen))
    ;; initial
    (file-to-atom (io/file (first args)) state)
    ))
