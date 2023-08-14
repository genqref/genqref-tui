(ns genqref-tui.core
  (:require [hawk.core :as hawk]
            [lanterna.screen :as s]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
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

(defn update-tui [scr _key _ref _old {:keys [local agent ships] :as new}]
  ;; TODO: maybe `(clojure.data/diff old new)`
  (s/clear scr)
  (s/put-string scr 0 0 (str "Credits: " (-> new :agent :credits)))
  (doseq [{:keys [index] :as ship} (indexed (sort-by ship-index (vals (:ships new))))]
     (s/put-string scr 0 (+ index 2) (or (:symbol ship) "-")))
  (let [[x y] (map dec (s/get-size scr))]
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
