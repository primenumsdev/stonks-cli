(ns stonks.db
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [taoensso.nippy :as nippy])
  (:import (java.util Date UUID)))


(defn utc-now! [] (Date.))

(defn uuid! [] (str (UUID/randomUUID)))


;; This is my own DB :)

;; Features:
;; 1. Keep all db state in memory, max size is limited by RAM
;; 2. Persist state to encrypted local file
;; 3. Load state from encrypted local file
;; 4. Query as usual clojure structures with filter, map, reduce

;; TODOs:
;; 1. Backup - create file with date in the db folder
;; 2. Use optimized local state structure - transient, type hints, arrays, .length, aget - clojure perf check
;; 3. Separate file for each collection, master file for DB meta
;; 4. query-memo - to cache queries - user choose when to use memo, no indexes


;; Datalog looks complicated, I just want to save data to disk and query it
;; perhaps I will get back to it
;; https://github.com/juji-io/datalevin

;; Using nippy for serialization, encryption
;; https://github.com/ptaoussanis/nippy

;; TODO: Separate file for collection
(def file-db "")
(def file-pass "")

(def state (atom {}))

(defn override! [path-vec data]
  (-> @state
      (assoc-in path-vec data)
      (->> (reset! state))))

(defn query [path-vec]
  (-> @state (get-in path-vec)))

(defn load!
  "Load state from file db."
  []
  (prn "loading db...")
  ;; nippy fast, encryption
  (-> file-db
      (nippy/thaw-from-file {:password [:salted file-pass]})
      (assoc-in [:meta :loaded] (utc-now!))
      (->> (reset! state))
      )
  ;; simple edn slurp, open text, maybe slow? benchmark?
  ;(-> file-db
  ;    slurp
  ;    edn/read-string
  ;    (assoc-in [:meta :loaded] (utc-now!))
  ;    (->> (reset! state)))
  )

;; TODO: Add backup? option?
(defn persist!
  "Save current state to file."
  []
  (prn "saving db...")
  (let [s (assoc-in @state [:meta :updated] (utc-now!))]
    ;; nippy fast, encryption
    (nippy/freeze-to-file file-db s {:password [:salted file-pass]})
    (reset! state s)
    ;; simple edn slurp, open text, maybe slow? benchmark?
    ;(->> (spit file-db
    ;           ;:append true
    ;           ))
    )
  )

(defn -init-state
  "Create default state for new db."
  [file-path name]
  {:id (uuid!)
   :name name
   :location file-path
   :meta {:created (utc-now!)}
   })

(defn create!
  [{:keys [file-path name password]} & {:keys [override? discard?]}]
  (if (and (.exists (io/file file-path))
           (not override?))
    (prn "db already exists, use override? option, aborting...")
    (if (and (seq @state)
             (not discard?))
      (prn "current db state is not empty, use discard? option, aborting...")
      (do
        (prn (str "creating db at " file-path "..."))
        (def file-db file-path)
        (def file-pass password)
        (reset! state (-init-state file-path name))
        (persist!)
        (load!)))))

(defn connect!
  [{:keys [file-path password]} & {:keys [discard?]}]
  (if (= file-path file-db)
    (prn "already connected")
    (if (and (seq @state)
             (not discard?))
      (prn "current db state is not empty, use discard? option, aborting...")
      (if-not (.exists (io/file file-path))
        (prn (str "db not found at " file-path))
        (do
          (prn (str "connecting to db at " file-path "..."))
          (try
            ;; try load data with given creds
            (with-redefs [file-db file-path
                          file-pass password]
              (load!))
            ;; save to local state if success
            ;; otherwise it will not be reached due to exception above
            (def file-pass password)
            (def file-db file-path)
            (prn "db loaded successfully")
            (catch Exception ex
              (prn (ex-message ex))
              (prn "db load error, aborting...")
              (throw ex))
            ))))))
