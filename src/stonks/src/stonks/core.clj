(ns stonks.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [clojure.core.memoize :as memo]
            [stonks.db :as db]
            [stonks.api :as api])
  (:import (java.util Date)
           (java.math RoundingMode))
  (:gen-class))

(defn utc-now! [] (Date.))

(defn user-home-dir []
  (System/getProperty "user.home"))

(defn userdata-path []
  (str (user-home-dir) "/.stonks/userdata.db"))

(defn userdata-exists? []
  (.exists (io/file (userdata-path))))

(defn read-big-dec []
  (BigDecimal. (str (read-line))))

(defn read-int []
  (Integer/parseInt (str (read-line))))

(defn prompt-str [msg]
  (println msg)
  (read-line))

(defn prompt-int [msg]
  (println msg)
  (try
    (read-int)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-int msg))))

(defn prompt-big-dec [msg]
  (println msg)
  (try
    (read-big-dec)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-int msg))))

(defn round2 [^BigDecimal big-dec]
  (.setScale (bigdec big-dec) 2 RoundingMode/HALF_UP))

(defn initial-setup! []
  (let [username (prompt-str "Hello, what is your name?")]
    (printf "Hi %s!\n" username)
    (println "We are going to setup a new user data file for you.")
    (let [pass (prompt-str "Please specify a password:")]
      (db/create!
        {:file-path (userdata-path)
         :name      "userdata"
         :password  pass}
        {:override? true
         :discard?  true}))
    (let [token (prompt-str "Enter finnhub.io API token:")]
      (db/set :finnhub-token token)
      (api/set-token! token))
    (db/with-save!
      (db/set :username username)
      (db/set :registered (utc-now!))
      (db/set :last-login (utc-now!))
      (db/set :transactions []))))

(defn load-userdata! []
  (let [pass (prompt-str "Hello, please enter your password:")]
    (db/connect!
      {:file-path (userdata-path)
       :password  pass}))
  (printf "Welcome back, %s!\n" (db/get :username))
  (printf "Last login: %s\n" (db/get :last-login))
  (db/with-save!
    (db/set :last-login (utc-now!)))
  (api/set-token! (db/get :finnhub-token)))

(defn add-new-transaction [trans-type]
  (let [ticker (prompt-str "Ticker:")
        amt    (prompt-int "Amount:")
        price  (prompt-big-dec "Price:")]
    (db/with-save!
      (db/update :transactions #(conj % [trans-type ticker amt price "USD" (utc-now!)])))))

(defn separator []
  (println "\n---\n"))

(defn approx [price]
  (format "~%s" price))

(defn usd [price]
  (format "%sUSD" price))

(defn pct [num]
  (format "%s%%" num))

(defn div-round2 [^BigDecimal dividend ^BigDecimal divisor]
  (.divide dividend divisor 2 RoundingMode/HALF_UP))

(defn buy-sell->+- [tr]
  (case (first tr)
    :buy +
    :sell -))

(defn sum-amt [sum tr]
  ((buy-sell->+- tr) sum (nth tr 2)))

(defn -get-cur-price [ticker]
  (bigdec (get (api/get-quote ticker) "c")))

(def get-cur-price
  (memo/ttl -get-cur-price {} :ttl/threshold 10000))

(defn sum-cur-val [sum ticker amt]
  (+ sum (round2 (* (get-cur-price ticker) amt))))

(defn sum-cost [sum tr]
  ;; Cost = Amt * Price
  (+ sum (round2 (* (nth tr 2) (nth tr 3)))))

(defn spent
  "
  Spent, sum of buy transactions multiplied by it's amounts.
  Spent = sum(amt * price)
  "
  ([trans] (spent nil trans))
  ([ticker trans]
   (->> trans
        (filter #(= :buy (first %)))
        (filter #(if (some? ticker)
                   (= ticker (second %))
                   true))
        (reduce sum-cost 0))))

(defn avg-price
  "
  AvgPrice, average price.
  Avg.Price = Total Spent / Amount
  "
  [^BigDecimal spent ^Long amt]
  (div-round2 spent (bigdec amt)))

(defn perf
  "
  Performance, profit or loss in percentage.
  Perf = (CurVal - Spent) / 1%ofSpent
  "
  [^BigDecimal cur-val ^BigDecimal spent]
  (div-round2 (- cur-val spent) (/ spent 100)))

(defn best-worst-perf
  "Best and worst performers based on profit."
  [holdings]
  (let [sorted   (sort-by :profit-val holdings)
        sel-keys [:ticker :profit :perf]]
    [(select-keys (last sorted) sel-keys)
     (select-keys (first sorted) sel-keys)]))

(defn allocation
  "Allocation = Amt / 1%ofTotal."
  [ticker-amt]
  (let [total-amt (->> ticker-amt
                       (reduce-kv (fn [t _ v]
                                    (+ t v))
                                  0))
        total%1   (div-round2 (bigdec total-amt) 100M)]
    (->> ticker-amt
         (reduce-kv (fn [m k v]
                      (assoc m k (div-round2 (bigdec v) total%1)))
                    {}))))

(defn stats []
  (let [trans        (db/get :transactions)
        total-spent  (spent trans)
        ticker-amt   (->> trans
                          (group-by second)
                          (reduce-kv (fn [m k v]
                                       (assoc m k (reduce sum-amt 0 v)))
                                     {}))
        ticker-alloc (allocation ticker-amt)
        cur-val      (reduce-kv sum-cur-val 0 ticker-amt)
        total-profit (usd (- cur-val total-spent))
        total-perf   (perf cur-val total-spent)
        holdings     (->> ticker-amt
                          (reduce-kv (fn [h k v]
                                       (let [cur-price (get-cur-price k)
                                             cur-val   (round2 (* v cur-price))
                                             spent     (spent k trans)]
                                         (conj h
                                               {:ticker     k
                                                :amount     v
                                                :cur-price  (usd cur-price)
                                                :avg-price  (usd (approx (avg-price spent v)))
                                                :spent      (usd spent)
                                                :cur-val    (usd cur-val)
                                                :profit-val (- cur-val spent)
                                                :profit     (usd (- cur-val spent))
                                                :perf       (pct (perf cur-val spent))
                                                })))
                                     []))
        [best-perf worst-perf] (best-worst-perf holdings)]
    (separator)
    (printf "Total spent: %s\n" (usd total-spent))
    (printf "Current evaluation: %s\n" (usd cur-val))
    (printf "Total profit: %s\n" total-profit)
    (printf "Performance: %s\n" (pct total-perf))
    (printf "Best performer: %s\n" best-perf)
    (printf "Worst performer: %s\n" worst-perf)
    (printf "Allocation: %s\n" ticker-alloc)
    (separator)
    (println "Holdings:")
    (print-table [:ticker :amout :cur-price :avg-price :spent :cur-val :profit :perf] holdings)
    (separator)
    (println "Transactions:")
    (print-table (mapv
                   #(zipmap [:type :ticker :amount :price :currency :time] %)
                   trans))))

(defn menu []
  (separator)
  (println "Menu:\n")
  (println "1 - Add buy transaction")
  (println "2 - Add sell transaction")
  (println "c - Clear all transactions")
  (println "q - Exit")
  (separator))

(defn dashboard [& {:keys [render-stats?]
                    :or   {render-stats? true}}]
  (when render-stats?
    (stats))
  (menu)
  ;; wait user input
  (case (read-line)
    "1" (do
          (println "Add new buy transaction")
          (add-new-transaction :buy)
          (recur nil))
    "2" (do
          (println "Add new sell transaction")
          (add-new-transaction :sell)
          (recur nil))
    ("c" "C") (do
                (println "Clearing all transactions...")
                (db/with-save!
                  (db/set :transactions []))
                (recur nil))
    ("q" "Q") (do
                (println "Bye bye!"))
    (do
      (println "Invalid input, try again.")
      (recur {:render-stats? false}))))

(defn -main
  "Entry."
  [& args]
  ;; check userdata
  (if-not (userdata-exists?)
    (initial-setup!)
    (load-userdata!))
  (dashboard))

(comment
  (binding [db/*DEBUG*  false
            api/*DEBUG* false]
    (-main))


  ;; TODO: implement allocation chart
  (defn print-progress-bar [percent]
    (let [bar (StringBuilder. "[")]
      (doseq [i (range 50)]
        (cond (< i (int (/ percent 2))) (.append bar "=")
              (= i (int (/ percent 2))) (.append bar ">")
              :else (.append bar " ")))
      (.append bar (str "] " percent "%     "))
      (print "\r" (.toString bar))
      (flush)))

  (print-progress-bar 56)

  )
