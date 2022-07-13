(ns stonks.http
  (:refer-clojure :exclude [get])
  (:import (java.net.http HttpRequest HttpClient HttpResponse$BodyHandlers)
           (java.net URI)))

(defonce ^:private client (delay (HttpClient/newHttpClient)))

(defn ->qs [query-params]
  (.toString (reduce-kv
               (fn [qs k v]
                 (.append qs (str
                               (if (= 0 (.length qs)) "?" "&")
                               k "=" v)))
               (StringBuilder.)
               query-params)))

(defn get [{:keys [url query-params headers]}]
  (let [req (.build (doto
                      (HttpRequest/newBuilder)
                      (.uri (URI. (str url (->qs query-params))))
                      (.headers (into-array (into [] cat headers)))
                      (.GET)))
        res (.send @client req (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode res)
     :body   (.body res)}))


(comment

  (get {:url          "https://finnhub.io/api/v1/quote"
        :query-params {"symbol" "AAPL"}
        :headers      {"X-Finnhub-Token" ""}})
  )


