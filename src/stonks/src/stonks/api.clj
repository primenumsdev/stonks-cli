(ns stonks.api
  (:require [org.httpkit.client :as http]
            [jsonista.core :as j])
  (:import (javax.net.ssl SSLEngine SSLParameters SNIHostName)
           (java.net URI)))

(def ^:dynamic *DEBUG* false)

(defn debug [& msg]
  (when *DEBUG*
    (apply prn "[DEBUG]" msg)))

(defn- sni-configure
  [^SSLEngine ssl-engine ^URI uri]
  (let [^SSLParameters ssl-params (.getSSLParameters ssl-engine)]
    (.setServerNames ssl-params [(SNIHostName. (.getHost uri))])
    (.setSSLParameters ssl-engine ssl-params)))

(def ^:private client (http/make-client {:ssl-configurer sni-configure}))

(defonce ^:private base-url "https://finnhub.io/api")
(defonce ^:private api-ver "/v1")

(def token "")

(defn set-token! [token]
  (def token token))

(defn get-quote
  "https://finnhub.io/docs/api/quote"
  [ticker]
  (let [req (http/request
              {:client       client
               :url          (str base-url api-ver "/quote")
               :method       :get
               :query-params {:symbol ticker}
               :headers      {"X-Finnhub-Token" token}})
        res @req]
    (debug "get-quote" ticker res)
    (-> res :body j/read-value)))

(comment

  (get (get-quote "AAPL") "c")
  ;{"d" -3.855 Change
  ; "dp" -2.7213, Percent change
  ; "t" 1656441986, time
  ; "l" 137.58, Low price of the day
  ; "h" 143.422, High price of the day
  ; "pc" 141.66, Previous close price
  ; "o" 142.695, Open price of the day
  ; "c" 137.805} Current price

  )
