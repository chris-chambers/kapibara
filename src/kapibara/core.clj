(ns kapibara.core
  "The core of Kapibara.  Provides low-level access to the Kubernetes API."
  (:require [clojure.core.async :refer [>!!] :as async]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]

            [clj-http.lite.client :as http]
            [lambdaisland.uri :refer [uri] :as uri]))


(defn make-client
  ([server] (make-client server nil))
  ([server options]
   (merge {:server/uri (uri server)} options)))


(comment
  ;; TODO: Support authentication via the following keys
  :auth/username
  :auth/password

  :auth/ca-cert
  :auth/client-cert
  :auth/client-key

  :auth/oauth-token
  :auth/oauth-token-fn)


(def ^:private clj-http-key-whitelist
  [:method :content-type :body :debug?])


(defn- request-options
  [client options]
  (let [url (str (uri/join (:server/uri client) (:uri options)))
        req-opts (merge {:url           url
                         :as            :stream
                         ;; TODO: Move insecure calculation to wherever
                         ;;       authentication options are calculated.
                         :insecure?     true
                         :save-request? true}
                        (select-keys options clj-http-key-whitelist))]
    (if-let [interceptor (:interceptor options)]
      (interceptor client options req-opts)
      req-opts)))


(defn- read-json-stream
  [to-chan ^java.io.InputStream from-stream]
  (loop [rdr (io/reader from-stream)]
    (let [obj (json/read rdr
                         :eof-error? false
                         :key-fn keyword)]
      (when (and (some? obj)
                 (>!! to-chan obj))
        (recur rdr)))))


(deftype Request [chan abortfn]
  clojure.lang.IDeref
  (deref [this] chan))


(defn- abort-fn
  [^Request req]
  (.abortfn req))


(defn abort!
  [req]
  ((abort-fn req)))

(defn request
  [client options]
  (let [ch (async/chan)
        p-request (promise)]
    (async/thread
      (try
        (let [req-opts (request-options client options)
              resp (http/request req-opts)]
          (deliver p-request (:request resp))
          (with-open [^java.io.InputStream body (:body resp)]
            (read-json-stream ch body)))
        (catch Exception e
          (>!! ch {::error e}))
        (finally
          (async/close! ch))))
    (->Request ch (fn []
                    (throw (Exception. "abort! cannot be implemented with clj-http-lite. Waiting on GraalVM 19.3, for JDK 11 and the new HTTPClient"))
                    #_(let [^org.apache.http.client.methods.AbortableHttpRequest req
                            (:http-req @p-request)]
                        (.abort req))))))


(defn update-request-chan
  [req f & args]
  (->Request (apply f @req args) (abort-fn req)))


;; TODO: It may be necessary for `merge-requests` to invalidate the original
;;       `Request` objects, since using them after this call could be an error.
(defn merge-requests
  [reqs]
  (let [reqs' (into [] reqs)]
    (->Request
     (async/merge (map deref reqs'))
     (fn [] (run! #(%) (map abort-fn reqs'))))))


(comment

  (def client (make-client "http://localhost:8080"))

  (def x (request client
                  {:method :get
                   :uri "/api/v1/namespaces/default/configmaps"}))

  (def x (merge-requests
          [
           (update-request-chan
            (request client
                     {:method :get
                      :uri "/api/v1/namespaces/default/configmaps/foo"})
            #(let [ch (async/chan 1 (map (fn [x] :bonk)))]
               (async/pipe % ch)))
           (request client
                    {:method :get
                     :uri "/api/v1/namespaces/default/configmaps/bar"})]))

  (async/<!! @x)

  (abort! x)

  (async/<!! (async/into [] (request client {:uri "/apis"
                                             :method :get
                                             :query {:watch true}})))

  )
