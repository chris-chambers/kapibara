(ns kapibara.core
  "The core of Kapibara.  Provides low-level access to the Kubernetes API."
  (:require [clojure.core.async :refer [>!!] :as async]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]

            [lambdaisland.uri :refer [uri] :as uri]

            [kapibara.http :as http])

  (:import [java.net.http HttpResponse]))


;; TODO: Decide whether the base URI should be attached to a client.  It's
;;       convenient (especially for `kapibara.resources/specialize`), but is it
;;       a good thing to do?
(defn client
  ([{:keys [uri] :as options}]
   {:http-client (http/client options)
    :uri uri}))


(defn request
  [client {request-uri :uri
           :as options}]

  (let [server-uri (:uri client)
        final-uri (java.net.URI/create (str (uri/join server-uri request-uri)))]
    (http/request (assoc options :uri final-uri))))

(comment
  ;; TODO: Support authentication
  :auth/username
  :auth/password

  :auth/ca-cert
  :auth/client-cert
  :auth/client-key

  :auth/oauth-token
  :auth/oauth-token-fn)

;; TODO: This should be updated to work more nicely with the BodySubscriber
;;       idiom that the new HttpClient uses.
(defn- read-json-stream
  [to-chan ^java.io.InputStream from-stream]
  (loop [rdr (io/reader from-stream)]
    (let [obj (json/read rdr
                         :eof-error? false
                         :key-fn keyword)]
      (when (and (some? obj)
                 (>!! to-chan obj))
        (recur rdr)))))


(deftype Response [chan abortfn]
  clojure.lang.IDeref
  (deref [this] chan))


(defn- abort-fn
  [^Response resp]
  (.abortfn resp))


(defn abort!
  [resp]
  ((abort-fn resp)))

(defn send!
  [client req]
  ;; TODO: This can be much better now that JDK11's HttpClient is being used.
  ;;       Should no longer need to create our own thread, and it may be
  ;;       possible to implement a BodySubscriber+BodyHandler that directly
  ;;       produces a chan of results.
  (let [ch (async/chan)
        p-body (promise)]
    (async/thread
      (try
        (let [^HttpResponse resp (http/send! (:http-client client) req)]
          (with-open [^java.io.InputStream body (.body resp)]
            (deliver p-body body)
            (read-json-stream ch body)))
        (catch Exception e
          (>!! ch {::error e}))
        (finally
          (async/close! ch))))
    (->Response ch (fn [] (let [body ^java.io.InputStream @p-body]
                            (.close body))))))


(defn update-response-chan
  [resp f & args]
  (->Response (apply f @resp args) (abort-fn resp)))


;; TODO: It may be necessary for `merge-responses` to invalidate the original
;;       `Response` objects, since using them after this call could be an error.
(defn merge-responses
  [reqs]
  (let [reqs' (vec reqs)]
    (->Response
     (async/merge (map deref reqs'))
     (fn [] (run! #(%) (map abort-fn reqs'))))))


(comment

  (def c (client nil))

  (def x (send! c
                (request {;; :method :get
                          :server/uri "http://localhost:8001"
                          :request/uri "/api/v1/namespaces/default/configmaps"})))


  (def x (merge-responses
          [
           (update-response-chan

            (request c
                     {:method :get
                      :uri "/api/v1/namespaces/default/configmaps/foo"})
            #(let [ch (async/chan 1 (map (fn [x] :bonk)))]
               (async/pipe % ch)))
           (request c
                    {:method :get
                     :uri "/api/v1/namespaces/default/configmaps/bar"})]))

  (async/<!! @x)

  (abort! x)

  (async/<!! (async/into [] (request c {:uri "/apis"
                                        :method :get
                                        :query {:watch true}})))

  )
