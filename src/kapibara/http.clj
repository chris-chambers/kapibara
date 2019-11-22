(ns kapibara.http
  (:require [clojure.string :as str])
  (:import [java.io InputStream]
           [java.net.http
            HttpClient
            HttpClient$Builder
            HttpClient$Version
            HttpRequest
            HttpRequest$Builder
            HttpRequest$BodyPublisher
            HttpRequest$BodyPublishers
            HttpResponse$BodyHandler
            HttpResponse$BodyHandlers
            HttpResponse$BodySubscriber
            HttpResponse$ResponseInfo]

           [java.util.function Supplier]))


(defn client
  [options]
  ;; TODO: Support the whole HttpClient(Builder) API
  (-> (HttpClient/newBuilder)
      .build))

(defn- coerce-to-string
  [v]
  (if (keyword? v) (subs (str v) 1) (str v)))

(defn- apply-method
  [^HttpRequest$Builder builder method body]
  (let [method (str/upper-case (coerce-to-string method))
        publisher (cond
                    (instance? HttpRequest$BodyPublisher body) body
                    ;; TODO: Support charset?
                    (string? body) (HttpRequest$BodyPublishers/ofString body)
                    (nil? body) (HttpRequest$BodyPublishers/noBody)
                    :else (throw (ex-info "unsupported body type" {:body body})))]
    (.method builder method publisher)))


;; TODO: Maybe it's better to treat headers as headers, rather than doing
;;       special handling for some.  If we want to give convenience, there could
;;       be free-standing functions that header composition and prevent header
;;       name typos.
(defn- apply-content-type
  [^HttpRequest$Builder builder content-type]
  (if content-type
    (.setHeader builder "Content-Type"  (coerce-to-string content-type))
    builder))


(defn request
  [{:keys [body content-type method uri]}]
  ;; TODO: Support the whole HttpRequest(Builder) API
  (let [^HttpRequest$Builder builder (HttpRequest/newBuilder)]
    (-> builder
        (apply-method method body)
        (apply-content-type content-type))
    ;; TODO: Am I doing something wrong?  When using `->` or `doto`, the type
    ;;       hint seems to be lost, and then these methods need to be done via
    ;;       reflection, which fails since the builder objects are private.
    (.uri builder uri)
    (.build builder)))


(defn send!
  [^HttpClient client ^HttpRequest req]
  (.send client req (HttpResponse$BodyHandlers/ofInputStream)))


;; (defn send-async!
;;   [^HttpClient client ^HttpRequest req]
;;   (.sendAsync client req))


(comment
  (clojure.reflect/reflect HttpClient$Version)

  )
