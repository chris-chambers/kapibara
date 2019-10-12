(ns kapibara.resources
  "Tools for working with resources using their resource definitions (eg, from
  the `kapibara.discovery`).  Wraps the core Kapibara client to provide
  convenient resource-centric requests."
  (:require [clojure.data.json :as json]
            [clojure.spec-alpha2 :as s]
            [clojure.string :as str]

            [lambdaisland.uri :refer [uri] :as uri]

            [kapibara.core :as k]
            [kapibara.util :as util]))


(defn core-api?
  [group-version]
  (not (str/includes? group-version "/")))


(defn specialize
  [client resource]
  (assoc client :resource resource))


;; TODO: spec `make-path`
(defn make-path
  ([resource] (make-path resource nil))
  ([resource options]
   (if (and (not (:namespaced resource))
            (some? (:namespace/name options)))
     ;; TODO: throw an appropriate exception (maybe use spec?)
     {:error (str "cannot use namespace with non-namespaced resource")}
     (let [group-version (:groupVersion resource)
           resource-name (:name resource)
           ns (:namespace/name options)
           name (:object/name options)
           parts (if (core-api? group-version)
                   ["/api" group-version]
                   ["/apis" group-version])
           parts (if-not ns parts
                         (conj parts "namespaces" ns))
           parts (if-not resource-name parts
                         (conj parts (:name resource)))
           parts (if-not name parts (conj parts name))]
       (str/join "/" parts)))))


(defn- apply-verb
  [options resource]
  ;; TODO: Validate verb/path combinations
  (case (keyword (get options :verb :get))
    :get
    (assoc options :method :get)

    :list
    (assoc options :method :get)

    :create
    ;; TODO: better content-type, body encoding
    (-> options
        (assoc :method :post
               :content-type :application/json)
        (update :body #(if-not (string? %) (json/write-str %) %))
        ;; FIXME: annoyingly, this dissoc for :object/name needs to be done
        ;;        sooner, since :uri must already be filled before this function
        ;;        runs
        (dissoc :object/name))

    :delete
    (-> options
        (assoc :method :delete)
        ;; FIXME: It's strange to dissoc the body here.  Callers should not add
        ;;        it, or if they do, we should encode it like in create/update
        (dissoc :body))

    :deletecollection
    (assoc options :method :delete)

    :patch
    ;; TODO: content-type, body encoding
    (assoc options :method :patch)

    :update
    ;; TODO: content-type, body encoding
    (-> options
        (assoc :method :put
               :content-type :application/json)
        (update :body #(if-not (string? %) (json/write-str %) %)))

    :watch
    (-> options
        (assoc :method :get)
        ;; TODO: This update could be more beautiful, somehow.
        (update :uri #(uri/join % (->> (uri %)
                                       :query
                                       (util/split-query)
                                       (remove (fn [[k]] (= k "watch")))
                                       (concat [["watch" "true"]])
                                       (util/join-query)))))))


(defn request
  ([client] (request client nil))
  ([client options]
   (let [resource (or (:resource options) (:resource client))
         ;; FIXME: This is ugly.  I want it to be possible for this to happen in
         ;;        apply-verb (see note there).
         options (if (= :create (:verb options))
                   (dissoc options :object/name)
                   options)
         uri (uri/join (make-path resource options)
                       (:uri options))
         options (-> options
                     (assoc :uri uri)
                     (apply-verb resource)
                     (dissoc :resource
                             :verb
                             :namespace/name
                             :object/name))]
     (k/request client options))))


(defn- list-kind->object-kind
  [list-kind]
  (str/replace list-kind #"List$" ""))


(defn distribute-list-version-kind
  [list]
  (let [updater #(util/select-keys-via {:apiVersion identity
                                        :kind list-kind->object-kind}
                                       list %)]
    (update list :items #(map updater %))))


(def unpack-list
  (comp (map :items)
        cat))


(comment

  (do
    (def configmaps {:groupVersion "v1"
                     :name "configmaps"
                     :namespaced true})

    (def pvs {:groupVersion "v1"
              :name "persistentvolumes"
              :namespaced false})

    (def namespaces {:groupVersion "v1"
                     :name "namespaces"
                     :namespaced false}))

  (make-path configmaps {:namespace/name "default"})
  (make-path configmaps {:namespace/name "default"
                         :object/name "foo"})

  ;; invalid (namespace not allowed)
  (make-path pvs {:namespace/name "default"})

  (make-path pvs)
  (make-path pvs {:object/name "foo"})

  (make-path namespaces)
  (make-path namespaces {:object/name "default"})

  (make-path {:groupVersion "v1"})
  (make-path {:groupVersion "node.k8s.io/v1beta1"})

  (list-kind->object-kind "ConfigMapList")

  (def client (specialize (k/make-client "http://localhost:8080")
                          configmaps))

  (def req (request client {:namespace/name "default"
                            :object/name "foo"
                            :verb :get}))

  (def req (request client {:namespace/name "default"
                            :object/name "baz"
                            :verb :delete}))

  (clojure.core.async/<!! @req)

  (k/abort! req)

  (apply-verb {:verb :get} {})
  (apply-verb {:verb "get"} {})

  (apply-verb {:verb :watch
               :uri "http://localhost:8080/api/v1/namespaces/default/configmaps"}
              {})

  (apply-verb {:verb :watch
               :uri "http://localhost:8080?watch=false&fun=times"}
              {})

  )
