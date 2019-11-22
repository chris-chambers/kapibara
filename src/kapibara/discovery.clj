(ns kapibara.discovery
  "Facilities for discovering resources available in a Kubernetes cluster"
  (:require [clojure.core.async :refer [<! >!] :as async]
            [clojure.string :as str]

            [lambdaisland.uri :refer [uri] :as uri]

            [kapibara.core :as k]
            [kapibara.resources :as res]
            [kapibara.util :as util]))


(defn distribute-list-group-version
  [list]
  (let [updater #(util/select-keys-via {:groupVersion identity} list %)]
    (update list :resources #(map updater %))))


(defn get-api-resources
  [client group-version]
  (k/send! client
           (k/request client {:method :get :uri (res/make-path group-version)})))


(defn get-api-groups
  [client]
  (let [ch (async/chan)]
    ;; FIXME: error handling for requests (if any error, die)
    (async/go
      (let [core-info (<! @(k/send! client (k/request client {:method :get :uri "/api"})))
            groups (<! @(k/send! client (k/request client {:method :get :uri "/apis"})))
            core-versions (into [] (map #(hash-map :groupVersion %
                                                   :version %))
                                (:versions core-info))
            core-group {:name ""
                        :versions core-versions
                        :preferredVersion (get core-versions 0)}]
        (>! ch (update groups :groups #(into [] (concat [core-group] %))))))
    ;; FIXME: Implement aborting.
    (k/->Response ch nil)))


(comment
  (do
    (require '[clojure.core.async :refer [<!!]])
    (def client (k/client {:uri "http://localhost:8001"})))

  (<!! @(get-api-resources client {:groupVersion "v1"}))
  (<!! @(get-api-groups client))

  (let [all-res (->> (<!! @(get-api-groups client))
                     :groups
                     (sequence (comp
                                (map :preferredVersion)
                                (map (partial get-api-resources client))
                                (map deref)))
                     async/merge
                     (async/into [])
                     (<!!))]
    (reduce cache-api-resource-list {} all-res))

  )
