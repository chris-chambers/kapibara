(ns kapibara.util
  (:require [clojure.string :as str])
  (:import [java.net URLEncoder URLDecoder]))


(defn select-keys-via
  ([spec from]
   (select-keys-via spec from nil))
  ([spec from to]
   (loop [spec' spec
          to' (transient to)]
     (if-let [[k transform] (first spec')]
       (let [v (get from k ::none)
             to' (if (identical? v ::none)
                   to'
                   (assoc! to' k (transform v)))]
         (recur (rest spec') to'))
       (persistent! to')))))


(defn split-group-version
  [gv]
  (let [[group-or-version maybe-version] (str/split gv #"/" 2)]
    (if maybe-version
      [group-or-version maybe-version]
      ["" group-or-version])))


(defn url-decode
  [^String s]
  (URLDecoder/decode s))


(defn url-encode
  [^String s]
  (URLEncoder/encode s))


(defn join-query
  [parts]
  (when (not-empty parts)
    (->> (sequence (comp
                    (map #(map url-encode %))
                    (map #(str/join "=" %)))
                   parts)
         (str/join "&")
         (str "?"))))


(defn split-query
  [s]
  (when s
    (as-> s x
      (url-decode x)
      (str/replace x #"^\?" "")
      (str/split x #"[&]")
      (map #(str/split % #"=") x))))


(comment
  (join-query [["x" "y"]
               ["z" "great times"]])

  (-> "?x=y&z=great+times"
   (split-query)
   (concat [["watch" "true"]])
   (join-query))

  (split-query nil)
  (join-query nil)
  (join-query [])


  (select-keys-via {:foo identity
                    :bar inc
                    :baz dec}
                   {:foo 5
                    :bar 6
                    :quux 99}
                   {})

  (split-group-version "v1")
  (split-group-version "node.k8s.io/v1")

  )
