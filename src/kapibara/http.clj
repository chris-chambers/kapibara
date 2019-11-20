(ns kapibara.http
  (:import [java.net.http HttpClient HttpRequest HttpClient$Version])

  )

(def ^:private versions
  {:v1.1 HttpClient$Version/HTTP_1_1
   :v2 HttpClient$Version/HTTP_2})

;; TODO: Promote this namespace to a separate package.

(defn client
  [options]

  ()

  )

(defn request
  [options]
  )


(defn send!
  [req]
  )

(defn send-async!
  [req]
  )


(comment
  (clojure.reflect/reflect HttpClient$Version)

  (versions :v1.1)
  (versions :v2)

  )
