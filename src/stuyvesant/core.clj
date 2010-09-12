(ns stuyvesant.core
  (:require [clj-http.client :as client]
            [clojure.contrib.java-utils :only (as-url) :as jutils]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.str-utils2 :as str2])
  (:use clojure.contrib.zip-filter.xml))

(defn valid-highrise-url? [candidate]
  (try
    (let [url (jutils/as-url candidate)
          protocol (. url getProtocol)]
      (if (and (re-find #"^http" protocol)
               (re-find #"highrisehq" candidate))
        true false))
    (catch java.net.MalformedURLException e false)))

(def *site* "")
(defn site
  "Set or query current site"
  ([] *site*)
  ([s] (if (valid-highrise-url? s)
         (def *site* s)
         (throw (IllegalArgumentException. "Not a valid Highrise URL")))))

(def *user* "")
(defn user
  "Set or query current user token"
  ([] *user*)
  ([s] (def *user* s)))

(defn can-connect? []
  (if (or
       (str2/blank? *site*)
       (str2/blank? *user*))
    true false))

(defn zip-str [s]
  (zip/xml-zip (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn authed-request [method path & [opts]]
  (client/request
   (merge {:method method :url (str *site* path) :basic-auth [*user* ""]} opts)))

(defn resource [name & rest]
  (let [[id method] rest
        method (or method :get)
        path (if id (str name "/" id) name)]
    (zip-str (:body (authed-request method (str "/" path ".xml"))))))

(defn person [id])

(defn runtest []
  (site "https://twitpay.highrisehq.com")
  (user "6b24c27dc4bd4cdb2027fe7f568fc27d")
  
  (xml-> (resource "people" 45664714) :first-name))