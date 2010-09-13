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

(defn text-of-first [x,f] (xml1-> x f text))
  
(defn hashify [xmlel & exclusions]
  (let [exclusions (set exclusions)
        content (:content (first xmlel))]
    (apply hash-map
           (flatten
            (map
             (fn [x] (let [t (:tag x)]
                       (if (not (contains? exclusions t))
                         [(:tag x) (xml1-> xmlel t text)]
                         []
                         )))
             content)))))

(defn build-person [p]
  (merge
   {:email-address
    (xml1-> p :contact-data :email-addresses :email-address :address text)
    :phone-number
    (xml1-> p :contact-data :phone-numbers :phone-number :number text)
    :name
    (str (text-of-first p :first-name) " " (text-of-first p :last-name))}
   (hashify p :contact-data)))

(defn person [id]
  (build-person (resource "people" id)))

(defn build-company [c]
  (merge
   {:email-address
    (xml1-> c :contact-data :email-addresses :email-address :address text)
    :phone-number
    (xml1-> c :contact-data :phone-numbers :phone-number :number text)}
   (hashify c :contact-data)))

(defn company [id]
  (build-company (resource "companies" id)))

;; (defn people []
;;   (map build-person (:content (first (resource "people")))))

(defn runtest []
  (site "https://twitpay.highrisehq.com")
  (user "6b24c27dc4bd4cdb2027fe7f568fc27d")
  (person 45664714))

