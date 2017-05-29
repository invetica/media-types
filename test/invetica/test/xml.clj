(ns invetica.test.xml
  (:require
   [clojure.data.xml :as xml]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Specs

(s/def ::tag
  keyword?)

(s/def ::attrs
  (s/map-of keyword? string?))

(s/def ::content
  (s/* ::node))

(s/def ::node
  (s/or :elem (s/keys :req-un [::tag ::attrs ::content])
        :str string?))

;; -----------------------------------------------------------------------------
;; Utils

(defn element?
  [x]
  (instance? clojure.data.xml.node.Element x))

(def children
  (mapcat :content))

(defn tagp
  [pred]
  (comp children (filter (comp pred :tag))))

(defn tag=
  [tag]
  (tagp #(= tag %)))

(defn tagname=
  [tag]
  (tagp #(some-> % name keyword (= tag))))

(defn attr-accessor
  [a]
  (comp a :attrs))

(defn attrp
  [a pred]
  (filter (comp pred (attr-accessor a))))

(defn attr=
  [a v]
  (attrp a #(= v %)))

(def text
  (comp (mapcat :content) (filter string?)))

(defn content->str
  [node]
  (some->> node :content (str/join "\n")))

;; -----------------------------------------------------------------------------
;; XML Records

(s/def :record/name
  string?)

(s/def :record/template
  (s/nilable string?))

(s/def ::record
  (s/keys :req [:record/name :record/template]))

(s/fdef record->map
  :args (s/cat :node ::node)
  :ret ::record)

(defn- record->map
  [{:keys [content] :as node}]
  (let [xs (filter element? content)]
    {:record/name (->> xs
                       (filter #(= :name (some-> % :tag name keyword)))
                       first
                       content->str)
     :record/template (->> xs
                           (filter #(= :file (some-> % :tag name keyword)))
                           (filter #(= "template" (get-in % [:attrs :type])))
                           first
                           content->str)}))

(defn doc->records
  [doc]
  (sequence (comp children
                  (tagname= :record)
                  (map record->map)
                  (filter :record/template))
            [doc]))
