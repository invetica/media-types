(ns invetica.media
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen]
   [clojure.string :as str]))

;; This regular expression is a little nasty, and I'd quite like to do away with
;; it, but in the interest of getting something working quickly (without the
;; need to pull in Instaparse) I'm solving one problem by introducting another.
;;
;; > Some people, when confronted with a problem, think "I know, I'll use
;; > regular expressions." Now they have two problems.
(def ^:const ^:private subtype-pattern
  #"^vnd\.(.*)(?:\.v([^\+]+?))(?:\+(.*))")

;; -----------------------------------------------------------------------------
;; Specs

(defn- maybe
  [k]
  (sgen/one-of [(sgen/return nil) k]))

(s/def ::some-string
  (s/with-gen string?
    (fn [] (sgen/not-empty (sgen/string-alphanumeric)))))

(s/def ::toptype
  (s/with-gen ::some-string
    (fn []
      (sgen/one-of
       [(s/gen ::some-string)
        (sgen/elements #{"application"
                         "audio"
                         "image"
                         "text"
                         "video"})]))))

(s/def ::vendor
  ::some-string)

(s/def ::version
  (s/nilable ::some-string))

(s/def ::format
  (s/nilable ::some-string))

(s/def ::vendor-subtype
  (s/with-gen #(and (string? %) (re-matches subtype-pattern %))
    (fn []
      (sgen/fmap (fn [[a b c]] (str "vnd." a ".v" b "+" c))
                 (->> [::vendor ::version ::format]
                      (map s/gen)
                      (apply sgen/tuple))))))

(s/def ::subtype
  (s/or :vendor ::vendor-subtype
        :simple ::some-string))

(s/def ::params
  (s/map-of ::some-string ::some-string))

(s/def ::type
  (s/keys :req-un [::toptype
                   ::subtype]
          :opt-un [::params]))

(defn- string-present?
  [x]
  (and (string? x) (not (str/blank? x))))

(defn type?
  "Returns true when given a map that looks like a parsed media type string."
  [x]
  (and (map? x)
       (-> x :toptype string-present?)
       (-> x :subtype string-present?)
       (-> x :params map?)))

(defn join
  "Joins map `m` into a string."
  [m]
  {:pre [(s/valid? ::type m)]}
  (str (:toptype m)
       (when-let [s (:subtype m)]
         (str "/" s))
       (when-let [m (:params m)]
         (str "; "
              (->> m
                   (map (fn [[k v]] (str k "=" v)))
                   (str/join "; "))))))

(s/def ::type-string
  (s/with-gen string?
    (fn [] (sgen/fmap join (s/gen ::type)))))

;; -----------------------------------------------------------------------------
;; Parse

(s/fdef parse
  :args (s/cat :s ::type-string)
  :ret (s/nilable ::type))

(defn parse
  "Parse the given string `s` into a media type map. Requires the media type
  string to include at least \"/\"."
  [s]
  (when (str/includes? s "/")
    (let [[toptype tail] (str/split s #"/" 2)
          [subtype & raw-params] (str/split tail #"\s*;\s*")
          params (into (sorted-map)
                       (map #(str/split % #"=" 2))
                       raw-params)]
      {:params params
       :subtype subtype
       :toptype toptype})))

(s/fdef prettify
  :args (s/cat :m (s/nilable ::type))
  :ret (s/nilable ::type))

(defn prettify
  "Trims and tidies a parse result."
  [m]
  (let [trim-kv (juxt (comp str/trim key)
                      (comp str/trim val))]
    (some-> m
            (update :subtype str/trim)
            (update :toptype str/trim)
            (update :params #(into (sorted-map) (map trim-kv) %)))))

(def parse-pretty
  "Parses a media type string into a presentable, pretty map."
  (comp prettify parse))

(s/fdef only-type
  :args (s/cat :m any?)
  :ret (s/or :nil nil? :type ::type))

(defn only-type
  "Only returns `m` if it is a valid type map."
  [m]
  (when (type? m) m))

(def parse-strict
  "Parses a media type string, returning `nil` unless the we have a value that
  conforms to `type?`."
  (comp only-type parse))

;; -----------------------------------------------------------------------------
;; Opinionated guesswork

(s/def ::guessed-subtype
  (s/keys :req-un [::vendor ::version ::format]))

(s/fdef guess-subtype
  :args (s/cat :s (s/nilable string?))
  :ret (s/nilable ::guessed-subtype))

(defn guess-subtype
  "Given a subtype `s` attempts to extract the vendor name, version number, and
  format assuming a format of `vnd.<vendor-name>.<version>+<format>`.

  This is a very opinionated extension of media types sometimes used in HTTP
  APIs when you really love hypermedia.

  When subtype is of unknown format returns `nil` (because we don't have
  Maybe)."
  [s]
  (when (string? s)
    (some->> s
             (re-matches subtype-pattern)
             rest
             (zipmap [:vendor :version :format]))))
