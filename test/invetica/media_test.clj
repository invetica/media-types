(ns invetica.media-test
  (:require
   [clojure.data.xml :as xml]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [invetica.media :as sut]
   [invetica.test.spec :as test.spec]
   [invetica.test.xml :as test.xml]))

(use-fixtures :once test.spec/instrument)

(deftest t-is-well-specified
  (test.spec/is-well-specified 'invetica.media))

;; -----------------------------------------------------------------------------
;; Explicit media types

(deftest t-parse
  (are [in out] (= out (sut/parse in))
    "/"   {:params {} :subtype "" :toptype ""}
    "/;;" {:params {} :subtype nil :toptype ""}
    "/a"  {:params {} :subtype "a" :toptype ""}
    "a/"  {:params {} :subtype "" :toptype "a"}

    "text/plain"
    {:params {}
     :subtype "plain"
     :toptype "text"}

    "text/javascript; charset=utf-8"
    {:params {"charset" "utf-8"}
     :subtype "javascript"
     :toptype "text"}

    "  text/ javascript  ; charset = utf-8   ; foo= bar ;"
    {:params {"charset " " utf-8"
              "foo" " bar"}
     :subtype " javascript"
     :toptype "  text"}

    (str "application/vnd.invetica.v1+msgpack; "
         "charset=utf-8 ; schema=http://foo.com/ ;")
    {:params {"charset" "utf-8"
              "schema" "http://foo.com/"}
     :subtype "vnd.invetica.v1+msgpack"
     :toptype "application"}))

(deftest t-parse-pretty
  (is (= {:params {"charset" "utf-8"
                   "foo" "bar"}
          :subtype "javascript"
          :toptype "text"}
         (sut/parse-pretty
          "  text/ javascript  ; charset = utf-8   ; foo= bar ;"))))

(deftest t-parse-strict
  (are [in] (nil? (sut/parse-strict in))
    " "
    ""
    "/"
    "//"
    "/;;"))

;; -----------------------------------------------------------------------------
;; Guesswork

(deftest t-guess-subtype
  (are [in out] (= out (select-keys (sut/guess-subtype in) (keys out)))
    "vnd.invetica.v1+json"
    {:format "json"
     :vendor "invetica"
     :version "1"}
    "vnd.uk.co.invetica.v1+json"
    {:format "json"
     :vendor "uk.co.invetica"
     :version "1"}
    "vnd.example.v1.448-af382feec+msgpack-3.0"
    {:format "msgpack-3.0"
     :vendor "example"
     :version "1.448-af382feec"}))

;; -----------------------------------------------------------------------------
;; Join

(deftest t-join
  (is (= (str "application/vnd.invetica.v1+msgpack; "
              "charset=utf-8; schema=http://foo.com/")
         (sut/join {:encoding "msgpack"
                    :params {"charset" "utf-8"
                             "schema" "http://foo.com/"}
                    :subtype "vnd.invetica.v1+msgpack"
                    :toptype "application"}))))

(defspec t-join-roundtrip
  (prop/for-all [m (s/gen ::sut/type)]
    (= m (select-keys (sut/parse (sut/join m)) (keys m)))))

;; -----------------------------------------------------------------------------
;; Registered types

(deftest t-registered-media-types
  (with-open [rdr (io/reader (io/resource "invetica/media-types.xml"))]
    (let [doc (xml/parse rdr)]
      (doseq [record (test.xml/doc->records doc)]
        (is (sut/type? (sut/parse (:record/template record)))
            (format "Expected template %s of type %s to parse"
                    (:record/template record)
                    (:record/name record)))))))
