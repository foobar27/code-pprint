(ns code-pprint.core-test
  (:require [clojure.test :as t]
            [code-pprint.core :refer :all]))

(defn- refer-private [ns]
  (doseq [[symbol var] (ns-interns ns)]
    (when (:private (meta var))
      (intern *ns* symbol var)))) 

(refer-private 'code-pprint.core)

;; Within the deftest macro, *ns* points to a different namespace,
;; so we have to name it specifically in these macros.
(def ^:private our-namespace 'code-pprint.core-test)

(t/deftest symbol-map-examples
  (let [symbol-map (ns->symbol-map our-namespace)]
    (t/are [input expected] 
        (= (get symbol-map input)
           expected)
      `map "map"
      `String "String")))

(t/deftest simple-qualifications-examples
  (let [ctx (ns->ns-context our-namespace)]
    (t/are [input expected] (= (simplify-qualification input ctx)
                               expected)
      `map (symbol "map")
      `clojure.core/unknown-symbol (symbol "clojure.core" "unknown-symbol")
      'unknown-symbol (symbol "unknown-symbol")
      `unknown-symbol (symbol "unknown-symbol")
      `unknown-ns/unknown-symbol (symbol "unknown-ns" "unknown-symbol")
      :unqualified (keyword "unqualified")
      ::a (keyword ":a")   ;; qualified in local namespace
      ::t/test (keyword "t" "test") ;; aliased namespace
      :clojure.test/test (keyword "t" "test")
      :unknown-ns/test (keyword "unknown-ns" "test")))) 



;; TODO write tests
(pprint `(defn foo# [x#] (str x# 42 map ::x :foo/bar))
        *ns*)

;; name clash
(pprint '(clojure.core/let [map 42] (clojure.core/map clojure.core/identity [1 2 3]))
        *ns*)


