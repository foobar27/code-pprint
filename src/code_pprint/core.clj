(ns code-pprint.core
  (:require [riddley.walk :refer [walk-exprs]]
            [zprint.core :as zp]))

(defn- cast-to-symbol [v]
  (cond
    (var? v) (let [n (.ns v)
                   v (.sym v)]
               (symbol (str n) (str v)))
    (symbol? v) v
    (instance? java.lang.Class v) (symbol (.getName v))))

(defn- ns->symbol-map [n]
  (into {} (for [[k v] (ns-map n)]
             [(cast-to-symbol v) (str k)])))

(defn- ns->alias-map [n]
  (into {}
        (for [[s n] (ns-aliases n)]
          [(str n) (str s)])))

(defn- ns->ns-context [n]
  {:namespace (name (ns-name n))
   :alias-map (ns->alias-map n)
   :symbol-map (ns->symbol-map n)})

(defn- simplify-qualification [x ns-context]
  (letfn [(same-namespace? [x]
            (let [n (namespace x)]
              (or (nil? n)
                  (= n (:namespace ns-context)))))
          (get-prefix [x]
            (get-in ns-context [:alias-map (namespace x)] (namespace x)))]
    (cond
      ;; TODO unify cases of kewyword and symbol
      (qualified-keyword? x) (if (same-namespace? x)
                               ;; try it out yourself - the following line has the same
                               ;; string representation as a locally qualified keyword
                               (keyword nil (str ":" (name x)))
                               (keyword (get-prefix x) (name x)))
      ;; TODO needed?
      (keyword? x) x
      (symbol? x) (or (some-> (get-in ns-context [:symbol-map x])
                              name
                              symbol)
                      (if (same-namespace? x)
                        (symbol nil (name x))
                        (symbol (get-prefix x) (name x)))))))

;; TOOD allow to pass all the fancy options from zprint
(defn pprint [form ns]
  (let [ns-context (ns->ns-context ns)]
    ;; TODO verify symbol not in scope
    (zp/zprint(walk-exprs (fn [form] (or (symbol? form) (keyword? form)))
                          (fn [form] (simplify-qualification form ns-context))
                          form)
              {:style :community})))


(zp/zprint-str `(defn foo# [x#] (str x# 42 ::x))
               {:map {:lift-ns-in-code? true}
                :style :community})

(zp/zprint-str `(defn foo# [x#] (str x# 42))
               {:style :community})
