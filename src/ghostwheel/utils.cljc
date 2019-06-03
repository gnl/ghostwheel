;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^:no-doc ghostwheel.utils
  #?(:cljs (:require-macros ghostwheel.utils))
  (:require [clojure.walk :as walk]))


(defn cljs-env? [env] (boolean (:ns env)))


(defn get-ns-meta [env]
  (if (cljs-env? env)
    (or (meta *ns*) (some-> env :ns :meta))
    (meta *ns*)))

(defn get-ns-name [env]
  (if (cljs-env? env)
    (or (.-name *ns*) (some-> env :ns :name))
    (.-name *ns*)))

(defn clj->cljs
  ([form]
   (clj->cljs form true))
  ([form strip-core-ns]
   (let [ns-replacements   (cond-> {"clojure.core"            "cljs.core"
                                    "clojure.test"            "cljs.test"
                                    "clojure.spec.alpha"      "cljs.spec.alpha"
                                    "clojure.spec.test.alpha" "cljs.spec.test.alpha"
                                    "orchestra.spec.test"     "orchestra-cljs.spec.test"
                                    "clojure.spec.gen.alpha"  "cljs.spec.gen.alpha"}
                                   strip-core-ns (merge {"clojure.core" nil
                                                         "cljs.core"    nil}))
         replace-namespace #(if-not (qualified-symbol? %)
                              %
                              (let [nspace (namespace %)]
                                (if (contains? ns-replacements nspace)
                                  (symbol (get ns-replacements nspace) (name %))
                                  %)))]
     (walk/postwalk replace-namespace form))))


(defn gen-exception [env msg]
  `(throw (~(if (cljs-env? env) 'js/Error. 'Exception.) ~msg)))


(defn devtools-config-override
  []
  `(let [current-config# (~'devtools.prefs/get-prefs)
         overrides#      {:max-print-level                                    4
                          :min-expandable-sequable-count-for-well-known-types 2}
         left-adjust#    (str "margin-left: -17px;")]
     (merge current-config#
            (into overrides# (for [k# [:header-style]
                                   :let [v# (get current-config# k#)]]
                               [k# (str v# left-adjust#)])))))

