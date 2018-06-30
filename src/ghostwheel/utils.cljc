;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.utils
  (:require [cljs.env]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]
            [cuerdas.core :as cs]
            #?@(:clj  [[clojure.core.specs.alpha]
                       [orchestra.spec.test :as ost]
                       [com.rpl.specter
                        :refer [setval transform select select-any
                                filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]]
                :cljs [[cljs.core.specs.alpha :include-macros true]
                       [orchestra-cljs.spec.test :as ost]
                       [com.rpl.specter
                        :refer-macros [setval transform select select-any]
                        :refer [filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]])))

(defn DBG [data]
  #?(:clj  (println data)
     :cljs (js/console.log data))
  data)

(defn cljs-env? [env] (boolean (:ns env)))

(defn get-ghostwheel-compiler-config [env]
  (if (cljs-env? env)
    (when cljs.env/*compiler*
      (let [compiler-config (or (get-in @cljs.env/*compiler* [:options :external-config :ghostwheel])
                                (get-in @cljs.env/*compiler* [:options :ghostwheel]))]
        (cond
          (map? compiler-config) (setval [MAP-KEYS NAMESPACE] (str `ghostwheel.core) compiler-config)
          (true? compiler-config) {}
          :else nil)))
    ;; TODO: Implement this properly for Clojure. At the moment
    ;; Ghostwheel is never off on Clojure - which is ok for now,
    ;; because we aren't doing any tracing, just s/fdef and it's
    ;; perfectly fine for that to end up in the production code
    {}))

;; This isn't necessary, strictly speaking, but it makes hacking on
;; Ghostwheel easier, because it allows env to be stubbed in order to
;; trace-debug code generating functions at runtime in ClojureScript
(defn get-ns-meta [env]
  (if (cljs-env? env)
    (some-> env :ns :meta)
    (meta *ns*)))

(defn get-ns-name [env]
  (if (cljs-env? env)
    (some-> env :ns :name)
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

;; Copy-pasted from Rosetta Code
(defn wrap-line [size text]
  (pprint/cl-format nil
                    (str "~{~<~%~1," size ":;~A~> ~}")
                    (cs/split text #" ")))
