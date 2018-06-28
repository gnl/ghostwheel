;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.reporting
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.test :as t]
            [expound.alpha :as expound]
            [cuerdas.core :as cs]
            [ghostwheel.utils :as u :refer [DBG]]
            [ghostwheel.tracing :as tr :refer [ghostwheel-colors get-styled-label pr-clog log-bold log]]))

(def all-tests-successful** (atom true))

(def wrap (partial u/wrap-line 80))

(defmethod t/report [::default :begin-test-ns] [m]
  #?(:cljs (apply js/console.group
                  (get-styled-label (str "Checking " (:ns m) " ...")
                                    {::tr/background (:base01 ghostwheel-colors)}))))

(defmethod t/report [::default :summary] [m]
  #?(:cljs (let [{:keys [fail error pass test warn]} m
                 passed?   (= pass test)
                 warnings? (> warn 0)
                 color     (cond
                             (= test 0) (:black ghostwheel-colors)
                             passed? (:green ghostwheel-colors)
                             :else (:red ghostwheel-colors))
                 label     (cond
                             (= test 0) "No active tests found"
                             passed? (str "Passed all " test " checks")
                             :else (str "Failed " fail " of " test " checks"
                                        (when (> error 0)
                                          (str "; " error " test error(s)"))))]
             (do
               (when (or (not passed?) warnings?)
                 (js/console.log ""))
               (apply js/console.log (get-styled-label label {::tr/background color}))
               (when warnings?
                 (apply js/console.log
                        (get-styled-label (str warn " warning(s)")
                                          {::tr/background (:orange0 ghostwheel-colors)})))
               ;; Might be overkill, but we want to make sure we reset the group nesting
               ;; in DevTools if anything should blow up above
               (dorun (repeatedly 5 js/console.groupEnd))))))

(defmethod t/report [::default :pass] [m]
  #?(:cljs (let [{:keys [::fn-name ::fspec ::spec-checks ::check-coverage ::marked-unsafe ::plain-defns]}
                 (:message m)
                 warning-style {::tr/background (:orange0 ghostwheel-colors)}]

             (do
               (t/inc-report-counter! :pass)
               ;; REVIEW : We don't expect
               (when (and check-coverage (not marked-unsafe))
                 (cond plain-defns
                       (do
                         (t/inc-report-counter! :warn)
                         (apply js/console.group
                                (get-styled-label
                                 (str "WARNING: Namespace contains functions defined using plain `defn` => Ghostwheel coverage incomplete.")
                                 warning-style))
                         (js/console.log plain-defns)
                         (log-bold "=> Use `>defn` instead.")
                         (js/console.groupEnd))

                       (not fspec)
                       (do
                         (t/inc-report-counter! :warn)
                         (apply js/console.log
                                (get-styled-label
                                 (str "WARNING: "
                                      fn-name
                                      " - Missing fspec(s) => Test coverage incomplete.")
                                 warning-style)))

                       (not spec-checks)
                       (do
                         (t/inc-report-counter! :warn)
                         (apply js/console.log
                                (get-styled-label
                                 (str "WARNING: "
                                      fn-name
                                      " - [:num-tests 0] => Generative testing disabled => Test coverage incomplete.")
                                 warning-style)))

                       :else nil))))))

;; REVIEW: We don't seem to be needing this anymore.
(defn- explain-problem-str [failure-type problem]
  (let [{:keys [pred val in path via]} problem]
    (case failure-type
      :check-failed (into {:val val :in in :not pred}
                          [(when (not= path [:ret]) [:at path])
                           (when (seq via) [:via via])])
      :instrument (into {:val val :in in :not pred :at path}
                        [(when (seq via) [:via via])])
      :else problem)))

(def ^:private issue-msg
  (str "\n"
       (cs/clean
        "Please file an issue at https://github.com/gnl/ghostwheel/issues if
        you encounter false positives or negatives in side effect detection.")))

(defn- report-unexpected-side-effects [message]
  #?(:cljs (let [{:keys [::fn-name ::found-fx]} message]
             (log-bold "Possible side effects detected in function marked as safe:\n")
             (->> found-fx
                  ;(map (fn [[type form details]] {:found type :at form :kind details}))
                  (map (fn [[type form details]]
                         (vec (concat [type]
                                      (when form ['at form])
                                      (when details ['kind details])))))
                  (map js/console.log)
                  (doall))
             (->> (str "=> Either remove the side effects, rename the function to '"
                       (str (name fn-name) "!'")
                       " to mark it as unsafe, or add ^::g/ignore-fx to its
                       metadata to disable this warning and consider the
                       function safe for automated generative testing.")
                  cs/clean
                  wrap
                  log)
             (log (wrap issue-msg)))))

(defn- report-unexpected-safety [message]
  (let [safe-name (cs/strip-suffix (name (::fn-name message)) "!")]
    (log-bold "No side effects detected in function marked as unsafe.")
    (->> (str "=> If safe, rename to '"
              safe-name
              "'. If unsafe, rename the called unsafe
              functions to suffix them with a '!', or add the
              ^::g/ignore-fx metadata to disable this check.")
         cs/clean
         wrap
         log)
    (log (wrap issue-msg))
    log))

(defn- report-spec-check [{:keys [::spec-checks ::fn-name]}]
  #?(:cljs (doseq [check spec-checks
                   :let [test-ret (:clojure.test.check/ret check)]
                   :when (not (:result test-ret))
                   :let [spec-error (-> test-ret
                                        :result-data
                                        :clojure.test.check.properties/error)
                         data       (.-data spec-error)
                         msg        (.-message spec-error)]]
             (if-not data
               (log-bold msg)
               (do
                 (when-let [args (::st/args data)]
                   (log "Call:" (cons fn-name args)))
                 (log)
                 (when (= (::s/failure data) :instrument)
                   (js/console.log (-> msg cs/lines first (str "\n"))))
                 (-> (expound/printer-str nil data) (str "\n") js/console.log)
                 (->> (get-styled-label "Raw error data:"
                                        {::tr/background (:base0 ghostwheel-colors)})
                      (apply js/console.groupCollapsed))
                 (js/console.log msg)
                 (js/console.log data)
                 (js/console.groupEnd))))))

(defmethod t/report [::default :fail] [m]
  #?(:cljs (let [message     (:message m)
                 {:keys [::fn-name ::failure]} message
                 summary     (case failure
                               ::unexpected-fx "Possible side effects detected"
                               ::unexpected-safety "Expected side effects not detected"
                               ::spec-failure "Spec check"
                               nil)
                 start-group js/console.group]
             (t/inc-report-counter! :fail)
             (apply start-group (get-styled-label (str "FAILED: " fn-name " - " summary)
                                                  {::tr/background (:red ghostwheel-colors)}))
             (case failure
               ::unexpected-fx (report-unexpected-side-effects message)
               ::unexpected-safety (report-unexpected-safety message)
               ::spec-failure (report-spec-check message)
               (do
                 (log-bold (str "Undefined failure reason: " failure))
                 (js/console.log message)))
             (js/console.groupEnd))))

;; NOTE - test this and clean it up
(defmethod t/report [::default :error] [m]
  #?(:cljs (let [[fn-name spec-check] (:message m)]
             (do
               (t/inc-report-counter! :error)
               (apply js/console.group
                      (get-styled-label (str "ERROR when testing " fn-name)
                                        {::tr/background (:red ghostwheel-colors)}))
               (t/inc-report-counter! :error)
               (println "\nERROR in" (t/testing-vars-str m))
               (when (seq (:testing-contexts (t/get-current-env)))
                 (println (t/testing-contexts-str)))
               (when-let [message (:message m)] (println message))
               (t/print-comparison m)
               (js/console.groupEnd)))))

(defmethod t/report [::default :end-run-tests] [m]
  (swap! all-tests-successful** #(and %1 %2) (t/successful? m)))
