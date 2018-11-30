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
            [clojure.string :as string]
            [ghostwheel.logging :as l
             :refer [log log-bold group group-collapsed group-end DBG]]))


(def *all-tests-successful (atom true))


(def wrap (partial l/wrap-line 80))


(def inc-report-counter! #?(:clj  t/inc-report-counter
                            :cljs t/inc-report-counter!))


(defmulti ^:dynamic report :type)


(defmethod report :begin-test-ns [m]
  (group (str "Checking " (:ns m) " ..."
           {::l/background (:base01 l/ghostwheel-colors)})))


(defmethod report :end-test-ns [m]
  (group-end))


(defmethod report :summary [m]
  (let [{:keys [fail error pass test warn]} m
        passed?   (= pass test)
        warnings? (some-> warn (> 0))
        color     (cond
                    (= test 0) (:black l/ghostwheel-colors)
                    passed? (:green l/ghostwheel-colors)
                    :else (:red l/ghostwheel-colors))
        label     (cond
                    (= test 0) "No active tests found"
                    passed? (str "Passed all " test " checks")
                    :else (str "Failed " fail " of " test " checks"
                               (when (> error 0)
                                 (str "; " error " test error(s)"))))]
    (do
      (group-end)
      (when (or (not passed?) warnings?)
        (log))
      (log label {::l/background color})
      (when warnings?
        (log (str warn " warning(s)")
             {::l/background (:orange0 l/ghostwheel-colors)}))
      ;; Might be overkill, but we want to make sure we reset the group nesting
      ;; in DevTools if anything should blow up above
      (dorun (repeatedly 5 l/group-end)))))


(defmethod report :pass [m]
  (let [{:keys [::ns-name ::fn-name ::fspec ::spec-checks ::check-coverage
                ::marked-unsafe ::plain-defns ::unchecked-defns
                ::unchecked-ns ::report-output]} (:message m)

        warning-style       {::l/background (:orange0 l/ghostwheel-colors)}
        incomplete-coverage " => Test coverage incomplete:"
        no-gen-testing      " => No generative testing performed"]
    (do
      (inc-report-counter! :pass)
      ;; REVIEW : We don't expect
      (when check-coverage
        (cond plain-defns
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              "Plain `defn` functions detected in "
                              ns-name
                              incomplete-coverage
                         warning-style))
                (log (mapv symbol plain-defns))
                (log-bold "=> Use `>defn` instead.")
                (group-end))

              unchecked-defns
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              "`::g/check` disabled for some functions in "
                              ns-name
                              incomplete-coverage
                         warning-style))
                (log (mapv symbol unchecked-defns))
                (group-end))

              unchecked-ns
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              "`::g/check` disabled for "
                              ns-name
                              incomplete-coverage
                         warning-style))
                (group-end))


              marked-unsafe
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              fn-name
                              " – Function marked as unsafe."
                              no-gen-testing
                              incomplete-coverage
                         warning-style))
                (group-end))

              (not fspec)
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              fn-name
                              " – Missing fspec(s)"
                              no-gen-testing
                              incomplete-coverage
                         warning-style))
                (group-end))

              (not spec-checks)
              (do
                (inc-report-counter! :warn)
                (group (str "WARNING: "
                              fn-name
                              " – Number of tests set to 0"
                              no-gen-testing
                              incomplete-coverage
                         warning-style))
                (group-end))

              :else nil)))))


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
  "\nPlease file an issue at https://github.com/gnl/ghostwheel/issues if you encounter false positives or negatives in side effect detection.")

(defn- report-unexpected-side-effects [message]
  (let [{:keys [::fn-name ::found-fx]} message]
    (log-bold "Possible side effects detected in function marked as safe:\n")
    (->> found-fx
         ;(map (fn [[type form details]] {:found type :at form :kind details}))
         (map (fn [[type form details]]
                (vec (concat [type]
                             (when form ['at form])
                             (when details ['kind details])))))
         (map log)
         (doall))
    (->> (str "=> Either remove the side effects, rename the function to '"
              (str (name fn-name) "!'")
              " to mark it as unsafe, or add ^::g/ignore-fx to its metadata to disable this warning and consider the function safe for automated generative testing.")
         wrap
         log)
    (log (wrap issue-msg))))


(defn- report-unexpected-safety [message]
  (let [safe-name (string/replace (name (::fn-name message)) #"!$" "")]
    (log-bold "No side effects detected in function marked as unsafe.")
    (->> (str "=> If safe, rename to '"
              safe-name
              "'. If unsafe, rename the called unsafe functions to suffix them with a '!', or add the ^::g/ignore-fx metadata to disable this check.")
         wrap
         log)
    (log (wrap issue-msg))
    log))


(defn- report-spec-check [{:keys [::spec-checks ::fn-name]}]
  (doseq [check spec-checks
          :let [test-ret (get check #?(:clj  :clojure.spec.test.check/ret
                                       :cljs :clojure.test.check/ret))]
          :when (not (:pass? test-ret))
          :let [spec-error (:result test-ret)
                data       (.-data spec-error)
                msg        (try (.-message spec-error)
                                (catch #?(:cljs js/Object :clj Exception) _ nil))]]
    (if-not data
      (log-bold msg)
      (do
        (when-let [args (::st/args data)]
          (log "\nCall:")
          (log (cons (with-meta fn-name nil) args)))
        (log)
        (when (= (::s/failure data) :instrument)
          (log (-> msg string/split-lines first (str "\n"))))
        (-> (#?(:cljs expound/printer-str :clj #'expound/printer-str) nil data)
            (str "\n")
            log)
        ;; REVIEW: Too noisy in the REPL, but
        ;; maybe add an option to enable it later
        (when (= l/*report-output* :js-console)
          (group-collapsed "Raw error data:" {::l/background (:base0 l/ghostwheel-colors)})
          (log msg)
          (log data)
          (group-end))))))


(defmethod report :fail [m]
  (let [message     (:message m)
        {:keys [::fn-name ::failure]} message
        summary     (case failure
                      ::unexpected-fx "Possible side effects detected"
                      ::unexpected-safety "Expected side effects not detected"
                      ::spec-failure "Spec check"
                      nil)
        start-group l/group]
    (inc-report-counter! :fail)
    (start-group (str "FAILED: " fn-name " – " summary)
                 {::l/background (:red l/ghostwheel-colors)})
    (case failure
      ::unexpected-fx (report-unexpected-side-effects message)
      ::unexpected-safety (report-unexpected-safety message)
      ::spec-failure (report-spec-check message)
      (do
        (log-bold (str "Undefined failure reason: " failure))
        (log message)))
    (group-end)))


;; REVIEW - test this and clean it up
(defmethod report :error [m]
  (DBG m)
  #_(let [[fn-name spec-check] (:message m)]
      (do
        (inc-report-counter! :error)
        (group (str "ERROR when testing " fn-name
                 {::l/background (:red l/ghostwheel-colors)}))
        (inc-report-counter! :error)
        (println "\nERROR in" (t/testing-vars-str m))
        #?(:cljs (when (seq (:testing-contexts (t/get-current-env)))
                   (println (t/testing-contexts-str))))
        (when-let [message (:message m)] (println message))
        #?(:cljs (t/print-comparison m))
        (group-end))))


(defmethod report :end-run-tests [m]
  (swap! *all-tests-successful #(and %1 %2) (t/successful? m)))


(defmethod report :default [_])

