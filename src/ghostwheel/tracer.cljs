(ns ghostwheel.tracer
  (:require [clojure.walk :refer [prewalk walk]]
            [clojure.pprint :as pprint]
            [clairvoyant.core
             :refer [ITraceEnter ITraceError ITraceExit]
             :include-macros true]
            [ghostwheel.logging :as l]
            [clojure.string :as string]))

(def *inside-let (atom false))

(defn tracer
  "Custom tracer for Clairvoyant used by Ghostwheel but not dependent on it.

  Parameters:
  :color - string, Example: \"#aabbcc\"
  :background - same as color
  :prefix - string tag to display before the traced op heading
  :suffix - ...
  :expand - set of op symbols to display expanded by default. Use :bindings to expand all bindings.
  Example: #{'defn 'let :bindings}"
  [& {:keys [color background prefix suffix expand] :as options}]
  (let [binding-group (if (contains? expand :bindings)
                        l/group
                        l/group-collapsed)
        has-bindings? l/ops-with-bindings
        fn-like?      (disj has-bindings? 'let `let)]
    (reify
      ITraceEnter
      (-trace-enter
        [_ {:keys [anonymous? arglist args dispatch-val form init name ns op protocol]}]
        (let [init        (if (and (seq? init)
                                   (= (first init) 'try)
                                   (seq? (second init))
                                   (-> (second init)
                                       first
                                       str
                                       (string/starts-with? "ghostwheel.threading-macros/*")))
                            nil
                            init)
              op-sym      (symbol (cljs.core/name op))
              unnamed-fn? (and (#{'fn 'fn*} op-sym)
                               (string/starts-with? (str name) "fn_"))
              group       (if (contains? expand op-sym)
                            (if unnamed-fn?
                              l/group-collapsed
                              l/group)
                            l/group-collapsed)
              anon-prefix "__anon_"]
          (cond
            (fn-like? op)
            (let [title (if protocol
                          (str protocol " " name " " arglist)
                          (str (when prefix (str prefix " – "))
                               ns "/" (when (and anonymous?
                                                 (or unnamed-fn?
                                                     (not (string/starts-with? (str name) anon-prefix))))
                                        anon-prefix)
                               name
                               (when dispatch-val
                                 (str " " (pr-str dispatch-val)))
                               (str " " arglist)))]
              (group title
                     {::l/background background
                      ::l/foreground color
                      ::l/weight     "bold"}
                     80
                     suffix)
              (l/group "bindings"))

            (#{'let `let} op)
            (do
              (reset! *inside-let true)
              (group (str op))
              (l/group "bindings"))

            (#{'binding} op)
            #_(binding-group (str form) nil nil init)
            (let [max-length 80
                  init       (when @*inside-let
                               (l/truncate-string (str init) max-length))]
              (binding-group (str form) nil nil init)
              #_(when (> (count label) max-length)
                  (l/group-collapsed "...")
                  (l/log (with-out-str (pprint/pprint label)))
                  (l/group-end))))))

      ITraceExit
      (-trace-exit [_ {:keys [op exit]}]
        (cond
          (#{'binding} op)
          (do (l/log-exit exit)
              (l/group-end))

          (has-bindings? op)
          (do
            (when (#{'let `let} op)
              (reset! *inside-let false))
            (l/group-end)
            (l/log-exit exit)
            (l/group-end))))

      ITraceError
      (-trace-error [_ {:keys [op form error ex-data]}]
        (cond
          (#{'binding} op)
          (do
            (l/error (.-stack error))
            (when ex-data
              (l/group-collapsed "ex-data")
              (l/group-collapsed ex-data)
              (l/group-end)
              (l/group-end)))

          (has-bindings? op)
          (do (l/group-end)
              (do
                (l/error (.-stack error))
                (when ex-data
                  (l/group-collapsed "ex-data")
                  (l/group-collapsed ex-data)
                  (l/group-end)
                  (l/group-end)))
              (l/group-end)))))))
