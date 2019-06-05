;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.dev-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.gen.alpha :as gen]
            [ghostwheel.core :as g :refer [>defn >defn- >fdef => | <- ? |> tr]]
            [ghostwheel.test-utils :as tu
             :refer [process-fdef extract-fdef threading-test deftest-defn-variations
                     deftest-adhoc-trace-variations]]
            [ghostwheel.threading-macros :refer [*-> *->> *as-> *cond-> *cond->> *some-> *some->>]]
            #?@(:clj  [[clojure.test :as t :refer [deftest testing is]]
                       [orchestra.spec.test :as ost]
                       [ghostwheel.test-utils-clj :refer [expand]]]
                :cljs [[clojure.test :as t :refer-macros [deftest testing is]]
                       [orchestra-cljs.spec.test :as ost]
                       [ghostwheel.test-utils-cljs :refer-macros [expand]]
                       [ghostwheel.tracer]])))

;; TODO - test fx detection

(def arity-1-fdef
  '(cljs.spec.alpha/fdef arity-1
     :args (cljs.spec.alpha/and (cljs.spec.alpha/cat
                                 :arg1 (s/tuple neg-int? pos-int?)
                                 :cd (s/tuple nat-int? nat-int?)
                                 :vw (s/map-of keyword? pos-int?)
                                 :arg4 (s/map-of keyword? pos-int?)
                                 :an-atom** any?)
                                (fn [{[a b]                 :arg1,
                                      [c d :as cd]          :cd,
                                      {:keys [v w], :as vw} :vw,
                                      {:keys [x y]}         :arg4,
                                      an-atom**             :an-atom**}]
                                  (< a b))
                                (fn [{[a b]                 :arg1,
                                      [c d :as cd]          :cd,
                                      {:keys [v w], :as vw} :vw,
                                      {:keys [x y]}         :arg4,
                                      an-atom**             :an-atom**}]
                                  (> x y))
                                (fn [{[a b]                 :arg1,
                                      [c d :as cd]          :cd,
                                      {:keys [v w], :as vw} :vw,
                                      {:keys [x y]}         :arg4,
                                      an-atom**             :an-atom**}]
                                  (= (count cd) (count vw) 2)))
     :ret int?
     :fn (cljs.spec.alpha/and (fn [{{[a b]                 :arg1,
                                     [c d :as cd]          :cd,
                                     {:keys [v w], :as vw} :vw,
                                     {:keys [x y]}         :arg4,
                                     an-atom**             :an-atom**} :args,
                                    ret__1                             :ret}]
                                (< a ret__1))
                              (fn [{{[a b]                 :arg1,
                                     [c d :as cd]          :cd,
                                     {:keys [v w], :as vw} :vw,
                                     {:keys [x y]}         :arg4,
                                     an-atom**             :an-atom**} :args,
                                    ret__1                             :ret}]
                                (> ret__1 (- a x))))))

(deftest-defn-variations arity-1
  {::tu/args-ret-mappings {[[-2 2] [0 10] {:v 10 :w 30} {:x 40 :y 10} (atom 2)] 100}
   ::tu/expected-fdef     arity-1-fdef}
  [[a b] [c d :as cd] {:keys [v w] :as vw} {:keys [x y]} an-atom**]
  [(s/tuple neg-int? pos-int?)
   (s/tuple nat-int? nat-int?)
   (s/map-of keyword? pos-int?)
   (s/map-of keyword? pos-int?)
   any?
   | #(< a b) #(> x y) #(= (count cd) (count vw) 2)
   => int? | #(< a %) #(> % (- a x))]
  (swap! an-atom** - 2)
  (let [alpha a
        bravo (->> b inc dec dec inc)
        up    #(let [num %] (inc num))]
    (let [nukyular** (atom nil)
          down       #(let [num %] (dec num))]
      (reset! nukyular** alpha)
      (apply + @an-atom** (down @nukyular**) (up bravo) v w x y cd))))

(deftest >fdef-arity-1-test
  (let [fdef (process-fdef
              (expand
               (>fdef arity-1
                 [[a b] [c d :as cd] {:keys [v w] :as vw} {:keys [x y]} an-atom**]
                 [(s/tuple neg-int? pos-int?)
                  (s/tuple nat-int? nat-int?)
                  (s/map-of keyword? pos-int?)
                  (s/map-of keyword? pos-int?)
                  any?
                  | #(< a b) #(> x y) #(= (count cd) (count vw) 2)
                  => int? | #(< a %) #(> % (- a x))])))]
    (is (= fdef arity-1-fdef))))

(deftest >fdef-nested-fspec-test
  (let [nested-fspec-fdef
        '(cljs.spec.alpha/fdef nested-fspec
           :args (cljs.spec.alpha/cat
                  :f (cljs.spec.alpha/fspec :args (cljs.spec.alpha/cat :arg1 nat-int? :arg2 string?) :ret string?)
                  :coll (? seqable?))
           :ret seq?)

        fdef
        (process-fdef
         (expand
          (>fdef nested-fspec
            [f coll]
            [[nat-int? string? => string?] (? seqable?) => seq?])))]
    (is (= fdef nested-fspec-fdef))))

(deftest >fdef-nested-nilable-fspec-test
  (let [nested-fspec-nilable-fdef
        '(cljs.spec.alpha/fdef nested-nilable-fspec
           :args (cljs.spec.alpha/cat
                  :f (cljs.spec.alpha/nilable (cljs.spec.alpha/fspec :args (cljs.spec.alpha/cat :arg1 nat-int? :arg2 string?) :ret string?))
                  :coll (? seqable?))
           :ret seq?)

        fdef
        (process-fdef
         (expand
          (>fdef nested-nilable-fspec
            [f coll]
            [[? [nat-int? string? => string?]] (? seqable?) => seq?])))]
    (is (= fdef nested-fspec-nilable-fdef))))

(deftest >fdef-nested-any-fspec-test
  (let [nested-any-fspec-fdef
        '(cljs.spec.alpha/fdef nested-any-fspec
           :args (cljs.spec.alpha/cat
                  :f ifn?
                  :coll (? seqable?))
           :ret seq?)

        fdef
        (process-fdef
         (expand
          (>fdef nested-any-fspec
            [f coll]
            [[nat-int? any? => any?] (? seqable?) => seq?])))]
    (is (= fdef nested-any-fspec-fdef))))

(deftest >fdef-keyword-test
  (let [keyword-fdef
        '(cljs.spec.alpha/def :ghostwheel.dev-test/fdef-keyword
           (cljs.spec.alpha/fspec :args (cljs.spec.alpha/cat :a int?) :ret int?))

        fdef
        (process-fdef
         (expand
          (>fdef ::fdef-keyword
            [a]
            [int? => int?])))]
    (is (= fdef keyword-fdef))))

(deftest >fdef-empty-arg-test
  (let [empty-arg-fdef
        '(cljs.spec.alpha/fdef empty-arg-fdef :args (cljs.spec.alpha/cat) :ret int?)

        fdef
        (process-fdef
         (expand
          (>fdef empty-arg-fdef
            []
            [=> int?])))]
    (is (= fdef empty-arg-fdef))))

(deftest arity-1-nilspec-test
  (>defn arity-1-nilspec
    [a]
    nil
    (inc a))
  ;; TODO check that no fdef is defined here
  (is (= (arity-1-nilspec 3) 4)))

(def arity-n-fdef-multiret
  '(cljs.spec.alpha/fdef arity-n
     :args (cljs.spec.alpha/or
            :arity-1 (cljs.spec.alpha/cat :a int?)
            :arity-2 (cljs.spec.alpha/and
                      (cljs.spec.alpha/cat :a nat-int? :b pos-int?)
                      (fn [{:keys [a b]}] (< a b)))
            :arity-n (cljs.spec.alpha/and
                      (cljs.spec.alpha/cat :a nat-int? :b pos-int? :c nat-int? :more (s/* int?))
                      (fn [{:keys [a b c more]}] (< a b))
                      (fn [{:keys [a b c more]}] (< a b c))))
     :fn (cljs.spec.alpha/and
          (fn valid-multi-arity-ret?
            [p1__1]
            (case (-> p1__1 :args key)
              :arity-1 (cljs.spec.alpha/valid? string? (:ret p1__1))
              :arity-2 (cljs.spec.alpha/valid? int? (:ret p1__1))
              :arity-n (cljs.spec.alpha/valid? int? (:ret p1__1))))
          (fn valid-multi-arity-fn?
            [p1__1]
            (case (-> p1__1 :args key)
              :arity-1 true
              :arity-2 (cljs.spec.alpha/valid?
                        (fn [{[_ {:keys [a b]}] :args, ret__1 :ret}] (> ret__1 a))
                        p1__1)
              :arity-n (cljs.spec.alpha/valid?
                        (cljs.spec.alpha/and
                         (fn [{[_ {:keys [a b c more]}] :args, ret__1 :ret}] (> ret__1 a))
                         (fn [{[_ {:keys [a b c more]}] :args, ret__1 :ret}] (> ret__1 (+ a c))))
                        p1__1))))))

(def arity-n-fdef-uniret
  '(cljs.spec.alpha/fdef arity-n
     :args (cljs.spec.alpha/or
            :arity-1 (cljs.spec.alpha/cat :a int?)
            :arity-2 (cljs.spec.alpha/and
                      (cljs.spec.alpha/cat :a nat-int? :b pos-int?)
                      (fn [{:keys [a b]}] (< a b)))
            :arity-n (cljs.spec.alpha/and
                      (cljs.spec.alpha/cat :a nat-int? :b pos-int? :c nat-int? :arg4 (s/* any?))
                      (fn [{a :a, b :b, c :c, [nukyular**] :arg4}] (< a b))
                      (fn [{a :a, b :b, c :c, [nukyular**] :arg4}] (< a b c))))
     :ret int?
     :fn (fn valid-multi-arity-fn? [p1__1]
           (case (-> p1__1 :args key)
             :arity-1 true
             :arity-2 (cljs.spec.alpha/valid?
                       (fn [{[_ {:keys [a b]}] :args, ret__1 :ret}] (> ret__1 a))
                       p1__1)
             :arity-n (cljs.spec.alpha/valid?
                       (cljs.spec.alpha/and
                        (fn [{[_ {a :a, b :b, c :c, [nukyular**] :arg4}] :args, ret__1 :ret}]
                          (> ret__1 a))
                        (fn [{[_ {a :a, b :b, c :c, [nukyular**] :arg4}] :args, ret__1 :ret}]
                          (> ret__1 (+ a c))))
                       p1__1)))))

(deftest-defn-variations arity-n-multiret
  {::tu/args-ret-mappings {[3]     "4"
                           [3 5]   8
                           [3 5 7] 15}
   ::tu/expected-fdef     arity-n-fdef-multiret}
  ([a]
   [int? => string?]
   (str (inc a)))
  ([a b]
   [nat-int? pos-int? | #(< a b)
    => int? | #(> % a)]
   (+ a b))
  ([a b c & more]
   [nat-int? pos-int? nat-int? (s/* int?) | #(< a b) #(< a b c)
    => int? | #(> % a) #(> % (+ a c))]
   (apply + a b c more)))

(deftest-defn-variations arity-n-uniret
  {::tu/args-ret-mappings {[3]              4
                           [3 5]            8
                           [3 5 7 (atom 3)] 15}
   ::tu/expected-fdef     arity-n-fdef-uniret}
  ([a]
   [int? => int?]
   (inc a))
  ([a b]
   [nat-int? pos-int? | #(< a b)
    => int? | #(> % a)]
   (+ a b))
  ([a b c & [nukyular**]]
   [nat-int? pos-int? nat-int? (s/* any?) | #(< a b) #(< a b c)
    => int? | #(> % a) #(> % (+ a c))]
   (swap! nukyular** - 3)
   (+ a b c @nukyular**)))

(deftest arity-n-nilspec-test
  (>defn arity-n-nilspec
    ([a]
     nil
     (inc a))
    ([a b]
     nil
     (+ a b)))
  ;; TODO check that no fdef is defined here
  (is (= (arity-n-nilspec 3) 4))
  (is (= (arity-n-nilspec 3 5) 8)))

(deftest >fdef-arity-n-uniret-test
  (let [fdef (process-fdef
              (expand
               (>fdef arity-n
                 ([a]
                  [int? => int?]
                  (inc a))
                 ([a b]
                  [nat-int? pos-int? | #(< a b)
                   => int? | #(> % a)]
                  (+ a b))
                 ([a b c & [nukyular**]]
                  [nat-int? pos-int? nat-int? (s/* any?) | #(< a b) #(< a b c)
                   => int? | #(> % a) #(> % (+ a c))]))))]
    (is (= fdef arity-n-fdef-uniret))))

(deftest >fdef-arity-n-multiret-test
  (let [fdef (process-fdef
              (expand
               (>fdef arity-n
                 ([a]
                  [int? => string?])
                 ([a b]
                  [nat-int? pos-int? | #(< a b)
                   => int? | #(> % a)])
                 ([a b c & more]
                  [nat-int? pos-int? nat-int? (s/* int?) | #(< a b) #(< a b c)
                   => int? | #(> % a) #(> % (+ a c))]))))]
    (is (= fdef arity-n-fdef-multiret))))


(>defn arity-1-stub
  [a b]
  [string? boolean? => int?])

(deftest arity-1-stub-test
  (is (binding [ghostwheel.logging/*report-output* nil]
        (int? (arity-1-stub "abc" true)))))


(>defn arity-n-stub
  ([a]
   [string? => int?])
  ([a b]
   [string? boolean? => int?]))

(deftest arity-n-stub-test
  (is (binding [ghostwheel.logging/*report-output* nil]
        (int? (arity-n-stub "abc"))))
  (is (binding [ghostwheel.logging/*report-output* nil]
        (int? (arity-n-stub "abc" false)))))


(deftest-adhoc-trace-variations trace-let-form
  (let [a 1
        b 2
        c [3 4 5]]
    (apply + a b c)))


(deftest-adhoc-trace-variations generic-trace-call
  (apply str
         (conj [1 2 3] 4)
         (drop 1 [4 5 6])
         (assoc {:a 1 :b 2} :c 3)
         (set [1 2 3])
         "asdf"
         1234
         true
         :bla
         nil
         nil))


(deftest-adhoc-trace-variations generic-trace-nil nil)
(deftest-adhoc-trace-variations generic-trace-string "asdf")
(deftest-adhoc-trace-variations generic-trace-number 1234)
(deftest-adhoc-trace-variations generic-trace-boolean true)
(deftest-adhoc-trace-variations generic-trace-map {:a 123 :b 456})
(deftest-adhoc-trace-variations generic-trace-vector [:a 1 2 3])
(deftest-adhoc-trace-variations generic-trace-set #{:a 1 2 3})


(deftest *->-test
  (is (threading-test -> *->
                      {:a [1 2 3] :b 456}
                      (assoc :c 234)
                      (dissoc :b)
                      (get :a)
                      (concat [4 5 6])
                      (nth 3)
                      (+ 1 2)
                      inc
                      (+ 2)
                      (/ 4))))


(deftest *->>-test
  (is (threading-test ->> *->>
                      [1 2 3 5 6]
                      (map inc)
                      (drop 1)
                      second
                      (+ 1 2)
                      inc
                      (+ 2)
                      (/ 4))))


(deftest *as->-test
  (is (threading-test as-> *as->
                      {:a [1 2 3]} x
                      (get x :a)
                      (apply * x)
                      (* 3 x)
                      (inc x)
                      (/ x 4))))


(deftest *cond->-test
  (is (threading-test cond-> *cond->
                      (+ 1 2)
                      (> 1 2) inc
                      (< 0 10) inc
                      false dec
                      true (+ 2)
                      true (/ 4))))


(deftest *cond->>-test
  (is (threading-test cond->> *cond->>
                      (+ 1 2)
                      (> 1 2) inc
                      (< 0 10) inc
                      false dec
                      true (+ 2)
                      true (/ 4))))


(deftest *some->-nil-test
  (is (threading-test some-> *some->
                      {:a 123 :b 456}
                      (dissoc :b)
                      :b
                      inc
                      inc)))


(deftest *some->-test
  (is (threading-test some-> *some->
                      {:a 123 :b 456}
                      :b
                      inc
                      inc)))


(deftest *some->>-nil-test
  (is (threading-test some->> *some->>
                      :c
                      (conj [:a :b])
                      (remove #{:b})
                      (some #{:b})
                      (conj [1 2 3]))))


(deftest *some->>-test
  (is (threading-test some->> *some->>
                      :c
                      (conj [:a :b])
                      (some #{:b})
                      (conj [1 2 3]))))
