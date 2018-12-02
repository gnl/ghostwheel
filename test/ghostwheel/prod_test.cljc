;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.prod-test
  (:require [ghostwheel.threading-macros :refer [*-> *->> *as-> *cond-> *cond->> *some-> *some->>]]
            [ghostwheel.core :as g :refer [=> | <- >defn >defn- >fdef ?]]
            [ghostwheel.utils]
            [ghostwheel.test-utils :refer [threading-expansion-test]]
            #?@(:clj  [[clojure.test :as t :refer [deftest testing is]]
                       [ghostwheel.test-utils-clj :refer [expand expand-full]]]
                :cljs [[clojure.test :as t :refer-macros [deftest testing is]]
                       [ghostwheel.test-utils-cljs :refer-macros [expand expand-full]]])))


(deftest >defn-arity-1-test
  (is (= (expand (>defn foobar-1-prod
                   {::g/check true
                    ::g/trace 5}
                   [a b]
                   [int? int? => int?]
                   (do nil)
                   (+ a b)))
         '(defn foobar-1-prod
            {::g/check true
             ::g/trace 5}
            [a b]
            (do nil)
            (+ a b))))
  (is (= (expand (>defn- foobar-1-prod
                   {::g/check true
                    ::g/trace 5}
                   [a b]
                   [int? int? => int?]
                   (do nil)
                   (+ a b)))
         '(defn- foobar-1-prod
            {::g/check true
             ::g/trace 5}
            [a b]
            (do nil)
            (+ a b)))))

(deftest >defn-arity-1-nil-gspec-test
  (is (= (expand (>defn foobar-1-prod
                   {::g/check true
                    ::g/trace 5}
                   [a b]
                   nil
                   (do nil)
                   (+ a b)))
         '(defn foobar-1-prod
            {::g/check true
             ::g/trace 5}
            [a b]
            (do nil)
            (+ a b))))
  (is (= (expand (>defn- foobar-1-prod
                   {::g/check true
                    ::g/trace 5}
                   [a b]
                   nil
                   (do nil)
                   (+ a b)))
         '(defn- foobar-1-prod
            {::g/check true
             ::g/trace 5}
            [a b]
            (do nil)
            (+ a b)))))

(deftest >defn-arity-n-test
  (is (= (expand (>defn foobar-n-prod
                   {::g/check true
                    ::g/trace 5}
                   ([a b]
                    [int? int? => int?]
                    (do nil)
                    (+ a b))
                   ([a b c]
                    [int? int? int? => int?]
                    (+ a b c))))
         '(defn foobar-n-prod
            {::g/check true
             ::g/trace 5}
            ([a b]
             (do nil)
             (+ a b))
            ([a b c]
             (+ a b c)))))
  (is (= (expand (>defn- foobar-n-prod
                   {::g/check true
                    ::g/trace 5}
                   ([a b]
                    [int? int? => int?]
                    (do nil)
                    (+ a b))
                   ([a b c]
                    [int? int? int? => int?]
                    (+ a b c))))
         '(defn- foobar-n-prod
            {::g/check true
             ::g/trace 5}
            ([a b]
             (do nil)
             (+ a b))
            ([a b c]
             (+ a b c))))))

(deftest >defn-arity-n-nil-gspec-test
  (is (= (expand (>defn foobar-n-prod
                   {::g/check true
                    ::g/trace 5}
                   ([a b]
                    nil
                    (do nil)
                    (+ a b))
                   ([a b c]
                    nil
                    (+ a b c))))
         '(defn foobar-n-prod
            {::g/check true
             ::g/trace 5}
            ([a b]
             (do nil)
             (+ a b))
            ([a b c]
             (+ a b c)))))
  (is (= (expand (>defn- foobar-n-prod
                   {::g/check true
                    ::g/trace 5}
                   ([a b]
                    nil
                    (do nil)
                    (+ a b))
                   ([a b c]
                    nil
                    (+ a b c))))
         '(defn- foobar-n-prod
            {::g/check true
             ::g/trace 5}
            ([a b]
             (do nil)
             (+ a b))
            ([a b c]
             (+ a b c))))))

(deftest >fdef-test
  (is (nil? (expand (>fdef foobar-fdef
                      [a b]
                      [int? int? => int?])))))

(deftest check-test
  (is (string? (expand-full (g/check)))))

(deftest *->-test
  (is (threading-expansion-test -> *->
                                (+ 1 2)
                                inc
                                inc
                                dec
                                (+ 2)
                                (/ 4))))

(deftest *->>-test
  (is (threading-expansion-test ->> *->>
                                (+ 1 2)
                                inc
                                inc
                                dec
                                (+ 2)
                                (/ 4))))

(deftest *as->-test
  (is (threading-expansion-test as-> *as->
                                (+ 1 2) x
                                (inc x)
                                (inc x)
                                (dec x)
                                (+ 2 x)
                                (/ x 4))))

(deftest *cond->-test
  (is (threading-expansion-test cond-> *cond->
                                (+ 1 2)
                                (> 1 2) inc
                                (< 0 10) inc
                                false dec
                                true (+ 2)
                                true (/ 4))))

(deftest *cond->>-test
  (is (threading-expansion-test cond->> *cond->>
                                (+ 1 2)
                                (> 1 2) inc
                                (< 0 10) inc
                                false dec
                                true (+ 2)
                                true (/ 4))))

(deftest *some->-nil-test
  (is (threading-expansion-test some-> *some->
                                {:a 123 :b 456}
                                (dissoc :b)
                                :b
                                inc
                                inc)))

(deftest *some->-test
  (is (threading-expansion-test some-> *some->
                                {:a 123 :b 456}
                                :b
                                inc
                                inc)))

(deftest *some->>-nil-test
  (is (threading-expansion-test some->> *some->>
                                :c
                                (conj [:a :b])
                                (remove #{:b})
                                (some #{:b})
                                (conj [1 2 3]))))

(deftest *some->>-test
  (is (threading-expansion-test some->> *some->>
                                :c
                                (conj [:a :b])
                                (some #{:b})
                                (conj [1 2 3]))))

