;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.specs.clojure.core
  #?(:clj
     (:import (clojure.lang Atom IReduce)))
  (:require [clojure.spec.alpha :as s]
            [clojure.core :as c]
            [clojure.string :as cstr]
            [clojure.set :as cset]
            #?@(:cljs [[ghostwheel.core :as g
                        :refer [=> | <-]
                        :refer-macros [? >defn >defn- >fdef]]]
                :clj  [[ghostwheel.core :as g
                        :refer [=> | <- ? >defn >defn- >fdef]]])))

;; Some of the gspecs below (or parts of them) may not be practically relevant or useful
;; at the moment, for example the [any? => any?] ones, but this may change depending on
;; what becomes eventually possible with clojure.spec and/or Ghostwheel.

;; WARNING: Do not instrument this as a whole for now – things will break.


;;;; Basics


;;; Compare

(>fdef c/=
  [x & more]
  [any? (s/* any?) => boolean?])

(>fdef c/not=
  [x & more]
  [any? (s/* any?) => boolean?])

(>fdef c/not
  [x]
  [any? => boolean?])

(>fdef c/compare
  [x y]
  [any? any? => number?])

;;; Loop

(>fdef c/map
  ([f]
   [ifn? => fn?])
  ([f coll & colls]
   [ifn? (? seqable?) (s/* (? seqable?)) => seq?]))

(>fdef c/map-indexed
  ([f]
   [[nat-int? any? => any?] => fn?])
  ([f coll]
   [[nat-int? any? => any?] (? seqable?) => seq?]))

(>fdef c/reduce
  ([f coll]
   [[any? any? => any?] (? seqable?) => any?])
  ([f val coll]
   [[any? any? => any?] any? (? seqable?) => any?]))

(>fdef c/reductions
  [f coll]
  [[any? any? => any?] (? seqable?) => seq?])

;;; Test

(>fdef c/true? [x] [any? => boolean?])
(>fdef c/false? [x] [any? => boolean?])
(>fdef c/instance? [o t] [any? any? => boolean?])
(>fdef c/nil? [x] [any? => boolean?])
(>fdef c/some? [x] [any? => boolean?])


;;;; Functions


;;; Create

(>fdef c/identity
  [x]
  [any? => any?])

(>fdef c/constantly
  [x]
  [any? => [(s/* any?) => any?]])

(>fdef c/comp
  [& fs]
  [(s/* ifn?) => fn?])

(>fdef c/complement
  [f]
  [ifn? => [(s/* any?) => boolean?]])

;; REVIEW: DevTools no like
#_(>fdef c/partial
    [f & args]
    [ifn? (s/* any?) => fn?])

(>fdef c/juxt
  [f & fs]
  [ifn? (s/* ifn?) => fn?])

(>fdef c/memoize
  [f]
  [ifn? => fn?])

(>fdef c/fnil
  ([f x]
   [ifn? some? => fn?])
  ([f x y]
   [ifn? some? some? => fn?])
  ([f x y z]
   [ifn? some? some? some? => fn?]))

(>fdef c/every-pred
  [p & ps]
  [[any? => any?] (s/* [any? => any?]) => [any? => boolean?]])

(>fdef c/some-fn
  [p & ps]
  [[any? => any?] (s/* [any? => any?]) => [any? => any?]])

;;; Call

;; TODO: instrumenting this makes it die with
;; `RangeError: Maximum call stack size exceeded`
#_(>fdef c/apply
    [f pre-args* args]
    [ifn? (s/* any?) (? seqable?) => any?])

;;; Test

(>fdef c/fn?
  [x]
  [any? => boolean?])

(>fdef c/ifn?
  [x]
  [any? => boolean?])


;;;; Numbers


;;; Arithmetic

(>fdef c/quot [x y] [number? number? => number?])
(>fdef c/rem [x y] [number? number? => number?])
(>fdef c/mod [x y] [number? number? => number?])
(>fdef c/inc [x] [number? => number?])
(>fdef c/dec [x] [number? => number?])
(>fdef c/max [x & more] [number? (s/* number?) => number?])
(>fdef c/min [x & more] [number? (s/* number?) => number?])

;;; Compare

;; REVIEW: => boolean? or => some?
(>fdef c/== [x & more] [number? (s/* number?) => boolean?])
(>fdef c/< [x & more] [number? (s/* number?) => boolean?])
(>fdef c/> [x & more] [number? (s/* number?) => boolean?])
(>fdef c/<= [x & more] [number? (s/* number?) => boolean?])
(>fdef c/>= [x & more] [number? (s/* number?) => boolean?])

;;; Cast

(>fdef c/int [x] [number? => int?])

;;; Test

(>fdef c/even? [x] [integer? => boolean?])
(>fdef c/odd? [x] [integer? => boolean?])
(>fdef c/number? [x] [any? => boolean?])
(>fdef c/integer? [x] [any? => boolean?])

;;; Random

(>fdef c/rand
  ([] [=> number?])
  ([x] [number? => number?]))

(>fdef c/rand-int [x] [number? => integer?])


;;;; Strings


;;; Create

(>fdef c/str
  [& xs]
  [(s/* any?) => string?])

(>fdef c/name
  [x]
  [(some-fn string? keyword? symbol?) => string?])

;;; Use

(>fdef c/subs
  ([s start]
   [string? nat-int? => string?])
  ([s start end]
   [string? nat-int? nat-int? => string?]))

(>fdef cstr/join
  ([coll]
   [(? seqable?) => string?])
  ([separator coll]
   [any? (? seqable?) => string?]))

(>fdef cstr/escape
  [s cmap]
  [string? (s/map-of char? any?) => string?])

(>fdef cstr/split
  ([s re]
   [string? regexp? => (s/every string? :kind vector?)])
  ([s re limit]
   [string? regexp? int? => (s/every string? :kind vector?)]))

(>fdef cstr/split-lines
  [s]
  [string? => (s/every string? :kind vector?)])

(>fdef cstr/replace
  [s match replacement]
  [string? (some-fn string? regexp?) (some-fn string? ifn?)
   => string?])

(>fdef cstr/replace-first
  [s match replacement]
  [string? (some-fn string? regexp?) (some-fn string? ifn?)
   => string?])

(>fdef cstr/reverse
  [s]
  [string? => string?])

;;; Regex

(>fdef c/re-find
  [re s]
  [regexp? string? => (? string?)])

(>fdef c/re-matches
  [re s]
  [regexp? string? => (? string?)])

(>fdef c/re-seq
  [re s]
  [regexp? string? => (? (s/every string? :kind seq?))])

(>fdef c/re-pattern
  [s]
  [string? => regexp?])

;;; Letters

(>fdef cstr/capitalize
  [s]
  [string? => string?])

(>fdef cstr/lower-case
  [s]
  [string? => string?])

(>fdef cstr/upper-case
  [s]
  [string? => string?])

;;; Trim

(>fdef cstr/trim
  [s]
  [string? => string?])

(>fdef cstr/trim-newline
  [s]
  [string? => string?])

(>fdef cstr/triml
  [s]
  [string? => string?])

(>fdef cstr/trimr
  [s]
  [string? => string?])

;;; Test

(>fdef c/char
  [x]
  [(s/or :nat-int nat-int?
         :char (s/and string?
                      (fn char? [s] (= (.-length s) 1))))
   => char?])

(>fdef c/string?
  [x]
  [any? => boolean?])

(>fdef cstr/blank?
  [s]
  [(? string?) => boolean?])


;;;; Sequences


;;; General

(>fdef c/count
  [x]
  [(? (some-fn seqable? counted?)) => nat-int?])

;;; Content tests

(>fdef c/distinct?
  [x & more]
  [any? (s/* any?) => boolean?])

(>fdef c/empty?
  [coll]
  [(? seqable?) => boolean?])

(>fdef c/every?
  [pred coll]
  [[any? => any?] (? seqable?) => boolean?])

(>fdef c/not-every?
  [pred coll]
  [[any? => any?] (? seqable?) => boolean?])

(>fdef c/not-any?
  [pred coll]
  [[any? => any?] (? seqable?) => boolean?])

;; REVIEW: some cannot return false. Does it matter?
(>fdef c/some
  [pred coll]
  [[any? => any?] (? seqable?) => any?])

;;; Get shorter

(>fdef c/distinct
  [coll]
  [(? seqable?) => seq?])

(>fdef c/filter
  ([pred]
   [[any? => any?] => fn?])
  ([pred coll]
   [[any? => any?] (? seqable?) => seq?]))

(>fdef c/remove
  ([pred]
   [[any? => any?] => fn?])
  ([pred coll]
   [[any? => any?] (? seqable?) => seq?]))

(>fdef c/take-nth
  ([n]
   [pos-int? => fn?])
  ([n coll]
   [pos-int? (? seqable?) => seq?]))

;;; Get longer

(>fdef c/cons
  [x coll]
  [any? (? seqable?) => seq?])

;; REVIEW: devtools no like
#_(>fdef c/concat
    [& xs]
    [(s/* (? seqable?)) => seq?])

;; REVIEW: Problem
#_(>fdef c/mapcat
    ([f]
     [ifn? => fn?])
    ([f coll & colls]
     [ifn? (? seqable?) (s/* (? seqable?)) => seq?]))

(>fdef c/cycle
  [coll]
  [(? seqable?) => seq?])

(>fdef c/interleave
  [& colls]
  [(s/* (? seqable?)) => seq?])

(>fdef c/interpose
  ([sep]
   [any? => fn?])
  ([sep coll]
   [any? (? seqable?) => seq?]))

;;; Get from tail

(>fdef c/rest
  [coll]
  [(? seqable?) => seq?])

(>fdef c/nthrest
  [coll n]
  [(? seqable?) nat-int? => seq?])

;; REVIEW: can't instrument this - 'max call stack exceeded' exception.
#_(>fdef c/next
    [coll]
    [(? seqable?) => (? seq?)])

(>fdef c/nnext
  [coll]
  [(? seqable?) => (? seq?)])

(>fdef c/fnext
  [coll]
  [(? seqable?) => any?])

(>fdef c/drop
  ([n]
   [nat-int? => fn?])
  ([n coll]
   [nat-int? (? seqable?) => seq?]))

(>fdef c/drop-while
  ([pred]
   [[any? => any?] => fn?])
  ([pred coll]
   [[any? => any?] (? seqable?) => seq?]))

(>fdef c/take-last
  [n coll]
  [nat-int? (? seqable?) => (? seq?)])

;;; Get from head

(>fdef c/take
  ([n]
   [nat-int? => fn?])
  ([n coll]
   [nat-int? (? seqable?) => seq?]))

(>fdef c/take-while
  ([pred]
   [[any? => any?] => fn?])
  ([pred coll]
   [[any? => any?] (? seqable?) => seq?]))

(>fdef c/butlast
  [coll]
  [(? seqable?) => (? seq?)])

(>fdef c/drop-last
  ([coll]
   [(? seqable?) => seq?])
  ([n coll]
   [nat-int? (? seqable?) => seq?]))

;;; 'Change'

(>fdef c/flatten
  [x]
  [(? sequential?) => (? seq?)])

(>fdef c/group-by
  [f coll]
  [[any? => any?] (? seqable?) => map?])

(>fdef c/partition
  ([n coll]
   [pos-int? (? seqable?) => (s/every seq? :kind seq?)])
  ([n step coll]
   [pos-int? pos-int? (? seqable?) => (s/every seq? :kind seq?)])
  ([n step pad coll]
   [pos-int? pos-int? (? seqable?) (? seqable?)
    => (s/every seq? :kind seq?)]))

(>fdef c/partition-all
  ([n]
   [pos-int? => fn?])
  ([n coll]
   [pos-int? (? seqable?) => (s/every seq? :kind seq?)])
  ([n step coll]
   [pos-int? pos-int? (? seqable?) => (s/every seq? :kind seq?)]))

(>fdef c/partition-by
  ([f]
   [[any? => any?] => fn?])
  ([f coll]
   [[any? => any?] (? seqable?) => (s/every seq? :kind seq?)]))

(>fdef c/split-at
  [n coll]
  [nat-int? (? seqable?) => (s/every seq? :kind vector?)])

(>fdef c/split-with
  [pred coll]
  [[any? => any?] (? seqable?) => (s/every seq? :kind vector?)])

(>fdef c/replace
  ([smap]
   [(? map?) => fn?])
  ([smap coll]
   [(? map?) (? seqable?) => seqable?]))

(>fdef c/shuffle
  [coll]
  [sequential? => vector?])

;;; Rearrange

(>fdef c/reverse
  [coll]
  [(? seqable?) => seq?])

(>fdef c/sort
  ([coll]
   [(? seqable?) => seq?])
  ([comp coll]
   [[any? any? => (some-fn boolean? number?)] (? seqable?) => seq?]))

(>fdef c/sort-by
  ([keyfn coll]
   [[any? => any?] (? seqable?) => seq?])
  ([keyfn comp coll]
   [[any? => any?] [any? any? => (some-fn boolean? number?)] (? seqable?)
    => seq?]))

;;; Extract item

(>fdef c/first
  [coll]
  [(? seqable?) => any?])

(>fdef c/second
  [coll]
  [(? seqable?) => any?])

(>fdef c/last
  [coll]
  [(? seqable?) => any?])

(s/def ::seq-head (s/cat :head (? seqable?) :tail (s/* any?)))

(>fdef c/ffirst
  [coll]
  [(? ::seq-head) => any?])

(>fdef c/nfirst
  [coll]
  [(? ::seq-head) => (? seq?)])

(>fdef c/fnext
  [coll]
  [(? seqable?) => any?])

(>fdef c/nnext
  [coll]
  [(? seqable?) => (? seq?)])

;; REVIEW: This is a stricter version of nth
#_(let [nth-index-in-bounds? (fn nth-index-in-bounds? [coll index]
                               (if (and (seq coll) (counted? coll))
                                 (< index (count coll))
                                 true))]
    (>fdef c/nth
      ([coll n]
       [(? seqable?) nat-int? | #(nth-index-in-bounds? coll n)
        => any?])
      ([coll n not-found]
       [(? seqable?) nat-int? any? | #(nth-index-in-bounds? coll n)
        => any?])))

(>fdef c/nth
  ([coll n]
   [(? seqable?) (? nat-int?)                         ;| #(nth-index-in-bounds? coll n)
    => any?])
  ([coll n not-found]
   [(? seqable?) (? nat-int?) any?                    ;| #(nth-index-in-bounds? coll n)
    => any?]))

(>fdef c/nthnext
  [coll n]
  [(? seqable?) nat-int? => (? seq?)])

(>fdef c/rand-nth
  [coll]
  [(? seqable?) => any?])

(>fdef c/max-key
  [k x & more]
  [[any? => number?] any? (s/* any?) => any?])

(>fdef c/min-key
  [k x & more]
  [[any? => number?] any? (s/* any?) => any?])


;;; Force evaluation

(>fdef c/dorun
  [aseq]
  [(? seq?) => nil?])

(>fdef c/doall
  [aseq]
  [(? seq?) => (? seq?)])

(>fdef c/realized?
  [lseq]
  [seq? => boolean?])

;;; Create from collection

;; REVIEW: Trying to instrument this blows up the stack
#_(>fdef c/seq
    [coll]
    [(? seqable?) => (? seq?)])

(>fdef c/rseq
  [coll]
  [reversible? => (? seq?)])

;; TODO
#_(>fdef c/subseq)
#_(>fdef c/rsubseq)

;;; Producer functions

(>fdef c/repeatedly
  ([f]
   [[=> any?] => any?])
  ([n f]
   [pos-int? [=> any?] => any?]))

(>fdef c/iterate
  [f x]
  [[any? => any?] any? => seq?])

;;; From constant

(>fdef c/repeat
  ([x]
   [any? => seq?])
  ([n x]
   [pos-int? any? => seq?]))

(>fdef c/range
  ([]
   [=> (s/every number? :kind seq?)])
  ([end]
   [(every-pred number? pos?) => (s/every number? :kind seq?)])
  ([start end]
   [number? number? | #(< start end) => (s/every number? :kind seq?)])
  ([start end step]
   [number? number? number? | #(not= start end) #(if (< start end)
                                                   (pos? step)
                                                   (neg? step))
    => (s/every number? :kind seq?)]))

;;; From other

;; TODO
#_(>fdef c/tree-seq)

;;; From sequence

(>fdef c/keep
  ([f]
   [[any? => any?] => fn?])
  ([f coll]
   [[any? => any?] (? seqable?) => seq?]))

(>fdef c/keep-indexed
  ([f]
   [[nat-int? any? => any?] => fn?])
  ([f coll]
   [[nat-int? any? => any?] (? seqable?) => seq?]))


;;;; Collections


;;; General

;; REVIEW: This needs more love, throws some exception with DevTools, investigate.
#_(>fdef c/get
    ([o k]
     [(? (some-fn associative? string? set?)) any?
      => any?])
    ([o k not-found]
     [(? (some-fn associative? string? set?)) any? any?
      => any?]))

(>fdef c/empty
  [coll]
  [(? coll?) => (? coll?)])

(>fdef c/not-empty
  [coll]
  [(? coll?) => (? coll?)])

;;; Construct collection

(>fdef c/into
  ([to from]
   [(? coll?) (? seqable?) => (? coll?)])
  ([to xform from]
   [(? coll?) fn? (? seqable?) => (? coll?)]))

(>fdef c/conj
  ([]
   [=> (every-pred vector? empty?)])
  ([coll & xs]
   [(? coll?) (s/* any?) => (? coll?)]))

;;; Capabilities

;; REVIEW: This may be too strict. It's possible to test non-colls for all of these,
;; but does it make sense?
(>fdef c/sequential?
  [coll]
  [any? => boolean?])

(>fdef c/associative?
  [coll]
  [any? => boolean?])

(>fdef c/sorted?
  [coll]
  [any? => boolean?])

(>fdef c/counted?
  [coll]
  [any? => boolean?])

(>fdef c/reversible?
  [coll]
  [any? => boolean?])

;;; Type tests

(>fdef c/coll?
  [x]
  [any? => boolean?])

(>fdef c/list?
  [x]
  [any? => boolean?])

(>fdef c/vector?
  [x]
  [any? => boolean?])

(>fdef c/set?
  [x]
  [any? => boolean?])

(>fdef c/map?
  [x]
  [any? => boolean?])

(>fdef c/seq?
  [x]
  [any? => boolean?])


;;;; Sequentials


(>fdef c/peek
  [coll]
  [(? sequential?) => any?])

(>fdef c/pop
  [coll]
  [(? (every-pred sequential? not-empty)) => (? sequential?)])


;;;; Associatives


(defn- vector-index-int-in-bounds? [v index]
  (and (nat-int? index)
       (<= index (count v))))

;; TODO: even without the st predicates this gives weird instrumentation errors when using assoc.
#_(>fdef c/assoc
    [coll k v & kvs]
    [(? associative?) any? any? (s/* any?) |
     #(even? (count kvs))
     #(if (vector? coll)
        (and (vector-index-int-in-bounds? coll k)
             (->> (take-nth 2 kvs)
                  (every? (partial vector-index-int-in-bounds? coll))))
        true)
     => associative?])

(>fdef c/reduce-kv
  [f init coll]
  [[any? any? any? => any?] any? (? associative?) => any?])

(>fdef c/get-in
  ([m ks]
   [(? associative?) (? sequential?) => any?])
  ([m ks not-found]
   [(? associative?) (? sequential?) any? => any?]))

(>fdef c/assoc-in
  [a ks v]
  [(? associative?) sequential? any? => associative?])

(>fdef c/update-in
  [m ks f & args]
  [associative? sequential? ifn? (s/* any?) => associative?])

(>fdef c/contains?
  [coll k]
  [(? (some-fn associative? set?)) any? |
   #(if (vector? coll) (nat-int? k) true)
   => boolean?])


;;;; Lists


;;; Create

(>fdef c/list
  [& items]
  [(s/* any?) => list?])

(>fdef c/list*
  [items* args]
  [(s/* any?) (? seqable?) => (? list?)])


;;;; Vectors


;;; Create

(>fdef c/vector
  [& args]
  [(s/* any?) => vector?])

(>fdef c/vec
  [coll]
  [(? seqable?) => vector?])

;;; 'Change'

(>fdef c/subvec
  ([v start]
   [vector? nat-int? | #(vector-index-int-in-bounds? v start)
    => vector?])
  ([v start end]
   [vector? nat-int? nat-int? |
    #(vector-index-int-in-bounds? v start)
    #(vector-index-int-in-bounds? v end)
    => vector?]))

;;; Loop

(>fdef c/mapv
  [f coll & colls]
  [ifn? (? seqable?) (s/* (? seqable?)) => vector?])

(>fdef c/filterv
  [pred coll]
  [[any? => any?] (? seqable?) => vector?])


;;;; Maps


;;; Create

(>fdef c/hash-map
  [& keyvals]
  [(s/* any?) | #(even? (count keyvals)) => map?])

(>fdef c/array-map
  [& keyvals]
  [(s/* any?) | #(even? (count keyvals)) => map?])

(>fdef c/sorted-map
  [& keyvals]
  [(s/* any?) | #(even? (count keyvals)) => map?])

(>fdef c/sorted-map-by
  [comparator & keyvals]
  [[any? any? => any?] (s/* any?) | #(even? (count keyvals)) => map?])

(>fdef c/zipmap
  [keys vals]
  [(? seqable?) (? seqable?) => map?])

(>fdef c/frequencies
  [coll]
  [(? seqable?) => map?])

;;; Examine

(>fdef c/vals
  [m]
  [(? map?) => (? seq?)])

(>fdef c/keys
  [m]
  [(? map?) => (? seq?)])

;;; Change

(>fdef c/dissoc
  [m & ks]
  [(? map?) (s/* any?) => (? map?)])

(>fdef c/merge
  [& maps]
  [(s/* (? map?)) => (? map?)])

(>fdef c/merge-with
  [f & maps]
  [[any? any? => any?] (s/* (? map?)) => (? map?)])

(>fdef c/select-keys
  [m ks]
  [(? map?) (? seqable?) => map?])

;;; Entry

(>fdef c/find
  [m k]
  [(? map?) any? => (? map-entry?)])

(>fdef c/key
  [map-entry]
  [map-entry? => any?])

(>fdef c/val
  [map-entry]
  [map-entry? => any?])


;;;; Sets


;;; Create

(>fdef c/set
  [coll]
  [(? seqable?) => set?])

(>fdef c/hash-set
  [& ks]
  [(s/* any?) => set?])

(>fdef c/sorted-set-by
  [comparator & ks]
  [[any? any? => any?] (s/* any?) => set?])

;;; Change

(>fdef c/disj
  [coll & ks]
  [(? set?) (s/* any?) => (? set?)])

;;; Set ops

(>fdef cset/union
  [& sets]
  [(s/* (? set?)) => (? set?)])

(>fdef cset/difference
  [s1 & sets]
  [(? set?) (s/* (? set?)) => (? set?)])

(>fdef cset/intersection
  [s1 & sets]
  [(? set?) (s/* (? set?)) => (? set?)])

(>fdef cset/select
  [pred xset]
  [[any? => any?] (? set?) => (? set?)])

;;; Test

(>fdef cset/subset?
  [a b]
  [(? set?) (? set?) => boolean?])

(>fdef cset/superset?
  [a b]
  [(? set?) (? set?) => boolean?])


;;;; Atoms / State


(defn atom?
  [x]
  #?(:clj  (instance? Atom x)                         ; REVIEW: This is probably too specific.
     :cljs (implements? IAtom x)))

;;; Create

(s/def ::meta map?)
(s/def ::validator ifn?)
(>fdef c/atom
  ([x]
   [any? => atom?])
  ([x opts]
   [any? (s/keys :opt-un [::meta ::validator]) => atom?]))

;;; Get value

;; REVIEW: This is too specific and generally doesn't work at the moment.
#_(>fdef c/deref
    [a]
    [atom? => any?])

;;; Set value

(>fdef c/swap!
  [a f & args]
  [atom? ifn? (s/* any?) => any?])

(>fdef c/reset!
  [a new-val]
  [atom? any? => any?])

(>fdef c/compare-and-set!
  [a old-val new-val]
  [atom? any? any? => any?])

;;; Watch

(>fdef c/add-watch
  [a key f]
  [atom? keyword? [keyword? atom? any? any? => any?]
   => any?])

(>fdef c/remove-watch
  [a key]
  [atom? keyword? => any?])

;;; Validators

(>fdef c/set-validator!
  [a f]
  [atom? [? [any? => any?]] => any?])

(>fdef c/get-validator
  [a]
  [atom? => (? fn?)])


;;;; JS Interop


;;; Create object

#?(:cljs
   (>fdef c/js-obj
     [& keyvals]
     [(s/* any?) | #(even? (count keyvals)) => object?]))

;;; Create array

#?(:cljs
   (do
     (>fdef c/into-array
       [aseq]
       [(? seqable?) => array?])

     (>fdef c/to-array-2d
       [coll]
       [(? (s/every (? seqable?))) => array?])

     (>fdef c/array
       [& args]
       [(s/* any?) => array?])

     (>fdef c/make-array
       [size]
       [nat-int? => array?])

     (>fdef c/aclone
       [arr]
       [array? => array?])))

;;; Delete property

#?(:cljs
   (>fdef c/js-delete
     [obj key]
     [object? string? => true?]))

;;; Convert between

;; REVIEW: Possibly too strict.
#?(:cljs
   (do
     (>fdef c/js->clj
       [x]
       [(some-fn array? object? boolean? number? string? nil?) => any?])

     (>fdef c/clj->js
       [x]
       [(complement (some-fn array? object?)) => any?])))

;;; Type tests

#?(:cljs
   (do
     (>fdef c/array?
       [x]
       [any? => boolean?])

     (>fdef c/object?
       [x]
       [any? => boolean?])))

