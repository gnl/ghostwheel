;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.test-utils-cljs)

(defmacro do+expand!
  [form]
  `(do ~form
       (macroexpand-1 (quote ~form))))

(defmacro expand
  [form]
  `(macroexpand-1 (quote ~form)))
