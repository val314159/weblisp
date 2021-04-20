;;;; package.lisp

(defpackage #:weblisp
  (:export spin-up spin-down *ws*)
  (:use #:cl :hunchentoot :hunchensocket :parenscript))
