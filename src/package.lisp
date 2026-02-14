;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name


(defpackage #:rss
  (:use #:cl)
  (:export #:initialize-fetcher-cache
	   #:print-cache-queue
	   #:make-fetcher
	   #:define-fetcher
	   #:fetcher-name
	   #:fetch
	   #:fetch/no-cache)
  (:documentation "The rss package."))

(in-package #:rss)

