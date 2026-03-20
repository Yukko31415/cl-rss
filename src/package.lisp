;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name


(defpackage #:rss
  (:use #:cl)
  (:export #:define-fetcher
	   #:fetch
	   #:fetch-rss-elements
	   #:fetch/no-cache
	   #:fetcher-name
	   #:initialize-fetcher-cache
	   #:make-fetcher
	   #:print-cache-queue)
  (:documentation "The rss package."))

(in-package #:rss)

