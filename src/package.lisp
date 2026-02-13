;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name


(defpackage #:rss
  (:use #:cl)
  (:export #:initialize-fetcher-cache
	   #:print-rss-cache-queue
	   #:make-rss-fetcher
	   #:define-rss-fetcher
	   #:fetcher-name
	   #:fetch)
  (:documentation "The rss package."))

(in-package #:rss)

