;;; cl-rss.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(asdf:defsystem #:cl-rss
  :description "A rss fetcher written by common lisp"
  :author      "Your Name"
  :license     "MIT"
  :version     "1.0.0"
  :depends-on  ("lquery" "dexador" "alexandria" "fifo-queue")
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))









