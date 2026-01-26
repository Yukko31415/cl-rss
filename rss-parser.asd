;;; rss-parser.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(asdf:defsystem #:rss-parser
  :description "A rss fetcher written by common lisp"
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ("lquery" "dexador" "alexandria" "fifo-queue")
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))




