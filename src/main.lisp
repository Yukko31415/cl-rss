;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(in-package #:rss-parser)



(defun string->keyword (string)
  (intern (string-upcase string) :keyword))

(defun string->symbol (string)
  (intern (string-upcase string)))

(defun fetch-rss-elements (url tags)
  "URLからRSSを取得し、TAGSで指定された要素をplistのリストとして返す"
  (let* ((xml-string (dex:get url))
	 ;; plumpでパースし、lqueryを初期化
	 (doc (plump:parse xml-string))
	 ;; RSS 2.0なら "item"、Atomなら "entry" を取得
	 (items (lquery:$ doc "item, entry"))
	 (tag-string (format nil "~{~@:(~a~)~^, ~}" tags)))

    ;; 各item/entryに対して処理を行う
    (loop :for item :across items
	  :collect (loop :for tag :in (mapcar #'string->keyword tags)
			 :and node :across (lquery:$ item tag-string (text))
			 :append (list tag node)))))


(defun check-id (rss-cache id)
  (let ((table (rss-cache-table rss-cache))
	(queue (rss-cache-queue rss-cache))
	(length (rss-cache-length rss-cache))
	(size (rss-cache-size rss-cache))) 
    (if (nth-value 1 (gethash id table))
	t
	(progn (if (>= length size)
		   (progn (remhash (funcall queue :pop) table)
			  (setf (gethash id table) id)
			  (funcall queue :push id))
		   (progn (setf (gethash id table) id)
			  (funcall queue :push id)
			  (incf (rss-cache-length rss-cache))))
	       nil))))


(defclass rss-cache ()
  ((name :initarg :name :reader rss-cache-name)
   (url :initarg :url :reader rss-cache-url)
   (tags :initarg :tags :reader rss-cache-tags)
   (table :initarg :table :reader rss-cache-table)
   (queue :initform (fifo-queue:make-fifo-queue-handler)
	  :reader rss-cache-queue)
   (size :initarg :size :reader rss-cache-size)
   (length :initform 0 :accessor rss-cache-length)))


(defclass rss-fetcher ()
  ((cache :initarg :cache :reader rss-fetcher-cache)
   (fetcher :initarg :fetcher :accessor rss-fetcher)))

(defmacro define-item-object (name tags)
  (let ((slots (mapcar #'string->symbol tags)))
    `(defstruct ,name ,@slots)))


(defmacro %make-rss-fecher (cache key)
  (alexandria:with-gensyms (items)
   (let* ((name (rss-cache-name cache))
	  (tags (rss-cache-tags cache))
	  (url (rss-cache-url cache))
	  (constructer (string->symbol (format nil "make-~a" name)))
	  (interned-key (string->keyword key)))
     `(lambda () (let ((,items (remove-if
			  #'(lambda (_) (check-id ,cache _))
			  (fetch-rss-elements ,url ',tags)
			  :key #'(lambda (_) (getf _ ,interned-key)))))
	      (mapcar #'(lambda (_) (apply #',constructer _)) ,items))))))


(defun make-rss-cache (name url tags &key (size 100))
  (make-instance 'rss-cache
		 :name name
		 :url url
		 :tags tags
		 :table (make-hash-table :size size :test #'equal)
		 :size size))


(defmacro make-rss-fecher (name url tags &key key (size 100))
  "キャッシュオブジェクトとフェッチャーを生成する。フェッチャーは関数オブジェクトであり、funcallするとキャッシュに存在しない新規の記事オブジェクトを確認し、リストを返す。"
  (let ((tags (push key tags))
	(cache (make-rss-cache name url tags :size size)))
    `(progn (define-item-object ,name ,tags)
	    (make-instance 'rss-fetcher
			   :cache ,cache
			   :fetcher (%make-rss-fecher ,cache ,key)))))







