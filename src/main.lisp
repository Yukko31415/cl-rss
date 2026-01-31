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


(defun make-constructor-name (name)
  (string->symbol (format nil "make-~a" name)))


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
			 :if node
			   :append (list tag node)))))


(defclass rss-cache ()
  ((name :initarg :name :reader rss-cache-name)
   (url :initarg :url :reader rss-cache-url)
   (tags :initarg :tags :reader rss-cache-tags)
   (table :initarg :table :reader rss-cache-table)
   (queue-handler :initform (fifo-queue:make-fifo-queue-handler)
	  :reader rss-cache-queue-handler)
   (size :initarg :size :reader rss-cache-size)
   (key :initarg :key :reader rss-cache-key)
   (length :initform 0 :accessor rss-cache-length)))



(declaim (ftype (function (symbol simple-string list &key (:key simple-string) (:size fixnum)))
	  make-rss-cache))
(defun make-rss-cache (name url tags &key key (size 100))
  (make-instance 'rss-cache
		 :name name
		 :url url
		 :tags tags
		 :table (make-hash-table :size size :test #'equal)
		 :key key
		 :size size))



(defun check-id (rss-cache id)
  (let ((table (rss-cache-table rss-cache))
	(queue-handler (rss-cache-queue-handler rss-cache))
	(length (rss-cache-length rss-cache))
	(size (rss-cache-size rss-cache))) 
    (if (nth-value 1 (gethash id table))
	t
	(progn (if (>= length size)
		   (progn (remhash (funcall queue-handler :pop) table)
			  (setf (gethash id table) id)
			  (funcall queue-handler :push id))
		   (progn (setf (gethash id table) id)
			  (funcall queue-handler :push id)
			  (incf (rss-cache-length rss-cache))))
	       nil))))


(defclass rss-fetcher ()
  ((cache :initarg :cache :reader rss-fetcher-cache)
   (fetcher :initarg :fetcher :accessor rss-fetcher)))




(defun make-constructor (name tags key)
  (let* ((symbolized-tags (mapcar #'string->symbol tags))
	 (symbolized-key (string->symbol key))
	 (arg-list (loop :for symbol :in symbolized-tags
			 :for keyword :in (mapcar #'string->keyword tags)
			 :append (list keyword symbol))))
    (if (member key tags :test 'string=)
	`(defun ,(make-constructor-name name) (&key ,@symbolized-tags)
	   (apply #'make-instance ',name '(,@arg-list)))
	`(defun ,(make-constructor-name name) (&key ,@symbolized-tags ,symbolized-key)
	   (apply #'make-instance ',name '(,@arg-list))))))


(defun make-slot (name tag)
  (let ((keyword (string->keyword tag))
	(symbol (string->symbol tag))
	(reader (string->symbol (format nil "~a-~a" name tag))))
    `(,symbol :initarg ,keyword :reader ,reader)))


(defmacro define-item-object (name tags key)
  (let ((slots (mapcar #'(lambda (_) (make-slot name _)) tags))
	(constructor (make-constructor name tags key)))
    `(progn (defclass ,name () ,slots)
	    ,constructor)))



(defun make-tags-list (tags key)
  (if (member key tags :test #'string=)
      tags
      (cons key tags)))

(defmacro %make-rss-fecher (cache)
  (alexandria:with-gensyms (items)
    (let* ((name (rss-cache-name cache))
	   (tags (rss-cache-tags cache))
	   (key (rss-cache-key cache))
	   (tags-list (make-tags-list tags key))
	   (url (rss-cache-url cache))
	   (constructor (make-constructor-name name))
	   (interned-key (string->keyword key)))
      `(lambda () (let ((,items (remove-if #'(lambda (_) (check-id ,cache _))
				      (fetch-rss-elements ,url ',tags-list)
				      :key #'(lambda (_) (getf _ ,interned-key)))))
	       (mapcar #'(lambda (_) (apply #',constructor _)) ,items))))))




(defun print-rss-cache-queue (fetcher &optional stream)
  "queueをstreamに出力する"
  (let* ((cache (rss-fetcher-cache fetcher))
	 (cache-queue (funcall (rss-cache-queue-handler cache) :view)))
    (print cache-queue stream))
  fetcher)


(defmacro make-rss-fecher (name url tags &key key (size 100))
  "キャッシュオブジェクトとフェッチャーを生成する。
フェッチャーは関数オブジェクトであり、funcallするとキャッシュに存在しない新規の記事オブジェクトを確認し、リストを返す。"
  (dolist (str tags) (declare (simple-string str)))
  (let* ((cache (make-rss-cache name url tags :key key :size size))
	 (name (rss-cache-name cache))
	 (tags (rss-cache-tags cache))
	 (key (rss-cache-key cache)))
    `(progn (define-item-object ,name ,tags ,key)
	    (make-instance 'rss-fetcher
			   :cache ,cache
			   :fetcher (%make-rss-fecher ,cache)))))


(defun initialize-fetcher-cache (fetcher queue)
  "出力されたqueueのリストを受け取り、cache内のqueueを初期化する"
  (let* ((cache (rss-fetcher-cache fetcher))
	 (table (rss-cache-table cache))
	 (queue-handler (rss-cache-queue-handler cache))
	 (list (nreverse queue)))
    (dolist (key list)
      (setf (gethash key table) key)
      (funcall queue-handler :push key))
    fetcher))





