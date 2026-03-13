;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(in-package #:rss)



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
  ((url :initarg :url :reader rss-cache-url)
   (tags :initarg :tags :reader rss-cache-tags)
   (table :initarg :table :reader rss-cache-table)
   (queue :initarg :queue :reader rss-cache-queue)
   (size :initarg :size :reader rss-cache-size)
   (key :initarg :key :reader rss-cache-key)
   (length :initform 0 :accessor rss-cache-length)))


(defun check-id (rss-cache id)
  (declare (rss-cache rss-cache))
  (let ((table (rss-cache-table rss-cache))
	(queue (rss-cache-queue rss-cache))
	(length (rss-cache-length rss-cache))
	(size (rss-cache-size rss-cache)))
    (if (nth-value 1 (gethash id table))
	t
	(progn (if (>= length size)
		   (progn (remhash (fifo-queue:pop-queue queue) table)
			  (setf (gethash id table) id)
			  (fifo-queue:push-queue id queue))
		   (progn (setf (gethash id table) id)
			  (fifo-queue:push-queue id queue)
			  (incf (rss-cache-length rss-cache))))
	       nil))))




(defclass rss-fetcher ()
  ((name :initarg :name :reader fetcher-name)
   (constructor :initarg :constructor :reader fetcher-constructor)
   (cache :initarg :cache :reader fetcher-cache)))


(defun make-constructor (name tags key)
  (let* ((symbolized-tags (mapcar #'string->symbol tags))
	 (symbolized-key (string->symbol key))
	 (arg-list (loop :for symbol :in symbolized-tags
			 :for keyword :in (mapcar #'string->keyword tags)
			 :append (list keyword symbol))))
    (if (member key tags :test 'string=)
	`(defun ,(make-constructor-name name) (&key ,@symbolized-tags)
	   (apply #'make-instance ',name (list ,@arg-list)))
	`(defun ,(make-constructor-name name) (&key ,@symbolized-tags ,symbolized-key)
	   (declare (ignore ,symbolized-key))
	   (apply #'make-instance ',name (list ,@arg-list))))))


(defun make-slot (name tag)
  (let ((keyword (string->keyword tag))
	(symbol (string->symbol tag)))
    `(,symbol :initarg ,keyword :reader ,symbol)))


(defmacro define-item-object (name tags key)
  (let ((slots (mapcar #'(lambda (_) (make-slot name _)) tags))
	(constructor (make-constructor name tags key)))
    `(progn (defclass ,name () ,slots)
	    ,constructor)))


(defun make-tags-list (tags key)
  (if (member key tags :test #'string=)
      tags
      (cons key tags)))






(defgeneric make-fetcher (name))


(defmacro define-fetcher (name url tags &key key (size 100))
  "キャッシュオブジェクトとフェッチャーを生成する。
フェッチャーは関数オブジェクトであり、funcallするとキャッシュに存在しない新規の記事オブジェクトを確認し、リストを返す。"
  (dolist (str tags) (declare (simple-string str)))
  (let ((constructor (make-constructor-name name)))
    (alexandria:with-gensyms (symbol cache)
      `(progn (define-item-object ,name ,tags ,key)
	      (defmethod make-fetcher ((,symbol (eql ',name)))
		(declare (ignore ,symbol))
		(let ((,cache (make-instance 'rss-cache
					     :url ,url
					     :tags ',tags
					     :queue (fifo-queue:make-queue)
					     :table (make-hash-table :size ,size :test #'equal)
					     :key ,key
					     :size ,size)))
		  (make-instance 'rss-fetcher
				 :name ',name
				 :cache ,cache
				 :constructor #',constructor)))))))





(defun print-cache-queue (fetcher &optional stream)
  "queueをstreamに出力する"
  (let* ((cache (fetcher-cache fetcher))
	 (cache-queue (rss-cache-queue cache)))
    (fifo-queue:print-queue-list cache-queue stream))
  fetcher)


(defun fetch (fetcher)
  "fetchを行う。itemのリストを返す。"
  (let* ((constructor (fetcher-constructor fetcher))
	 (cache (fetcher-cache fetcher))
	 (tags (rss-cache-tags cache))
	 (key (rss-cache-key cache))
	 (tags-list (make-tags-list tags key))
	 (interned-key (string->keyword key))
	 (url (rss-cache-url cache))
	 (items (remove-if #'(lambda (_) (check-id cache _)) (fetch-rss-elements url tags-list)
			   :key #'(lambda (_) (getf _ interned-key)))))
    (mapcar #'(lambda (_) (apply constructor _)) items)))


(defun fetch/no-cache (fetcher)
  (let* ((constructor (fetcher-constructor fetcher))
	 (cache (fetcher-cache fetcher))
	 (tags (rss-cache-tags cache))
	 (key (rss-cache-key cache))
	 (tags-list (make-tags-list tags key))
	 (url (rss-cache-url cache))
	 (items (fetch-rss-elements url tags-list)))
    (mapcar #'(lambda (_) (apply constructor _)) items)))


(defun initialize-fetcher-cache (fetcher queue)
  "出力されたqueueのリストを受け取り、cache内のqueueを初期化する"
  (let* ((cache (fetcher-cache fetcher))
	 (list (nreverse queue)))
    (dolist (key list) (check-id cache key))
    fetcher))

