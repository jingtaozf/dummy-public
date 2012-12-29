;;; -*- encoding:utf-8 -*-  --- 
;; 
;; Filename: kindlecollection.el
;; Description: kindle collection manager.
;; Author: Xu Jingtao <jingtaozf@gmail.com>
;; Created: 2012.01.24 21:59:04(+0800)
;; Last-Updated: 2012.05.14 08:40:58(+0800)
;;     Update #: 64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 

(provide 'kindlecollection)
(require 'binview)

(defvar *kindle-filetypes* '("pdf" "mobi" "prc" "txt" "tpz" "azw1" "azw" "manga" "azw2"))

(defvar *kindle-path-hash* nil)
(defun kindle-hash-file (file)
  (when (loop for type in *kindle-filetypes*
              thereis (string= type (file-name-extension file)))
    (cond ((string-match ".*-asin_\\(.*?\\)-type_\\(.*?\\)-.*$" file)
           (concat "*" (match-string 1 file) "^" (match-string 2 file)))
          ((loop for name in '("azw" "mobi")
                 thereis (string= name (file-name-extension file)))
           (ignore-errors (concat "#" (kindle-mobi-hash file)))))))
(defun kindle-refresh-path-hash ()
  (setq *kindle-path-hash* (make-hash-table :test 'equal))
  ;;  traverse-walk-directory
  (traverse-walk-directory
   "/mnt/us/documents" 
   :file-fn #'(lambda (file)
                (message "add file %s to *kindle-path-hash*" file)
                (awhen (kindle-hash-file file)
                  (setf (gethash it *kindle-path-hash*) file))
                (setf (gethash (sha1 file) *kindle-path-hash*) file)))
  (message "kindle-refresh-path-hash...done"))

(defun kindle-load-collects.json ()
  (let ((collections (reverse (json-read-file "/mnt/us/system/collections.json"))))
    (loop for collection in collections
          do (loop with items = (cdr (assoc 'items collection))
                   for i from 0
                   for x across items
                   do (setf (aref items i) (gethash (subseq x 1) *kindle-path-hash* ))))
    collections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dump mobi file format info from mobi file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kindle-mobi-hash (file)
  "converted from http://www.richardpeng.com/projects/kindelabra/
This function is too slow when process large files!!!"
  (with-temp-buffer 
    (insert-file-contents-literally file nil 0 #x60)
    (hexl-mode 1)
    (view-mode 1)
    (hexl-goto-address #x3c)
    (when (string= "BOOKMOBI" (loop for i from 1 to 8 concat (binview-char)))
      (let* ((sections-number (progn (hexl-goto-address #x4c) (binview-uint16)))
             (section-1-start (progn (hexl-goto-address #x4e) (binview-uint32)))
             (section-1-end (progn (hexl-goto-address #x56) (binview-uint32))))
        (with-temp-buffer
          (insert-file-contents-literally file nil section-1-start section-1-end)
          (hexl-mode 1)
          (view-mode 1)
          (let* ((mobi-len (progn (hexl-goto-address 20) (+ 16 (binview-uint32))))
                 (title-offset (progn (hexl-goto-address 84) (binview-uint32)))
                 (title-len (progn (hexl-goto-address 88) (binview-uint32)))
                 (title (progn (hexl-goto-address title-offset)
                               (loop for i from 1 to title-len concat (binview-char))))
                 (exth-len (progn (hexl-goto-address (+ mobi-len 4)) (binview-uint32)))
                 asin type)
            (hexl-goto-address (+ mobi-len 12))
            (loop with count = 12
                  while (< count exth-len)
                  do (let* ((rec-type (binview-uint32))
                            (rec-len (- (binview-uint32) 8))
                            (rec-data (loop for i from 1 to rec-len concat (binview-char))))
                       (case rec-type
                         (113 (setf asin rec-data))
                         (501 (setf type rec-data))
                         (503 (setf title rec-data)))
                       (incf count (+ 8 rec-len))))
            (when (and asin type)
              (format "#%s^%s" asin type))))))))
