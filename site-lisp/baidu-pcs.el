;;; -*- encoding:utf-8 -*-  ---
;; 
;; Filename: baidu-pcs.el
;; Description: 
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2013.12.11 13:04:20(+0800)
;; Last-Updated: 2013.12.31 15:37:59(+0800)
;;     Update #: 195
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  1. custom the variable `*pcs-id*',`*pcs-secret*'.
;;  2. run command `pcs-user-code' to authorize current machine.
;;  3. run command `pcs-setup-token' to fetch access token.
;;
;; Then you call run command `pcs-info',`pcs-upload',`pcs-list' now.
(provide 'baidu-pcs)

(defgroup pcs nil "*baidu personal cloud storage")
(defcustom *pcs-id* nil "*pcs developer id" :group 'pcs :type 'string)
(defcustom *pcs-secret* nil "*pCS developer secret code" :group 'pcs :type 'string)
(defcustom *pcs-token* nil "*PCS token list" :group 'pcs)
(defcustom *pcs-root* nil "*PCS default root path in server,often start with '/apps'" :group 'pcs :type 'string)

(loop for symbol in '(*pcs-id* *pcs-secret* *pcs-token* *pcs-root*)
      do (customize-mark-to-save symbol))

(defvar *pcs-device-code* nil)

(defun pcs-token-expire-p ()
  (let ((current-time (floor (time-to-seconds (current-time))))) 
    (> current-time
       (+ (cdr (assoc 'begin_time *pcs-token*)) (- (cdr (assoc 'expires_in *pcs-token*)) 120)))))
(defun pcs-access-token ()
  (cdr (assoc 'access_token *pcs-token*)))

(defun pcs-curl-cmd (url post-alist &optional curl-args)
  (let ((post-data (loop for (key . value) in post-alist
                         when (and key value)
                           concat (concat "&" (url-encode-url key) "=" (if (= 0 (length value))
                                                                         ""
                                                                         (url-encode-url value))))))
    (concat "curl --silent --insecure --location --max-time 30 \"" url "\""
            (if (> (length post-data) 0)
              (concat " -d \"" (substring post-data 1) "\"")
              "")
            " " (or curl-args ""))))

(defun pcs-curl (url post-alist &optional curl-args)
  (require 'json)
  (json-read-from-string (shell-command-to-string (pcs-curl-cmd url post-alist curl-args))))

(defun pcs-acurl (url post-alist curl-args callback)
  (require 'json)
  (eval `(async-start
          #'(lambda () (shell-command-to-string ,(pcs-curl-cmd url post-alist curl-args)))
          #'(lambda (result)
              (message "result:%s" result)
              (funcall ,callback (json-read-from-string result))))))

(defun pcs-error-detect (result)
  (when (assoc 'error result)
    (error "pcs error detected:%s" result)))

(defun pcs-user-code ()
  (interactive)
  (let ((result (pcs-curl "https://openapi.baidu.com/oauth/2.0/device/code"
                          `(("client_id" . ,*pcs-id*)
                            ("response_type" . "device_code")
                            ("scope" . "basic,netdisk")))))
    (pcs-error-detect result)
    (let ((user-code (cdr (assoc 'user_code result)))
          (verification-url (cdr (assoc 'verification_url result))))
      (setf *pcs-device-code* (cdr (assoc 'device_code result)))
      (when (fboundp 'my-set-clipboard-contents-from-string)
        (my-set-clipboard-contents-from-string user-code))
      (if (fboundp 'chrome)
        (chrome verification-url))
      (message "user code is <%s>, please visit url %s to verify it,then execute <pcs-setup-token>"
               user-code verification-url))))

(defun pcs-setup-token ()
  (interactive)
  (if *pcs-device-code*
    (let ((result (pcs-curl "https://openapi.baidu.com/oauth/2.0/token"
                            `(("grant_type" . "device_token")
                              ("code" . ,*pcs-device-code*)
                              ("client_id" . ,*pcs-id*)
                              ("client_secret" . ,*pcs-secret*)))))
      (pcs-error-detect result)
      (setf *pcs-device-code* nil
            *pcs-token* (cons (cons 'begin_time (floor (time-to-seconds (current-time)))) result))
      (custom-save-all))
    (pcs-user-code)))

(defun pcs-refresh-token ()
  (let ((result (pcs-curl "https://openapi.baidu.com/oauth/2.0/token"
                          `(("grant_type" . "refresh_token")
                            ("refresh_token" . ,(cdr (assoc 'refresh_token *pcs-token*)))
                            ("client_id" . ,*pcs-id*)
                            ("client_secret" . ,*pcs-secret*)))))
    (pcs-error-detect result)
    (setf *pcs-token* (cons (cons 'begin_time (floor (time-to-seconds (current-time)))) result))
    (custom-save-all)))

(defun pcs-ensure-token ()
  (unless *pcs-token*
    (error "please execute <pcs-user-code> first!"))
  (when (pcs-token-expire-p)
    (pcs-refresh-token)))
    
(defun pcs-info ()
  (interactive)
  (pcs-ensure-token)
  (let ((result (pcs-curl (build-url-get-parameters
                           "https://pcs.baidu.com/rest/2.0/pcs/quota"
                           `(("method" . "info")
                             ("client_id" . ,*pcs-id*)
                             ("client_secret" . , *pcs-secret*)
                             ("access_token" . ,(pcs-access-token))))
                          nil)))
    (pcs-error-detect result)
    (message "pcs info:%s" result)
    result))

(defun pcs-upload (local-file remote-path)
  (interactive "fInput local file to upload:\nMinput the remote path:")
  (pcs-ensure-token)
  (setf remote-path (concat *pcs-root* "/" remote-path))
  (pcs-ensure-token)
  (pcs-acurl (build-url-get-parameters
              "https://c.pcs.baidu.com/rest/2.0/pcs/file"
              `(("method" . "upload")
                ("path" . ,(concat remote-path "/" (pathname-nondirectory local-file)))
                ("ondup" . "overwrite")
                ("access_token" . ,(pcs-access-token))))
             nil
             (concat " -F 'file=@\"" local-file "\"'")
             #'(lambda (result)
                 (pcs-error-detect result)
                 (message "file uploaded to pcs:%s" result))))

(defun pcs-list (&optional remote-path)
  (interactive "Minput the remote path: ")
  (setf remote-path (concat *pcs-root* "/" (or remote-path "")))
  (pcs-ensure-token)
  (let ((result (pcs-curl (build-url-get-parameters
                           "https://c.pcs.baidu.com/rest/2.0/pcs/file"
                           `(("method" . "list")
                             ("path" . ,remote-path)
                             ("access_token" . ,(pcs-access-token))))
                          nil)))
    (pcs-error-detect result)
    (let* ((list (cdr (assoc 'list result)))
           (paths (loop for i from 0 to (1- (length list))
                        for info = (aref list i)
                        for path = (cdr (assoc 'path info))
                        collect (substring path (1+ (length *pcs-root*))))))
      (message "pcs list:%s" paths)
      paths)))

;; 下载文件：
;; curl -k -O "https://d.pcs.baidu.com/rest/2.0/pcs/file?method=download&access_token=<access_token>&\
;; path=/apps/pcsupload/haiyun.me.tar.gz"

;; 删除文件：
;; curl -k -L "https://pcs.baidu.com/rest/2.0/pcs/file?method=delete&access_token=<access_token>\
;; &path=/apps/pcsupload/haiyun.me.tar.gz"

;; 复制文件：
;; curl -k -L "https://c.pcs.baidu.com/rest/2.0/pcs/file?method=copy&access_token=<access_token>\
;; &from=/apps/pcsupload/haiyun.me.tar.gz&to=/apps/pcsupload/www.haiyun.me.tar.gz"

(defvar *pcs-actions*
  '(("info" pcs-info)
    ("upload a file" pcs-upload)
    ("list files in directory" pcs-list)))
(defun pcs-menu ()
  (interactive)
  (when (fboundp 'my-select-window)
    (awhen (my-select-window (mapcar #'(lambda (x) (car x)) *pcs-actions*) :delay-seconds 0)
      (awhen (assoc it *pcs-actions*)
        (call-interactively (cadr it))))))
