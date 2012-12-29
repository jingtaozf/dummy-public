;;; -*- encoding:utf-8 -*-  --- 
;; 
;; Filename: virsh.el
;; Description: emacs virsh utilities. 
;; Author: Xu Jingtao <jingtaozf@gmail.com>
;; Created: 2011.12.08 13:42:21(+0800)
;; Last-Updated: 2012.06.28 08:39:59(+0800)
;;     Update #: 32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; 1. setq *virsh-hosts* '(host1 host2 ...)
;; 2. for host in host1 host2 ...; do ssh-copy-id -i ~/.ssh/id_rsa.pub $host; done
;; 3. M-x, my-virsh
;; 

(provide 'virsh)

(defvar *virsh-hosts* '())
(defvar *virsh-actions*
  '(("start" virsh-start)
    ("destroy" virsh-destroy)
    ("vncview" virsh-vncview)))

;;;###autoload
(defun virsh-list-split (str)
  (let ((pos1 0) (pos2 0) temp id name state)
    (loop while (= (aref str pos1) (string-to-char " "))
          do (incf pos1)
          finally (setf pos2 pos1))
    
    (loop while (/= (aref str pos2) (string-to-char " "))
          do (incf pos2)
          finally
          (setf id (subseq str pos1 pos2))
          (setf pos1 pos2))
    (loop while (= (aref str pos1) (string-to-char " "))
          do (incf pos1)
          finally (setf pos2 pos1))
    (loop while (/= (aref str pos2) (string-to-char " "))
          do (incf pos2)
          finally
          (setf name (subseq str pos1 pos2))
          (setf pos1 pos2))
    (loop while (= (aref str pos1) (string-to-char " "))
          do (incf pos1)
          finally (setf state (subseq str pos1)))
    (list id name state)))

;;;###autoload
(defun virsh-select-host ()
  (cond ((= 0 (length *virsh-hosts*))
         (error "Please configure varialbe *virsh-hosts* first!"))
        ((= 1 (length *virsh-hosts*))
         (car *virsh-hosts*))
        (t (my-select-window *virsh-hosts* :prompt "Pls select the target host: " :delay-seconds 0))))
;;;###autoload
(defun vm-name-with-state (host state)
  (let* ((cmd (shell-command-to-string
               (format "virsh -c qemu+ssh://%s/system list --all" host)))
         (vlist (loop for x in (cddr (split-string cmd "\n"))
                      if (> (length x) 3)
                      collect (virsh-list-split x)))
         (name-list (loop for x in vlist
                          if (string-equal state (nth 2 x))
                          collect (nth 1 x))))
    (and name-list
         (my-select-window name-list :prompt (format "Pls select virtual machine to %s: " name 0)
                           :delay-seconds 0))))

;;;###autoload
(defun virsh-do-action (name state action)
  (let* ((host (virsh-select-host))
         (vm-name (vm-name-with-state host state)))
    (when vm-name
      (funcall action host vm-name))))

;;;###autoload
(defun virsh-start ()
  (interactive)
  (let* ((host (virsh-select-host))
         (vm-name (vm-name-with-state host "shut off")))
    (and vm-name
         (message (shell-command-to-string
                   (format "virsh -c qemu+ssh://%s/system start %s" host vm-name))))))

;;;###autoload
(defun virsh-destroy ()
  (interactive)
  (let* ((host (virsh-select-host))
         (vm-name (vm-name-with-state host "running")))
    (and vm-name
         (message (shell-command-to-string
                   (format "virsh -c qemu+ssh://%s/system destroy %s" host vm-name))))))
         
(defun virsh-vncview ()
  (interactive)
  (let* ((host (virsh-select-host))
         (vm-name (vm-name-with-state host "running")))
    (and vm-name
         (let* ((port-str (shell-command-to-string
                           (format "virsh -c qemu+ssh://%s/system vncdisplay %s" host vm-name)))
                (port (+ 5900 (string-to-int (if (= (aref port-str 0) (string-to-char ":"))
                                                 (subseq port-str 1)
                                               port-str)))))
           (shell-command (format "vncviewer %s:%s &" host port))))))

(defun my-virsh ()
  (interactive)
  (awhen (my-select-window (mapcar #'(lambda (x) (car x)) *virsh-actions*) :delay-seconds 0)
      (awhen (assoc it *virsh-actions*)
             (call-interactively (cadr it)))))
