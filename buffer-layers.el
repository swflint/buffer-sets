;;; buffer-layers.el --- Layered Buffers for Buffer Management

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.0
;; Package-Requires: (cl)
;; Keywords: buffer-management
;; URL: http://github.com/swflint/buffer-layers

;;; Commentary:
;;


;;; Code:

(require 'cl)

(defvar *buffer-layers* nil)

(defvar *buffer-layers-applied* nil)

(defun buffer-layer--applier-name (name)
  "Generate name to apply a buffer layer based on NAME."
  (intern (format "apply-buffer-layer-%s" name)))

(defun buffer-layer--remover-name (name)
  "Generate name to remove a buffer layer based on NAME."
  (intern (format "remove-buffer-layer-%s" name)))

(defun buffer-layer--buffer-list-name (name)
  "Generate name to contain buffer layer buffer list based on NAME."
  (intern (format "*buffer-layer-%s-buffers*" name)))

(cl-defmacro define-buffer-layer (name &key files run-on-apply run-on-remove buffer-to-select)
  (let ((applier (buffer-layer--applier-name name))
        (remover (buffer-layer--remover-name name))
        (buffers-list (buffer-layer--buffer-list-name name))
        (files-list (cons 'list
                          (when (not (null files))
                            (mapcar #'(lambda (name)
                                        (format "%s" name)) files)))))
    `(progn
       (add-to-list '*buffer-layers* ',name)
       (defvar ,buffers-list nil)
       (defun ,applier ()
         (interactive)
         (mapcar #'(lambda (file)
                     (add-to-list ',buffers-list (find-file file)))
                 ,files-list)
         ,@run-on-apply
         (when (not (null ,buffer-to-select))
           (switch-to-buffer ,buffer-to-select))
         (add-to-list '*buffer-layers-applied* ',name)
         (message "Applied Buffer Layer %s" ',name))
       (defun ,remover ()
         (interactive)
         (mapc #'(lambda (buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (save-buffer)
                       (kill-buffer))))
               ,buffers-list)
         ,@run-on-remove
         (setq ,buffers-list nil)
         (setq *buffer-layers-applied* (delq ',name *buffer-layers-applied*))
         (message "Removed Buffer Layer %s" ',name))
       (setq current-buffer-applier ',applier)
       (list ',applier ',remover))))

(defun load-buffer-layer (name-or-path load-it-p)
  "Load a buffer named NAME-OR-PATH, and if a file, apply if LOAD-IT-P is true."
  (interactive (list (completing-read "Buffer Layer Name or Path: " (remove-if #'(lambda (layer)
                                                                                   (member layer *buffer-layers-applied*))
                                                                               *buffer-layers*))
                     nil))
  (if (functionp (buffer-layer--applier-name name-or-path))
      (funcall (buffer-layer--applier-name name-or-path))
    (load name-or-path)
    (when load-it-p
      (funcall current-buffer-applier))))

(defun unload-buffer-layer (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (completing-read "Buffer Layer Name: " *buffer-layers-applied*)))
  (funcall (buffer-layer--remover-name name)))

(defun unload-all-buffer-layers ()
  "Unload all loaded buffer layers."
  (interactive)
  (dolist (buffer-layer *buffer-layers-applied*)
    (unload-buffer-layer buffer-layer)))

(defvar buffer-layer-map (make-keymap))

(define-key buffer-layer-map (kbd "l") #'load-buffer-layer)
(define-key buffer-layer-map (kbd "u") #'unload-buffer-layer)
(define-key buffer-layer-map (kbd "U") #'unload-all-buffer-layers)

(global-set-key (kbd "C-x L") buffer-layer-map)

(add-hook 'kill-emacs-hook #'unload-all-buffer-layers)

(provide 'buffer-layers)

;;; buffer-layers.el ends here
