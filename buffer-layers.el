;;; buffer-layers.el --- Layered Buffers for Buffer Management

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.0
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: buffer-management
;; URL: http://github.com/swflint/buffer-layers

;;; Commentary:
;;


;;; Code:

(require 'cl-lib)

(defvar *buffer-layers* nil
  "List of all defined buffer layers.")

(defvar *buffer-layers-applied* nil
  "List of applied buffer-layers.")

(defun buffer-layer-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-layers-applied*))

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
  "Define a buffer layer named NAME, taking FILES, RUN-ON-APPLY, RUN-ON-REMOVE and BUFFER-TO-SELECT as keyword arguments."
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
         ,(format "Apply buffer-layer %s." name)
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
         ,(format "Remove buffer-layer %s." name)
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
  (interactive (list (completing-read "Buffer Layer Name or Path: " (cl-remove-if #'(lambda (layer)
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

(defun buffer-layer-list ()
  "Produce a list of defined buffer layers."
  (interactive)
  (with-help-window "*Buffer Layers*"
    (when (buffer-live-p "*Buffer Layers*")
      (kill-buffer "*Buffer Layers*"))
    (with-current-buffer "*Buffer Layers*"
      (insert "Defined Buffer Layers:\n\n")
      (dolist (layer *buffer-layers*)
        (insert (format " - %s%s\n" layer (if (buffer-layer-applied-p layer) " (Applied)"
                                            "")))))))

(defun unload-all-buffer-layers ()
  "Unload all loaded buffer layers."
  (interactive)
  (dolist (buffer-layer *buffer-layers-applied*)
    (unload-buffer-layer buffer-layer)))

(defvar buffer-layer-map (make-keymap)
  "Keymap for buffer-layer commands.")

(define-key buffer-layer-map (kbd "l") #'load-buffer-layer)
(define-key buffer-layer-map (kbd "L") #'buffer-layer-list)
(define-key buffer-layer-map (kbd "u") #'unload-buffer-layer)
(define-key buffer-layer-map (kbd "U") #'unload-all-buffer-layers)

(global-set-key (kbd "C-x L") buffer-layer-map)

(add-hook 'kill-emacs-hook #'unload-all-buffer-layers)

(provide 'buffer-layers)

;;; buffer-layers.el ends here
