;;; buffer-layers.el --- Layered Buffers for Buffer Management

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.5
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: buffer-management
;; URL: http://github.com/swflint/buffer-layers

;;; Commentary:
;;
;; This is Buffer Layers, a simple, layer-based buffer management system.
;;
;; It works by defining buffer layers using a fairly simple macro.  An example is as follows:
;;
;; (define-buffer-layer org
;;   :files ("~/org/"
;;           "~/org/main.org")
;;   :buffer-to-select "main.org"
;;   :run-on-apply ((my/find-current-notes-file)))


;;; Code:

(require 'cl-lib)

(defvar *buffer-layers* nil
  "List of all defined buffer layers.")

(defvar *buffer-layers-applied* nil
  "List of applied buffer-layers.")

(defun buffer-layers-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-layers-applied*))

(defun buffer-layers--applier-name (name)
  "Generate name to apply a buffer layer based on NAME."
  (intern (format "apply-buffer-layers-%s" name)))

(defun buffer-layers--remover-name (name)
  "Generate name to remove a buffer layer based on NAME."
  (intern (format "remove-buffer-layers-%s" name)))

(defun buffer-layers--buffer-list-name (name)
  "Generate name to contain buffer layer buffer list based on NAME."
  (intern (format "*buffer-layers-%s-buffers*" name)))

(cl-defmacro define-buffer-layer (name &key files run-on-apply run-on-remove buffer-to-select)
  "Define a buffer layer named NAME, taking FILES, RUN-ON-APPLY, RUN-ON-REMOVE and BUFFER-TO-SELECT as keyword arguments."
  (let ((applier (buffer-layers--applier-name name))
        (remover (buffer-layers--remover-name name))
        (buffers-list (buffer-layers--buffer-list-name name))
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

(defun buffer-layers-load-buffer-layer (name-or-path load-it-p)
  "Load a buffer named NAME-OR-PATH, and if a file, apply if LOAD-IT-P is true."
  (interactive (list (completing-read "Buffer Layer Name or Path: " (cl-remove-if #'(lambda (layer)
                                                                                      (member layer *buffer-layers-applied*))
                                                                                  *buffer-layers*))
                     nil))
  (if (functionp (buffer-layers--applier-name name-or-path))
      (funcall (buffer-layers--applier-name name-or-path))
    (load name-or-path)
    (when load-it-p
      (funcall current-buffer-applier))))

(defalias 'load-buffer-layer 'buffer-layers-load-buffer-layer)

(defun buffer-layers-unload-buffer-layer (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (completing-read "Buffer Layer Name: " *buffer-layers-applied*)))
  (funcall (buffer-layers--remover-name name)))

(defun buffer-layers-list ()
  "Produce a list of defined buffer layers."
  (interactive)
  (with-help-window "*Buffer Layers*"
    (when (buffer-live-p "*Buffer Layers*")
      (kill-buffer "*Buffer Layers*"))
    (with-current-buffer "*Buffer Layers*"
      (insert "Defined Buffer Layers:\n\n")
      (insert (with-temp-buffer
                (dolist (layer *buffer-layers*)
                  (insert (format " - %s%s\n" layer (if (buffer-layers-applied-p layer) " (Applied)"
                                                      ""))))
                (sort-lines nil (buffer-end -1) (buffer-end +1))
                (buffer-string))))))

(defun buffer-layers-unload-all-buffer-layers ()
  "Unload all loaded buffer layers."
  (interactive)
  (dolist (buffer-layer *buffer-layers-applied*)
    (buffer-layers-unload-buffer-layer buffer-layer)))

(defvar buffer-layers-map (make-keymap)
  "Keymap for buffer-layer commands.")

(define-key buffer-layers-map (kbd "l") #'buffer-layers-load-buffer-layer)
(define-key buffer-layers-map (kbd "L") #'buffer-layers-list)
(define-key buffer-layers-map (kbd "u") #'buffer-layers-unload-buffer-layer)
(define-key buffer-layers-map (kbd "U") #'buffer-layers-unload-all-buffer-layers)

(define-minor-mode buffer-layers-mode
  "A mode for managing layers of buffers."
  :lighter " BLM" :global t :variable buffer-layers-mode-p
  (if buffer-layers-mode-p
      (progn
        (define-key ctl-x-map (kbd "L") buffer-layers-map)
        (add-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers))
    (progn
      (define-key ctl-x-map (kbd "L") nil)
      (remove-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers))))

(provide 'buffer-layers)

;;; buffer-layers.el ends here
