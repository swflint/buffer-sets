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

(defvar *buffer-layer-definitions* nil
  "List of buffer layer definitions.")

(defun buffer-layer-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-layers-applied*))

(defun buffer-layer-defined-p (layer)
  "Returns true if LAYER is defined."
  (null (member layer *buffer-layers*)))

(defun buffer-layer--buffer-list-name (name)
  "Generate name to contain buffer layer buffer list based on NAME."
  (intern (format "*buffer-layer-%s-buffers*" name)))

(cl-defmacro define-buffer-layer (name &key files run-on-apply run-on-remove buffer-to-select)
  "Define a buffer layer named NAME, taking FILES, RUN-ON-APPLY, RUN-ON-REMOVE and BUFFER-TO-SELECT as keyword arguments."
  (let ((buffers-list (buffer-layer--buffer-list-name name))
        ;; (files-list (cons 'list
        ;;                   (when (not (null files))
        ;;                     (mapcar #'(lambda (name)
        ;;                                 (format "%s" name)) files))))
        )
    `(progn
       (add-to-list '*buffer-layers* ',name)
       (defvar ,buffers-list nil)
       (add-to-list '*buffer-layer-definitions*
                    '(,name
                      ,files
                      ,buffer-to-select
                      ,run-on-apply
                      ,run-on-remove))
       ;; (defun ,applier ()
       ;;   ,(format "Apply buffer-layer %s." name)
       ;;   (interactive)
       ;;   (mapcar #'(lambda (file)
       ;;               (add-to-list ',buffers-list (find-file file)))
       ;;           ,files-list)
       ;;   ,@run-on-apply
       ;;   (when (not (null ,buffer-to-select))
       ;;     (switch-to-buffer ,buffer-to-select))
       ;;   (add-to-list '*buffer-layers-applied* ',name)
       ;;   (message "Applied Buffer Layer %s" ',name))
       ;; (defun ,remover ()
       ;;   ,(format "Remove buffer-layer %s." name)
       ;;   (interactive)
       ;;   (mapc #'(lambda (buffer)
       ;;             (when (buffer-live-p buffer)
       ;;               (with-current-buffer buffer
       ;;                 (save-buffer)
       ;;                 (kill-buffer))))
       ;;         ,buffers-list)
       ;;   ,@run-on-remove
       ;;   (setq ,buffers-list nil)
       ;;   (setq *buffer-layers-applied* (delq ',name *buffer-layers-applied*))
       ;;   (message "Removed Buffer Layer %s" ',name))
       (setq current-buffer-layer ',name)
       ',name)))

(defun load-buffer-layer (name-or-path load-it-p)
  "Load a buffer named NAME-OR-PATH, and if a file, apply if LOAD-IT-P is true."
  (interactive (list (completing-read "Buffer Layer Name or Path: " (cl-remove-if #'(lambda (layer)
                                                                                      (member layer *buffer-layers-applied*))
                                                                                  *buffer-layers*))
                     nil))
  (if (buffer-layer-defined-p name-or-path)
      (let* ((record (assoc name-or-path *buffer-layer-definitions*))
             (files-list (nth 1 record))
             (selected (nth 2 record))
             (on-apply (nth 3 record)))
        (mapcar #'(lambda (file)
                    (add-to-list (buffer-layer--buffer-list-name name-or-path)
                                 (find-file file)))
                files-list)
        (funcall (eval `(lambda () #'on-apply nil)))
        (when (not (null selected))
          (switch-to-buffer selected))
        (add-to-list '*buffer-layers-applied* name-or-path)
        (message "Applied buffer layer %s." name-or-path))
    (progn
      (load name-or-path)
      (when load-it-p
        (load-buffer-layer current-buffer-layer t)))))

(defun unload-buffer-layer (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (completing-read "Buffer Layer Name: " *buffer-layers-applied*)))
  (let ((on-remove (nth 4 (assoc name *buffer-layer-definitions*))))
    (mapc #'(lambda (buffer)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (save-buffer)
                  (kill-buffer))))
          (symbol-value (buffer-layer--buffer-list-name name)))
    (setq (buffer-layer--buffer-list-name name) nil)
    (setq *buffer-layers-applied* (delq name *buffer-layers-applied*))
    (funcall (eval `(lambda () ,@on-remove nil)))
    (message "Removed Buffer Layer %s." name)))

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

(define-minor-mode buffer-layer-mode
  "A mode for managing layers of buffers."
  :lighter " BLM" :global t :variable buffer-layer-mode-p
  (if buffer-layer-mode-p
      (progn
        (define-key ctl-x-map (kbd "L") buffer-layer-map)
        (add-hook 'kill-emacs-hook #'unload-all-buffer-layers))
    (progn
      (define-key ctl-x-map (kbd "L") nil)
      (remove-hook 'kill-emacs-hook #'unload-all-buffer-layers))))

(provide 'buffer-layers)

;;; buffer-layers.el ends here
