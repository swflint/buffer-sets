;;; buffer-layers.el --- Layered Buffers for Buffer Management

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.8
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: buffer-management
;; URL: http://github.com/swflint/buffer-layers

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(cl-defstruct buffer-layer
  name
  files
  select
  on-apply
  on-apply-source
  on-remove
  on-remove-source)

(defvar *buffer-layers* nil
  "List of all defined buffer layers.")

(defvar *buffer-layers-applied* nil
  "List of applied buffer-layers.")

(defvar *buffer-layer-definitions* nil
  "List of all buffer layer definitions.")

(defvar *buffer-layer-buffers* nil
  "List of buffers in loaded buffer layers.")

(defvar *buffer-layer-file* "~/.emacs.d/buffer-layer-definitions.el"
  "The file to store buffer layer definitions in.")

(defun buffer-layers-load-definitions-file ()
  "Load buffer layer definitions file."
  (load *buffer-layer-file* t t)
  (message "Loaded Buffer Layer Definitions."))

(defun buffer-layers-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-layers-applied*))

(defun buffer-layer--get-buffer-layer-definition (layer-name)
  (car (cl-remove-if-not (lambda (layer)
			   (eq layer-name (buffer-layer-name layer))) *buffer-layer-definitions*)))

(defun buffer-layer--generate-buffers-list (layer-name)
  (intern (format "*buffer-layer-%s--buffers*" layer-name)))

(cl-defmacro define-buffer-layer (name &key files select on-apply on-remove)
  "Define a buffer layer named NAME, taking FILES, RUN-ON-APPLY, RUN-ON-REMOVE and BUFFER-TO-SELECT as keyword arguments."
  `(progn
     (cl-pushnew ',name *buffer-layers*)
     (cl-pushnew (make-buffer-layer :name ',name
                                    :files ',files
                                    :select ,select
                                    :on-apply-source ',on-apply
                                    :on-remove-source ',on-remove
                                    :on-apply (lambda () ,@on-apply)
                                    :on-remove (lambda () ,@on-remove))
                 *buffer-layer-definitions*
                 :key #'buffer-layer-name)
     (defvar ,(buffer-layer--generate-buffers-list name) nil)
     ',name))

(defun buffer-layers-load-layer (name)
  (interactive (list (intern (completing-read "Layer Name: "
                                              (cl-remove-if #'(lambda (layer) (member layer *buffer-layers-applied*)) *buffer-layers*)
                                              nil t))))
  (let ((layer-definition (buffer-layer--get-buffer-layer-definition name)))
    (if (not (buffer-layer-p layer-definition))
    	(error "Layer Undefined: %s" name)
      (let ((files (buffer-layer-files layer-definition))
            (select (buffer-layer-select layer-definition))
            (on-apply (buffer-layer-on-apply layer-definition))
            (buffers-list (buffer-layer--generate-buffers-list name)))
        (setf (symbol-value buffers-list) (mapcar #'find-file files))
        (funcall on-apply)
        (when (stringp select)
          (switch-to-buffer select))
        (add-to-list '*buffer-layers-applied* name)
        (message "Applied buffer layer %s." name)))))

(defalias 'load-buffer-layer 'buffer-layers-load-layer)

(defun buffer-layers-unload-buffer-layer (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (intern (completing-read "Buffer Layer Name: " *buffer-layers-applied*))))
  (let ((layer-definition (buffer-layer--get-buffer-layer-definition name)))
    (if (not (buffer-layer-p layer-definition))
        (error "Layer Undefined: %s" name)
      (let ((buffers-list (buffer-layer--generate-buffers-list name))
            (on-remove (buffer-layer-on-remove layer-definition)))
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (save-buffer)
                  (kill-buffer buffer)))
              (symbol-value buffers-list))
        (funcall on-remove)
        (setf (symbol-value buffers-list) nil)
        (setq *buffer-layers-applied* (delq name *buffer-layers-applied*))
        (message "Removed Buffer Layer: %s" name)))))

(defun buffer-layers-list ()
  "Produce a list of defined buffer layers."
  (interactive)
  (when (buffer-live-p "*Buffer Layers*")
    (kill-buffer "*Buffer Layers*"))
  (with-help-window "*Buffer Layers*"
    (with-current-buffer "*Buffer Layers*"
      (insert "Defined Buffer Layers:\n\n")
      (dolist (layer *buffer-layers*)
        (if (not (buffer-layers-applied-p layer))
            (insert (format " - %s\n" layer))
          (insert (format " - %s (Applied)\n" layer)))
        (dolist (buffer (symbol-value (buffer-layer--generate-buffers-list layer)))
          (if (null (get-buffer-window-list buffer nil t))
              (insert (format "    - %s\n" (buffer-name buffer)))
            (insert (format "    - %s (visible)\n" (buffer-name buffer)))))))))

(defun buffer-layers-unload-all-buffer-layers ()
  "Unload all loaded buffer layers."
  (interactive)
  (dolist (buffer-layer *buffer-layers-applied*)
    (buffer-layers-unload-buffer-layer buffer-layer)))

(defun buffer-layers-create-layer (name)
  "Create a new layer."
  (interactive "SNew Layer Name: ")
  (when (not (member name *buffer-layers*))
    (pushnew name *buffer-layers*)
    (setf (symbol-value (buffer-layer--generate-buffers-list name)) nil)
    (pushnew (make-buffer-layer :name name
                                :on-apply (lambda () nil)
                                :on-remove (lambda () nil)) *buffer-layer-definitions*)))

(defun buffer-layers-add-file-to-layer (name file)
  "Add a file to the layer."
  (interactive (list
                (intern (completing-read "Layer: " *buffer-layers* nil t))
                (read-file-name "File Name: ")))
  (let ((layer (buffer-layer--get-buffer-layer-definition name)))
    (setf (buffer-layer-files layer) (append (buffer-layer-files layer) (list file)))))

(defun buffer-layers-add-buffer-to-layer (name buffer)
  "Add a buffer to the given layer."
  (interactive (list
                (intern (completing-read "Layer: " *buffer-layers* nil t))
                (get-buffer (read-buffer "Buffer: " (current-buffer)))))
  (let ((layer (buffer-layer--get-buffer-layer-definition name))
        (file (buffer-file-name buffer)))
    (setf (buffer-layer-files layer) (append (buffer-layer-files layer) (list file)))))

(defun buffer-layers-edit-load-actions (layer)
  "Edit the actions to be preformed on buffer layer load."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-edit-remove-actions (layer)
  "Edit the actions to be preformed on buffer layer removal."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-set-buffer-to-select (layer)
  "Set the buffer to automatically select."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-save (the-layer)
  "Save defined buffer layers."
  (interactive)
  (insert (format "%S\n\n" (let ((name (buffer-layer-name the-layer))
                                 (files (buffer-layer-files the-layer))
                                 (select (buffer-layer-select the-layer))
                                 (on-apply (buffer-layer-on-apply-source the-layer))
                                 (on-remove (buffer-layer-on-remove-source the-layer)))
                             `(define-buffer-layer ,name
                                :files ,files
                                :select ,select
                                :on-apply ,on-apply
                                :on-remove ,on-remove)))))

(defun buffer-layers-save-definitions ()
  (with-current-buffer (find-file *buffer-layer-file*)
    (kill-region (buffer-end -1) (buffer-end 1))
    (mapc #'buffer-layers-save (reverse *buffer-layer-definitions*))
    (save-buffer)
    (kill-buffer))
  (message "Saved Buffer Layer Definitions."))

(defvar buffer-layers-map (make-keymap)
  "Keymap for buffer-layer commands.")

(define-key buffer-layers-map (kbd "l") #'buffer-layers-load-layer)
(define-key buffer-layers-map (kbd "L") #'buffer-layers-list)
(define-key buffer-layers-map (kbd "u") #'buffer-layers-unload-buffer-layer)
(define-key buffer-layers-map (kbd "U") #'buffer-layers-unload-all-buffer-layers)
(define-key buffer-layers-map (kbd "c") #'buffer-layers-create-layer)
(define-key buffer-layers-map (kbd "f") #'buffer-layers-add-file-to-layer)
(define-key buffer-layers-map (kbd "b") #'buffer-layers-add-buffer-to-layer)
(define-key buffer-layers-map (kbd "a") #'buffer-layers-edit-load-actions)
(define-key buffer-layers-map (kbd "r") #'buffer-layers-edit-remove-actions)
(define-key buffer-layers-map (kbd "s") #'buffer-layers-set-buffer-to-select)
(define-key buffer-layers-map (kbd "C-s") #'buffer-layers-save)

(define-minor-mode buffer-layers-mode
  "A mode for managing layers of buffers."
  :lighter " BLM" :global t :variable buffer-layers-mode-p
  (if buffer-layers-mode-p
      (progn
        (buffer-layers-load-definitions-file)
        (define-key ctl-x-map (kbd "L") buffer-layers-map)
        (add-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers)
        (add-hook 'kill-emacs-hook #'buffer-layers-save-definitions))
    (progn
      (buffer-layers-save-definitions)
      (define-key ctl-x-map (kbd "L") nil)
      (remove-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers)
      (remove-hook 'kill-emacs-hook #'buffer-layers-save-definitions))))

(provide 'buffer-layers)

;;; buffer-layers.el ends here
