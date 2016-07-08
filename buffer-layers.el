;;; buffer-layers.el --- Layered Buffers for Buffer Management

;; Copyright (C) 2016 Samuel Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.7
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: buffer-management
;; URL: http://github.com/swflint/buffer-layers

;;; Commentary:
;;
;; This is Buffer Layers, a simple, layer-based buffer management system.
;;
;; It works by defining buffer layers using a fairly simple macro.  The following would be put in a file called `org.layer`
;;
;; ```elisp
;; ;; -*- emacs-lisp -*-
;; (define-buffer-layer org
;;   :files ("~/org/"
;;           "~/org/main.org")
;;   :buffer-to-select "main.org"
;;   :run-on-apply ((my/find-current-notes-file)))
;; ```
;;
;; It can be loaded with `(load-buffer-layer "/path/to/org.layer" nil)`.  If the final `nil` is changed to `t`, it will load and apply the layer.
;;
;; Buffer Layer Definitions take the following arguments:
;;
;;  - `:files`: A list.  This is the list of files that are loaded when the buffer layer is applied.
;;  - `:buffer-to-select`: The buffer to select after files are loaded, and the given forms to run on application are executed.
;;  - `:run-on-apply`: This is a list of forms to be executed in between finding files and selecting the given buffer.
;;  - `:run-on-remove`: This is a list of forms to be executed after killing the buffers that have been loaded.
;;
;; To manipulate buffer layers, execute `buffer-layer-mode`, and then you can use the following keybindings:
;;
;;  - `C-x L l`: Load a buffer layer, if defined, otherwise, load from the given file.
;;  - `C-x L u`: Unload a loaded buffer layer.
;;  - `C-x L U`: Unload all loaded buffer layers.
;;  - `C-x L L`: List defined buffer layers, noting if they've been applied.
;;
;; The following are the user-facing functions:
;;
;;  - `define-buffer-layer`
;;  - `buffer-layer-load-buffer-layer`, also known as `load-buffer-layer`
;;  - `buffer-layers-unload-buffer-layer`
;;  - `buffer-layers-list`
;;  - `buffer-layers-unload-all-buffer-layers`
;;  - `buffer-layers-mode`
;;
;; On enabling `buffer-layer-mode`, the map is placed onto `C-x L`, and `buffer-layers-unload-all-buffer-layers` is added to the `kill-emacs-hook`, and on disabling the mode, they are removed.


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

(defun buffer-layers-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-layers-applied*))

(defun buffer-layer--get-buffer-layer-definition (layer-name)
  (first (cl-remove-if-not (lambda (layer)
			     (eq layer-name (buffer-layer-name layer))) *buffer-layer-definitions*)))

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
     ',name))

(defun buffer-layers-load-layer (name)
  (interactive (list (completing-read "Layer Name: " *buffer-layers*
                                      ;; (cl-remove-if #'(lambda (layer) (member layer *buffer-layers-applied*)) *buffer-layers*)
                                      nil t)))
  (let ((layer-definition (buffer-layer--get-buffer-layer-definition name)))
    (if (null layer-definition)
	(error "Layer Undefined: %s" name)
      (progn))))

(defalias 'load-buffer-layer 'buffer-layers-load-layer)

(defun buffer-layers-unload-buffer-layer (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (completing-read "Buffer Layer Name: " *buffer-layers-applied*)))
  (funcall (buffer-layers--remover-name name)))

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
        (dolist (buffer (symbol-value (buffer-layers--buffer-list-name layer)))
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
  (interactive "SNew Layer Name: "))

(defun buffer-layers-add-file-to-layer (name file)
  "Add a file to the layer."
  (interactive (list
                (completing-read "Layer: " *buffer-layers* nil t)
                (read-file-name "File Name: "))))

(defun buffer-layers-add-buffer-to-layer (name buffer)
  "Add a buffer to the given layer."
  (interactive (list
                (completing-read "Layer: " *buffer-layers* nil t)
                (read-buffer "Buffer: " (current-buffer)))))

(defun buffer-layers-edit-load-actions (layer)
  "Edit the actions to be preformed on buffer layer load."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-edit-remove-actions (layer)
  "Edit the actions to be preformed on buffer layer removal."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-set-buffer-to-select (layer)
  "Set the buffer to automatically select."
  (interactive (list (completing-read "Layer: " *buffer-layers* nil t))))

(defun buffer-layers-save ()
  "Save defined buffer layers."
  (interactive)

  (insert (format "%S\n\n" (let ((name (buffer-layer-name the-layer))
                                 (files (buffer-layer-files the-layer))
                                 (select (buffer-layer-select the-layer))
                                 (on-apply (buffer-layer-on-apply-source the-layer))
                                 (on-remove (buffer-layer-on-remove-source the-layer)))
                             `(define-buffer-layer* ,name
                                :files ,files
                                :select ,select
                                :on-apply ,on-apply
                                :on-remove ,on-remove)))))

(defvar buffer-layers-map (make-keymap)
  "Keymap for buffer-layer commands.")

(define-key buffer-layers-map (kbd "l") #'buffer-layers-load-buffer-layer)
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
        (define-key ctl-x-map (kbd "L") buffer-layers-map)
        (add-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers))
    (progn
      (define-key ctl-x-map (kbd "L") nil)
      (remove-hook 'kill-emacs-hook #'buffer-layers-unload-all-buffer-layers))))

(provide 'buffer-layers)

;;; buffer-layers.el ends here
