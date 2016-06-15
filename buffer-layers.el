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

(cl-defmacro define-buffer-layer-new (name &key files run-on-apply run-on-remove buffer-to-select)
  `(progn
     (cl-pushnew '(,name
                   (:files ,@files)
                   (:on-apply ,@run-on-apply)
                   (:on-remove ,@run-on-remove)
                   ,@(when buffer-to-select
                       (list :select-buffer buffer-to-select))
                   ,@(when run-on-apply
                       (list :on-apply-lambda (eval `(lambda () ,@run-on-apply))))
                   ,@(when run-on-remove
                       (list :on-remove-lambda (eval `(lambda () ,@run-on-remove)))))
                 *buffer-layer-definitions* :key #'car)
     (cl-pushnew ',name *buffer-layers*)
     ',name))

(defun buffer-layers-load-layer (name)
  (interactive (list (completing-read "Layer Name: " *buffer-layers* ;; (cl-remove-if #'(lambda (layer) (member layer *buffer-layers-applied*)) *buffer-layers*)
                                      nil t)))
  (let* ((record (rest (assoc name *buffer-layer-definitions*)))
         (files (rest (assoc :files record)))
         (on-apply (rest (assoc :on-apply-lambda record)))
         (buffer-select (rest (assoc :select-buffer record))))
    (let ((buffer-list nil))
      (dolist (file files)
        (cl-pushnew (find-file file) buffer-list))
      (add-to-list '*buffer-layer-buffers* `(,name ,@buffer-list)))
    (when (functionp on-apply)
      (funcall on-apply))
    (when buffer-select
      (switch-to-buffer buffer-select))
    (message "Applied Buffer Layer %s." name)))

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
  (interactive))

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
