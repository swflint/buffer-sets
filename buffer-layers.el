;;; buffer-sets.el --- Configurable sets of buffers

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
;; (define-buffer-set org
;;   :files ("~/org/"
;;           "~/org/main.org")
;;   :buffer-to-select "main.org"
;;   :run-on-apply ((my/find-current-notes-file)))
;; ```
;;
;; It can be loaded with `(load-buffer-set "/path/to/org.layer" nil)`.  If the final `nil` is changed to `t`, it will load and apply the layer.
;;
;; Buffer Layer Definitions take the following arguments:
;;
;;  - `:files`: A list.  This is the list of files that are loaded when the buffer layer is applied.
;;  - `:buffer-to-select`: The buffer to select after files are loaded, and the given forms to run on application are executed.
;;  - `:run-on-apply`: This is a list of forms to be executed in between finding files and selecting the given buffer.
;;  - `:run-on-remove`: This is a list of forms to be executed after killing the buffers that have been loaded.
;;
;; To manipulate buffer layers, execute `buffer-set-mode`, and then you can use the following keybindings:
;;
;;  - `C-x L l`: Load a buffer layer, if defined, otherwise, load from the given file.
;;  - `C-x L u`: Unload a loaded buffer layer.
;;  - `C-x L U`: Unload all loaded buffer layers.
;;  - `C-x L L`: List defined buffer layers, noting if they've been applied.
;;
;; The following are the user-facing functions:
;;
;;  - `define-buffer-set`
;;  - `buffer-set-load-buffer-set`, also known as `load-buffer-set`
;;  - `buffer-sets-unload-buffer-set`
;;  - `buffer-sets-list`
;;  - `buffer-sets-unload-all-buffer-sets`
;;  - `buffer-sets-mode`
;;
;; On enabling `buffer-set-mode`, the map is placed onto `C-x L`, and `buffer-sets-unload-all-buffer-sets` is added to the `kill-emacs-hook`, and on disabling the mode, they are removed.


;;; Code:

(require 'cl-lib)

(defvar *buffer-sets* nil
  "List of all defined buffer layers.")

(defvar *buffer-sets-applied* nil
  "List of applied buffer-sets.")

(defun buffer-sets-applied-p (layer)
  "Returns true if LAYER is applied."
  (member layer *buffer-sets-applied*))

(defun buffer-sets--applier-name (name)
  "Generate name to apply a buffer layer based on NAME."
  (intern (format "apply-buffer-sets-%s" name)))

(defun buffer-sets--remover-name (name)
  "Generate name to remove a buffer layer based on NAME."
  (intern (format "remove-buffer-sets-%s" name)))

(defun buffer-sets--buffer-list-name (name)
  "Generate name to contain buffer layer buffer list based on NAME."
  (intern (format "*buffer-sets-%s-buffers*" name)))

(cl-defmacro define-buffer-set (name &key files run-on-apply run-on-remove buffer-to-select)
  "Define a buffer layer named NAME, taking FILES, RUN-ON-APPLY, RUN-ON-REMOVE and BUFFER-TO-SELECT as keyword arguments."
  (let ((applier (buffer-sets--applier-name name))
        (remover (buffer-sets--remover-name name))
        (buffers-list (buffer-sets--buffer-list-name name))
        (files-list (cons 'list
                          (when (not (null files))
                            (mapcar #'(lambda (name)
                                        (format "%s" name)) files)))))
    `(progn
       (add-to-list '*buffer-sets* ',name)
       (defvar ,buffers-list nil)
       (defun ,applier ()
         ,(format "Apply buffer-set %s." name)
         (interactive)
         (mapcar #'(lambda (file)
                     (add-to-list ',buffers-list (find-file file)))
                 ,files-list)
         ,@run-on-apply
         (when (not (null ,buffer-to-select))
           (switch-to-buffer ,buffer-to-select))
         (add-to-list '*buffer-sets-applied* ',name)
         (message "Applied Buffer Layer %s" ',name))
       (defun ,remover ()
         ,(format "Remove buffer-set %s." name)
         (interactive)
         (mapc #'(lambda (buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (save-buffer)
                       (kill-buffer))))
               ,buffers-list)
         ,@run-on-remove
         (setq ,buffers-list nil)
         (setq *buffer-sets-applied* (delq ',name *buffer-sets-applied*))
         (message "Removed Buffer Layer %s" ',name))
       (setq current-buffer-applier ',applier)
       (list ',applier ',remover))))

(defun buffer-sets-load-buffer-set (name-or-path load-it-p)
  "Load a buffer named NAME-OR-PATH, and if a file, apply if LOAD-IT-P is true."
  (interactive (list (completing-read "Buffer Layer Name or Path: " (cl-remove-if #'(lambda (layer)
                                                                                      (member layer *buffer-sets-applied*))
                                                                                  *buffer-sets*))
                     nil))
  (if (functionp (buffer-sets--applier-name name-or-path))
      (funcall (buffer-sets--applier-name name-or-path))
    (load name-or-path)
    (when load-it-p
      (funcall current-buffer-applier))))

(defalias 'load-buffer-set 'buffer-sets-load-buffer-set)

(defun buffer-sets-unload-buffer-set (name)
  "Unload Buffer Layer named NAME."
  (interactive (list (completing-read "Buffer Layer Name: " *buffer-sets-applied*)))
  (funcall (buffer-sets--remover-name name)))

(defun buffer-sets-list ()
  "Produce a list of defined buffer layers."
  (interactive)
  (when (buffer-live-p "*Buffer Layers*")
    (kill-buffer "*Buffer Layers*"))
  (with-help-window "*Buffer Layers*"
    (with-current-buffer "*Buffer Layers*"
      (insert "Defined Buffer Layers:\n\n")
      (dolist (layer *buffer-sets*)
        (if (not (buffer-sets-applied-p layer))
            (insert (format " - %s\n" layer))
          (insert (format " - %s (Applied)\n" layer)))
        (dolist (buffer (symbol-value (buffer-sets--buffer-list-name layer)))
          (if (null (get-buffer-window-list buffer nil t))
              (insert (format "    - %s\n" (buffer-name buffer)))
            (insert (format "    - %s (visible)\n" (buffer-name buffer)))))))))

(defun buffer-sets-unload-all-buffer-sets ()
  "Unload all loaded buffer layers."
  (interactive)
  (dolist (buffer-set *buffer-sets-applied*)
    (buffer-sets-unload-buffer-set buffer-set)))

(defvar buffer-sets-map (make-keymap)
  "Keymap for buffer-set commands.")

(define-key buffer-sets-map (kbd "l") #'buffer-sets-load-buffer-set)
(define-key buffer-sets-map (kbd "L") #'buffer-sets-list)
(define-key buffer-sets-map (kbd "u") #'buffer-sets-unload-buffer-set)
(define-key buffer-sets-map (kbd "U") #'buffer-sets-unload-all-buffer-sets)

(define-minor-mode buffer-sets-mode
  "A mode for managing configurable sets of buffers."
  :lighter " BSS" :global t :variable buffer-sets-mode-p
  (if buffer-sets-mode-p
      (progn
        (define-key ctl-x-map (kbd "S") buffer-sets-map)
        (add-hook 'kill-emacs-hook #'buffer-sets-unload-all-buffer-sets))
    (progn
      (define-key ctl-x-map (kbd "S") nil)
      (remove-hook 'kill-emacs-hook #'buffer-sets-unload-all-buffer-sets))))

(provide 'buffer-sets)

;;; buffer-sets.el ends here
