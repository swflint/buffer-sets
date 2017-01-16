# Buffer Sets has moved [here](https://git.flintfam.org/swf-projects/buffer-sets)!

This is Buffer Layers, a simple, layer-based buffer management system.

It works by defining buffer layers using a fairly simple macro.  The following would be put in a file called `org.layer`

```elisp
;; -*- emacs-lisp -*-
(define-buffer-layer org
  :files ("~/org/"
          "~/org/main.org")
  :buffer-to-select "main.org"
  :run-on-apply ((my/find-current-notes-file)))
```

It can be loaded with `(load-buffer-layer "/path/to/org.layer" nil)`.  If the final `nil` is changed to `t`, it will load and apply the layer.

Buffer Layer Definitions take the following arguments:

 - `:files`: A list.  This is the list of files that are loaded when the buffer layer is applied.
 - `:buffer-to-select`: The buffer to select after files are loaded, and the given forms to run on application are executed.
 - `:run-on-apply`: This is a list of forms to be executed in between finding files and selecting the given buffer.
 - `:run-on-remove`: This is a list of forms to be executed after killing the buffers that have been loaded.

To manipulate buffer layers, execute `buffer-layer-mode`, and then you can use the following keybindings:

 - `C-x L l`: Load a buffer layer, if defined, otherwise, load from the given file.
 - `C-x L u`: Unload a loaded buffer layer.
 - `C-x L U`: Unload all loaded buffer layers.
 - `C-x L L`: List defined buffer layers, noting if they've been applied.

The following are the user-facing functions:

 - `define-buffer-layer`
 - `buffer-layer-load-buffer-layer`, also known as `load-buffer-layer`
 - `buffer-layers-unload-buffer-layer`
 - `buffer-layers-list`
 - `buffer-layers-unload-all-buffer-layers`
 - `buffer-layers-mode`

On enabling `buffer-layer-mode`, the map is placed onto `C-x L`, and `buffer-layers-unload-all-buffer-layers` is added to the `kill-emacs-hook`, and on disabling the mode, they are removed.

[![MELPA](https://melpa.org/packages/buffer-sets-badge.svg)](https://melpa.org/#/buffer-sets)
