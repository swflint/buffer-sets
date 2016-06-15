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

It can be loaded with `(load-buffer-layer "/path/to/org.layer" nil)`.  If the final `nil` is changed to true, it will load and apply the layer.

To manipulate buffer layers, execute `buffer-layer-mode`, and then you can use the following keybindings:

 - `C-x L l` Load a buffer layer, if defined, otherwise, load from the given file.
 - `C-x L u` Unload a loaded buffer layer.
 - `C-x L U` Unload all loaded buffer layers.
 - `C-x L L` List defined buffer layers, noting if they've been applied.
