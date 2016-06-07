This is Buffer Layers, a simple, layer-based buffer management system.

It works by defining buffer layers using a fairly simple macro.  An example is as follows:

```elisp
(define-buffer-layer org
  :files ("~/org/"
          "~/org/main.org")
  :buffer-to-select "main.org"
  :run-on-apply ((my/find-current-notes-file)))
```

