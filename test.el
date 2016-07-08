(setq the-layer
      (make-buffer-layer
       :name 'org
       :files '("~/org/"
                "~/org/agenda.org"
                "~/org/bookmarks.org"
                "~/org/college.org"
                "~/org/index.org"
                "~/org/personal-log.org"
                "~/org/quotes.org"
                "~/org/recipes.org"
                "~/org/school.org"
                "~/org/snips.org"
                "~/org/travel-list.org"
                "~/org/main.org")
       :select "main.org"
       :on-apply (lambda ()
                   (my/find-current-notes-file))
       :on-apply-source '((my/find-current-notes-file))))


(insert (format "%S\n\n" (let ((name (buffer-layer-name the-layer))
                               (files (buffer-layer-files the-layer))
                               (select (buffer-layer-select the-layer))
                               (on-apply (buffer-layer-on-apply-source the-layer))
                               (on-remove (buffer-layer-on-remove-source the-layer)))
                           `(define-buffer-layer* ,name
                              :files ,files
                              :select ,select
                              :on-apply ,on-apply
                              :on-remove ,on-remove))))

(define-buffer-layer org
  :files ("~/org/"
          "~/org/agenda.org"
          "~/org/bookmarks.org"
          "~/org/college.org"
          "~/org/index.org"
          "~/org/personal-log.org"
          "~/org/quotes.org"
          "~/org/recipes.org"
          "~/org/school.org"
          "~/org/snips.org"
          "~/org/travel-list.org"
          "~/org/main.org")
  :select "main.org"
  :on-apply ((my/find-current-notes-file)))

*buffer-layer-definitions*
