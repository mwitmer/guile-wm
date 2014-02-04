(define-module (guile-wm module help)
  #:use-module (guile-wm command)
  #:use-module (guile-wm shared)
  #:use-module (language command spec))

(use-wm-modules message menu)

(define-command (help)
  "List all the commands and show documentation for the one selected
by the user"
  (menu "Select a command:"
        (map (lambda (c) (cons (symbol->string c) (command-documentation c)))
             (hash-map->list (lambda (k v) k) commands))
        (lambda (doc)
          (if doc (message doc) (message "Not documented")))))

(define-command (document (cmd #:symbol))
  "Display docstring for command CMD"
  (or
   (and=> (command-documentation cmd) message)
   (message (format #f "No command with name ~a" cmd))))

