(define-module (guile-wm module help)
  #:use-module (guile-wm command)
  #:use-module (guile-wm shared)
  #:use-module (language command spec))

(use-wm-modules message menu)

(define (format-type type)
  (case type
    ((#:string) "string")
    ((#:symbol) "symbol")
    ((#:number) "number")))

(define (format-arg arg)
  (format #f "(^(color cyan)~a^(color white): ^(color salmon)~a^(color white)) "
          (car arg) (format-type (cdr arg))))

(define (format-args args)
  (if (list? args)
      (apply string-append (map format-arg args))
      (format #f "~a ..." (format-arg args))))

(define (command-description c)
  (define summary (command-summary c))
  (format #f "~a\n~a" summary (command-documentation c)))

(define (command-summary c)
  (format #f "^(color wheat)~a^(color white) ~a" 
          (symbol->string c)
          (format-args (hashq-ref commands c))))

(define-command (help)
  "List all the commands and show documentation for the one selected
by the user"
  (menu "Select a command:"
        (map (lambda (c)
               (cons (symbol->string c)
                     (command-description c)))
             (hash-map->list (lambda (k v) k) commands))
        (lambda (doc)
          (if doc (sticky-message doc) (message "Not documented")))))

(define-command (document (cmd #:symbol))
  "Display arguments, their types, and the docstring for command CMD"
  (sticky-message (command-description cmd)))

