(define-module (language command spec)
  #:use-module (ice-9 rdelim)
  #:use-module (system base language)
  #:use-module (guile-wm shared)
  #:export (command))

(define (type-convert arg type) 
  (case type
    ((#:number #:symbol) arg)
    ((#:string) (format #f "~a" arg))))

(define (compile-scheme exp env opts)
  (define command (get-command (car exp)))
  (define typed-exp
    (if (list? command)
        `(,(car exp) ,@(map type-convert (cdr exp) command))
        `(,(car exp) ,@(map (lambda (arg) (type-convert arg command))
                            (cdr exp)))))
  (values typed-exp env env))

(define-language command
  #:title "command"
  #:reader (lambda (p e) (read p)) 
  #:compilers `((scheme . ,compile-scheme))
  #:printer write)
