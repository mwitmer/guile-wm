(define-module (language command spec)
  #:use-module (ice-9 rdelim)
  #:use-module (system base language)
  #:use-module (guile-wm shared)
  #:export (command))

(define-public arg-missing (make-parameter #f))

(define (string-convert arg type)
  (case type
    ((#:number) (string->number arg))
    ((#:symbol) (string->symbol arg))
    ((#:string) arg)))

(define (number-convert arg type)
  (case type
    ((#:number) arg)
    ((#:symbol) (string->symbol (number->string arg)))
    ((#:string) (number->string arg))))

(define (symbol-convert arg type)
  (case type
    ((#:number) (string->number (symbol->string arg)))
    ((#:symbol) arg)
    ((#:string) (symbol->string arg))))

(define (type-convert arg type)
  (cond
   ((string? arg) (string-convert arg type))
   ((symbol? arg) (symbol-convert arg type))
   ((number? arg) (number-convert arg type))))

(define* (produce-args args types #:optional (converted '()))
  (cond
   ((null? types) (reverse converted))
   ((null? args)
    (let ((val ((arg-missing) (caar types) (cdar types))))
      (if (unspecified? val)
          (list val)
          (produce-args
           '() (cdr types) (cons (type-convert val (cdar types)) converted)))))
   (else
    (produce-args
     (cdr args) (cdr types)
     (cons (type-convert (car args) (cdar types))
           converted)))))

(define (compile-scheme exp env opts)
  (format #t "~a\n" (arg-missing))
  (let* ((command (get-command (car exp)))
         (typed-exp
          (if (list? command)
              `(,(car exp) ,@(produce-args (cdr exp) command))
              `(,(car exp) ,@(map (lambda (arg) (type-convert arg (cdr command)))
                                  (cdr exp))))))
    (if (and (> (length typed-exp) 1) (unspecified? (cadr typed-exp)))
        (values '*unspecified* env env)
        (values typed-exp env env))))

(define-language command
  #:title "command"
  #:reader (lambda (p e) (read p))
  #:compilers `((scheme . ,compile-scheme))
  #:printer write)
