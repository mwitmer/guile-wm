;; This file is part of Guile-WM.

;;    Guile-WM is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    Guile-WM is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with Guile-WM.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guile-wm keymap)
  #:use-module (srfi srfi-9)
  #:export (define-keymap create-keymap define-keymap-once
             create-prompt-keymap define-prompt-keymap))

(define-record-type keymap
  (make-keymap-inner mappings default)
  keymap?
  (mappings mappings)
  (default default set-default!))

(define-public (keymap-ignore key . args) (apply values args))

(define* (make-keymap #:optional default)
  (make-keymap-inner (make-hash-table) default))

(define-public (keymap-with-default keymap default)
  (make-keymap-inner (mappings keymap) default))

(define-public (bind-key! keymap key proc-or-keymap)
  (if (not (or (procedure? proc-or-keymap) (keymap? proc-or-keymap)))
      (error "keymap: Attempt to bind to a non-procedure/keymap")
      (hashq-set!
       (mappings keymap) key
       (if (procedure? proc-or-keymap)
           (lambda (get-key . args) (apply proc-or-keymap args))
           proc-or-keymap))))

(define-public (bind-intermediate-key! keymap key proc)
  (if (not (procedure? proc))
      (error "keymap: Attempt to bind to a non-procedure")
      (hashq-set! (mappings keymap) key proc)))

(define-public (unbind-key! keymap key)
  (hashq-remove! (mappings keymap) key))

(define-syntax define-keymap-once
  (syntax-rules ()
    ((_ name mapping ...)
     (define-once name (create-keymap mapping ...)))))

(define-syntax define-keymap
  (syntax-rules ()
    ((_ name mapping ...)
     (define name (create-keymap mapping ...)))))

(define-syntax create-keymap
  (syntax-rules ()
    ((_ mapping ...)
     (let ((km (make-keymap)))
       (make-mapping km mapping) ... km))))

(define-syntax make-mapping
  (syntax-rules (=> ==>)
    ((_ km ((k ==> args ...) stmt ...))
     (bind-intermediate-key! km (quasiquote k) (lambda (args ...) stmt ...)))
    ((_ km ((k args ...) stmt ...))
     (bind-key! km (quasiquote k) (lambda (args ...) stmt ...)))
    ((_ km (k => proc))
     (bind-key! km (quasiquote k) (lambda args (apply proc args))))
    ((_ km k ==> proc)
     (bind-intermediate-key! km (quasiquote k) proc))))

(define-public (keymap-lookup keymap key-provider . args)
  (let* ((key (key-provider))
         (val (hashq-ref (mappings keymap) key)))
    (if val
        (lambda () (apply val key-provider args))
        (lambda () (apply (default keymap) key args)))))

(define-public (do-keymap keymap key-provider . args)
  ((apply keymap-lookup keymap key-provider args)))

(define-public (cancel-keymap state . args) (apply values 'cancel args))
(define-public (confirm-keymap state . args) (apply values 'confirm args))
(define-public (continue-keymap state . args) (apply values 'continue args))
(define-public (do-prompt-keymap
         keymap get-key on-continue on-confirm on-cancel . args)
  (apply on-continue args)
  (let lp ((state 'continue) (args args))
    (call-with-values (apply keymap-lookup keymap get-key state args)
       (lambda (new-state . new-args)
         (case new-state
          ((continue) (apply on-continue new-args) (lp new-state new-args))
          ((cancel) (apply on-cancel new-args))
          ((confirm) (apply on-confirm new-args)))))))

(define-syntax create-prompt-keymap
  (syntax-rules ()
    ((_ mapping ...)
     (let ((km (make-keymap)))
       (make-prompt-mapping km mapping) ...
       (bind-key! km 'escape cancel-keymap)
       (bind-key! km 'C-g cancel-keymap)
       (bind-key! km 'return confirm-keymap)
       km))))

(define-syntax define-prompt-keymap
  (syntax-rules ()
    ((_ name mapping ...)
     (define name (create-prompt-keymap mapping ...)))))

(define-syntax make-prompt-mapping
  (syntax-rules (=> ==>)
    ((_ km ((k ==> args ...) stmt ...))
     (make-mapping km ((k ==> args ...) stmt ...)))
    ((_ km ((k args ...) stmt ...))
     (bind-key! km (quasiquote k)
                (lambda (state args ...)
                  (call-with-values (lambda () stmt ...)
                    (lambda vals
                      (apply continue-keymap state vals))))))
    ((_ km (k => proc))
     (bind-key! km (quasiquote k)
                (lambda (state . args)
                  (call-with-values
                      (lambda () (apply proc args))
                    (lambda vals (apply continue-keymap state vals))))))
    ((_ km k ==> proc)
     (make-mapping km k ==> proc))))

(define-public (prompt-keymap-with-default keymap default)
  (make-keymap-inner
   (mappings keymap)
   (lambda (key state . args)
     (call-with-values (lambda () (apply default key args))
       (lambda vals (apply continue-keymap state vals))))))
