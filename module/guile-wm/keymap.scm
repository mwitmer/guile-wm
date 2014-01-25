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
  #:export (define-keymap create-keymap define-keymap-once))

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
      (hashq-set! (mappings keymap) key proc-or-keymap)))

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
       (make-mapping km mapping) ... km))
    ((_ mapping ...)
     (let ((km (make-keymap)))
       (make-mapping km mapping) ... km))))

(define-syntax make-mapping
  (syntax-rules (=> :)
    ((_ km ((k args ...) stmt ...))
     (bind-key! km (quasiquote k) (lambda (args ...) stmt ...)))
    ((_ km (k => proc))
     (bind-key! km (quasiquote k) (lambda args (apply proc args))))
    ((_ km (k : nkm))
     (bind-key! km (quasiquote k) nkm))))

(define-public (keymap-lookup keymap key-provider . args)
  (let* ((key (key-provider))
         (val (hashq-ref (mappings keymap) key)))
    (let ((result
           (cond ((keymap? val) (apply keymap-lookup val key-provider args))
                 ((procedure? val) (lambda () (apply val args)))
                 (else (lambda () (apply (default keymap) key args))))))
      result)))
