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

(define-module (guile-wm module repl)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm shared)
  #:use-module (ice-9 threads)
  #:use-module (guile-wm log)
  #:use-module (system repl server)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (xcb xml)
  #:export (start-wm-repl))

(define main-thread (current-thread))

(define (post-to-main-thread thunk)
  (define not-done (make-tag 'not-done))
  (define repl-results not-done)
  (define repl-thread (current-thread))
  (define output-port (current-output-port))
  (define input-port (current-input-port))
  (define error-port (current-error-port))
  (define bt #f)
  (define (post) 
    (parameterize ((current-output-port output-port)
                   (current-input-port input-port)
                   (current-error-port error-port))
      (call-with-values
          thunk
        (lambda results
          (system-async-mark 
           (lambda () (set! repl-results results))
           repl-thread)))))
  (define (err key . args)    
    (system-async-mark
     (lambda ()
       (format #t "Posted function raised error ~a with args ~a\n" key args)
       (display bt)
       (set! repl-results (list *unspecified*)))
     repl-thread))
  (define (before-err key . args)
    (set! bt
      (with-output-to-string
        (lambda () (backtrace)))))
  (system-async-mark
   (lambda ()
     (post-to-event-loop (lambda () (catch #t post err before-err))))
   main-thread)
  (while (eq? repl-results not-done) (yield))
  repl-results)

(define-meta-command 
  ((post guile-wm) repl expr . args)
  "post EXPRESSION
Post an expression to the event loop"
  (for-each (lambda (v) (repl-print repl v))
            (post-to-main-thread (eval `(lambda () ,expr) (current-module)))))

(define* (start-wm-repl #:optional (sock (make-tcp-server-socket)))
  (define xcb-conn (current-xcb-connection))
  (make-thread
   (lambda () 
     (parameterize ((current-xcb-connection xcb-conn))
       (run-server sock)))))
