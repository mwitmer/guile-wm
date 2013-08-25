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

(define-module (guile-wm command)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 threads)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module (guile-wm log)
  #:use-module (guile-wm keymap)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm text)
  #:use-module (language command spec)
  #:use-module (language scheme spec)
  #:use-module (system base compile)
  #:replace (quit)
  #:export (define-command shell-command bind-key-commands))

(define-public (bind-key-command! keymap key str)
  (bind-key! keymap key (lambda () (run-command str))))

(define-syntax bind-key-commands
  (syntax-rules ()
    ((_ keymap (key command) ...)
     (begin
       (bind-key-command! keymap (quasiquote key) command) ...))))

(define-syntax define-command
  (syntax-rules () 
    ((_ (name (arg type) ...) stmt ...)
     (begin
       (define! 'name
         (let ((proc (lambda (arg ...) stmt ...)))
           (hashq-set! commands (quote name) '(type ...))
           proc))
       (export name)))
    ((_ (name arg type) stmt ...)
     (begin
       (define! 'name
         (let ((proc (lambda arg stmt ...)))
           (hashq-set! commands (quote name) type)
           proc))
       (export name)))))

(define-public (run-command cmd)
  (catch #t
    (lambda ()
      (log! (format #f "User command: ~a" (unescape-text cmd)))
      (with-input-from-string (format #f "(~a)" (unescape-text cmd))
        (lambda () (read-and-compile
                    (current-input-port)
                    #:from command
                    #:to 'value
                    #:env (current-module)))))
    (lambda args
      (log! (format #f "Error in command: ~a ~a" command args)))
    (lambda args
      (backtrace))))

(define-command (quit) 
  (when (and (current-xcb-connection) (xcb-connected? (current-xcb-connection)))
    (log! (format #f "Quitting Guile-WM"))
    (xcb-disconnect! (current-xcb-connection))))

(define-command (shell-command commands #:string)
  (close-port (open-pipe (string-join commands) OPEN_READ)))

(define-command (wm-eval (arg #:string)) 
  (with-input-from-string arg
    (lambda () (read-and-compile
                (current-input-port)
                #:from scheme
                #:to 'value
                #:env (current-module)))))
