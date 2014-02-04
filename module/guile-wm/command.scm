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
  #:use-module (ice-9 rdelim)
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
  #:export (define-command shell-command bind-key-commands run-command))

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
           (hashq-set! commands (quote name) `(,(cons 'arg type) ...))
           proc))
       (export name)))
    ((_ (name arg type) stmt ...)
     (begin
       (define! 'name
         (let ((proc (lambda arg stmt ...)))
           (hashq-set! commands (quote name) (cons 'arg type))
           proc))
       (export name)))))

(define (arg-missing-default type)
  (error "guile-wm: Cannot request missing argument of type " type))

(define* (run-command cmd #:optional (arg-missing-proc arg-missing-default))
  (catch #t
    (lambda ()
      (log! (format #f "User command: ~a" (unescape-text cmd)))
      (with-input-from-string (format #f "(~a)" (unescape-text cmd))
        (lambda ()
          (parameterize ((arg-missing arg-missing-proc))
            (read-and-compile
             (current-input-port)
             #:from command
             #:to 'value
             #:env (current-module))))))
    (lambda args
      (log! (format #f "Error in command: ~a ~a" cmd args)))
    (lambda args
      (backtrace))))

(define-command (quit) 
  "Quit the window manager and close the connection to the X
server. Replaces the core binding of the same name."
  (when (and (current-xcb-connection) (xcb-connected? (current-xcb-connection)))
    (log! (format #f "Quitting Guile-WM"))
    (xcb-disconnect! (current-xcb-connection))))

(define-command (shell-command commands #:string)
  "Concatenate COMMANDS into a single string and execute the result in
a detached process."
  (close-port (open-pipe (string-join commands) OPEN_READ)))

(define-command (shell-command-output cmd #:string)
  "Concatenate COMMANDS into a single string and execute the result
in a process; wait for the command to terminate and return a string
containing its output."
  (let* ((port (open-input-pipe (string-join cmd)))
         (str (read-delimited "" port)))
    (close-pipe port)
    str))

(define-command (wm-eval (exp #:string))
"Evaluate S-expression @var{exp} in the window manager's current
environment."
  (catch #t
    (lambda ()
      (with-input-from-string exp
        (lambda () (read-and-compile
                    (current-input-port)
                    #:from scheme
                    #:to 'value
                    #:env (current-module)))))
    (lambda args
      (log! (format #f "Error in evaluated expression: ~a ~a" arg args)))
    (lambda args
      (backtrace))))
