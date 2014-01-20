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

(define-module (guile-wm shared)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (guile-wm log)
  #:export (use-wm-modules)
  #:replace (with-input-from-string))

(define-public current-root (make-parameter #f))
(define-public current-screen (make-parameter #f))

(define module-init-thunks (make-hash-table))
(define-public obscured-windows (make-hash-table))

(define-public (register-guile-wm-module! thunk)
  "Register the current module so that it can be initialized once the
window manager is running. THUNK will be executed after
`current-xcb-connection' is set to the window manager's X connection."
  (hashq-set! module-init-thunks (current-module) thunk))

(define-public (init-guile-wm-modules!)
  "Call all of the initialization thunks for registered window manager
modules."
  (for-each 
   (lambda (kv)
     (log! (format #f "Initializing module: ~a" (module-name (car kv))))
     ((cdr kv))) 
   (hash-map->list cons module-init-thunks)))

(define-public commands (make-hash-table))
(define-public (get-command key)
  "Retrieve a window manager command with key KEY. Returns #f is
none exists."
  (hashq-ref commands key))

(define-public reparents (make-hash-table))

;; This procedure is redefined so that we can rewind delimited
;; continuations through it

(define (with-input-from-string string thunk)
  (parameterize ((current-input-port (open-input-string string)))
    (thunk)))

(define-public (reverse-reparents)
  "Return a association list for all reparented windows. The car of
each assoc is the integer value of the xid of the parent window, and
the cdr is the xid of the child."
  (hash-map->list 
   (lambda (k v) (cons (xid->integer v) (make-xid k xwindow))) 
   reparents))

(define-public (reparented-windows)
  "Return a association list for all reparented windows. The car of
each assoc is the integer value of the xid of the child window, and
the cdr is the xid of the parent."
  (if reparents
      (hash-map->list (lambda (k v) (make-xid k xwindow)) reparents)
      #f))

(define-public (window-parent win)
  (or (hashv-ref reparents (xid->integer win)) win))
