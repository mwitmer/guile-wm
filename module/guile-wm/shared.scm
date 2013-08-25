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
  #:export (use-wm-modules))

(define-public current-root (make-parameter #f))
(define-public current-screen (make-parameter #f))

(define module-init-thunks (make-hash-table))
(define-public obscured-windows (make-hash-table))

(define-public (register-guile-wm-module! thunk)
  (hashq-set! module-init-thunks (current-module) thunk))

(define-public (init-guile-wm-modules!)
  (for-each 
   (lambda (kv)
     (log! (format #f "Initializing module: ~a ~a" (car kv) (cdr kv)))
     ((cdr kv))) 
   (hash-map->list cons module-init-thunks)))

(define-public commands (make-hash-table))
(define-public (get-command key) (hashq-ref commands key))

(define-public reparents (make-hash-table))

(define-public (reverse-reparents)
  (hash-map->list 
   (lambda (k v) (cons (xid->integer v) (make-xid k xwindow))) 
   reparents))

(define-public (reparented-windows)
  (if reparents
      (hash-map->list (lambda (k v) (make-xid k xwindow)) reparents)
      #f))

(define-public (window-parent win)
  (or (hashv-ref reparents (xid->integer win)) win))
