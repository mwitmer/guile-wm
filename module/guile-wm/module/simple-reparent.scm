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

(define-module (guile-wm module simple-reparent)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm color)
  #:use-module (guile-wm reparent)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm redirect))

(define (on-map map-request)
  (define xcb-conn (current-xcb-connection))
  (define original-parent (xref map-request 'parent))
  (define child (xref map-request 'window))
  (define stored-parent (hashv-ref reparents (xid->integer child)))
  (if stored-parent
      (map-window stored-parent)
      (let ((new-parent (basic-window-create 0 0 1 1 2 '())))
        (grab new-parent)
        (wm-reparent-window child new-parent 0 0))))

(define (on-configure configure-request) 
  (define value-mask (xref configure-request 'value-mask))
  (define win (xref configure-request 'window))
  (define (get-prop prop) 
    (define val (xref configure-request prop))    
    (cons (symbol->keyword prop)
          (case prop
            ((sibling) (xid->integer val))
            ((stack-mode) (xenum-ref stack-mode val))
            (else val))))
  (apply configure-window win
         (let flatten ((i (map get-prop value-mask)) (o '()))
           (if (null? i) o (flatten (cdr i) `(,(caar i) ,(cdar i) ,@o))))))

(define (on-circulate circulate-request) #f)

(define (grab win)
  (grab-button #f win '(button-press) 'sync 'async (xcb-none xwindow)
               (xcb-none xcursor) '#{1}# '()))

(define (click-to-focus button-press)
  (and=>
   (assv-ref (reverse-reparents) (xid->integer (xref button-press 'event)))
   set-focus)
  (allow-events 'replay-pointer (xref button-press 'time)))

(register-guile-wm-module!
 (lambda () 
   (set! reparents (make-hash-table))
   (end-redirect!)
   (begin-redirect! on-map on-configure on-circulate)
   (listen! button-press-event 'click-to-focus click-to-focus)))
