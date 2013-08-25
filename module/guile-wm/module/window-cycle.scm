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

(define-module (guile-wm module window-cycle)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (guile-wm reparent)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm command)
  #:use-module (guile-wm focus))

(define (pick-next-matching-window start all pred)
  (let pick ((to-test (cdr start)))
    (cond
     ((null? to-test) (pick all))
     ((eq? to-test start) #f)
     ((pred (make-xid (caar to-test) xwindow))
      (make-xid (caar to-test) xwindow))
     (else (pick (cdr to-test))))))

(define (basic-window-cycle pred)
  (define reparent-alist (hash-map->list cons reparents))
  (with-replies ((focus get-input-focus))
    (define old (xref focus 'focus))
    (if (not (null? reparent-alist))
        (if (not (memv (xid->integer old) (xenum-values input-focus)))
            (and=> (let find-focus ((alist reparent-alist))
                     (cond
                      ((null? alist) #f)
                      ((= (caar alist) (xid->integer old))
                       (pick-next-matching-window alist reparent-alist pred))
                      (else (find-focus (cdr alist)))))
                   set-focus)
            (set-focus (make-xid (caar reparent-alist) xwindow))))))

(define-command (window-cycle) (basic-window-cycle (lambda (win) #t)))

(define-command (visible-window-cycle)
  (basic-window-cycle
   (lambda (win) (not (hashv-ref obscured-windows (xid->integer win))))))
