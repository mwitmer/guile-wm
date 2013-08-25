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

(define-module (guile-wm icccm)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:use-module (guile-wm shared))

(define max-length (- (expt 2 32) 1))
(define-public (pre-defined-atom sym) (make-xid (xenum-ref atom sym) xatom))

(define wm-name-atom (pre-defined-atom 'wm-name))

(define-public (request-text-property win atom)
  (delay-reply get-property #f win atom (pre-defined-atom 'any) 0 max-length))

(define-public (get-text-property win atom)
  (solicit (request-text-property win atom)))

(define-public (text-property-value get-property-reply)
  (xref-string get-property-reply 'value))

(define-public (text-property-type get-property-reply)
  (xref get-property-reply 'type))

(define (force-requests reqs)
  (define (get-result req) (solicit req))
  (map get-result reqs))

(define-public (window-attributes wins)
  (define (make-request win) (delay-reply get-window-attributes win))
  (force-requests (map make-request wins)))

(define-public (window-names wins)
  (define (make-request win) (request-text-property win wm-name-atom))
  (map text-property-value (force-requests (map make-request wins))))

(define-public (window-name win)
  (text-property-value (get-text-property win wm-name-atom)))

(define-public (top-level-windows)
  (define query-tree (reply-for query-tree (current-root)))
  (define wins (vector->list (xref query-tree 'children)))
  (define attribute-alist (map cons wins (window-attributes wins)))
  (define (is-top-level? attr-pair) 
    (and (eq? (xref (cdr attr-pair) 'map-state) 'viewable)
         (not (xref (cdr attr-pair) 'override-redirect))))
  (map car (filter is-top-level? attribute-alist)))

