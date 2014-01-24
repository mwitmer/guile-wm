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
  #:use-module (ice-9 binary-ports)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml type)
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

(define-xcb-struct wm-size-hints
  (make-wm-size-hints
   flags min-width min-height max-width max-height
   width-inc height-inc min-aspect-numerator
   min-aspect-denominator max-aspect-numerator
   max-aspect-denominator base-width base-height
   win-gravity)
  wm-size-hints? wm-size-hints-type #f 72
  (flags CARD32)
  (*pad* 16)
  (min-width INT32)
  (min-height INT32)
  (max-width INT32)
  (max-height INT32)
  (width-inc INT32)
  (height-inc INT32)
  (min-aspect-numerator INT32)
  (min-aspect-denominator INT32)
  (max-aspect-numerator INT32)
  (max-aspect-denominator INT32)
  (base-width INT32)
  (base-height INT32)
  (win-gravity INT32))

(define-public (window-hints win)
  (xcb-struct-unpack
   wm-size-hints
   (open-bytevector-input-port
    (list->u8vector
     (vector->list
      (xref
       (get-text-property win (pre-defined-atom 'wm-normal-hints))
       'value))))))
