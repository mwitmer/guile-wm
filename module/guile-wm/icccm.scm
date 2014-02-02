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

(define-public (request-window-property win atom)
  (delay-reply get-property #f win atom (pre-defined-atom 'any) 0 max-length))

(define-public (get-window-property win atom)
  (solicit (request-window-property win atom)))

(define-public (window-property-value get-property-reply)
  (xref-string get-property-reply 'value))

(define-public (window-property-type get-property-reply)
  (xref get-property-reply 'type))

(define (force-requests reqs)
  (define (get-result req) (solicit req))
  (map get-result reqs))

(define-public (window-attributes wins)
  (define (make-request win) (delay-reply get-window-attributes win))
  (force-requests (map make-request wins)))

(define-public (window-names wins)
  (define (make-request win) (request-window-property win wm-name-atom))
  (map window-property-value (force-requests (map make-request wins))))

(define-public (window-name win)
  (window-property-value (get-window-property win wm-name-atom)))

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

(define-xcb-struct wm-hints
  (make-wm-hints
   flags input initial-state icon-pixmap icon-x icon-y icon-mask window-group)
  wm-hints? wm-hints-type #f 36
  (flags CARD32)
  (input CARD32)
  (initial-state CARD32)
  (icon-pixmap xpixmap)
  (icon-window xwindow)
  (icon-x INT32)
  (icon-y INT32)
  (icon-mask xpixmap)
  (window-group xwindow))

(define-xcb-struct wm-state
  (make-wm-state state icon)
  wm-state? wm-state-type #f 8
  (state CARD32)
  (icon xwindow))

(define-public (window-struct-property win struct atom)
  (define property (get-window-property win atom))
  (define bv
    (if (xid= (xref property 'type) (pre-defined-atom 'none)) #f
        (list->u8vector (vector->list (xref property 'value)))))
  (if (not bv) #f
      (xcb-struct-unpack
       struct
       (open-bytevector-input-port
        bv))))

(define-public window-state-withdrawn 0)
(define-public window-state-normal 1)
(define-public window-state-iconic 3)

(define-public (window-wm-hints win)
  (window-struct-property win wm-hints (pre-defined-atom 'wm-hints)))

(define (wm-state-atom) (xref (reply-for intern-atom #f "WM_STATE") 'atom))

(define-public (window-state win)
  (window-struct-property win wm-state (wm-state-atom)))

(define-public (set-window-state! win state)
  (define new-win-wm-state (make-wm-state state 0))
  (format #t "Setting window state on ~a to ~a\n" win state)
  (let ((property-atom (wm-state-atom)))
   (change-property
    'replace win property-atom property-atom 8
    (list->vector
     (u8vector->list (xcb-struct-pack-to-bytevector new-win-wm-state))))))

(define-public (window-size-hints win)
  (window-struct-property
   win wm-size-hints (pre-defined-atom 'wm-normal-hints)))
