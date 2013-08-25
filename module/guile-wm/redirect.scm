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

(define-module (guile-wm redirect)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm log))

(define redirect-tag (make-tag 'redirect))

(define (root-only request) (xid= (current-root) (xref request 'parent)))

(define-public (begin-redirect! on-map on-configure on-circulate)
  (listen! map-request-event redirect-tag on-map root-only)
  (listen! configure-request-event redirect-tag on-configure root-only)
  (listen! circulate-request-event redirect-tag on-circulate root-only)
  (with-replies ((attributes get-window-attributes (current-root)))
    (define old-events (xref attributes 'your-event-mask))
    (if (not (memq 'substructure-redirect old-events))
        (let ((new-events (cons 'substructure-redirect old-events)))
          (change-window-attributes (current-root) #:event-mask new-events)))))

(define-public (end-redirect!)
  (unlisten! map-request-event redirect-tag)
  (unlisten! configure-request-event redirect-tag)
  (unlisten! circulate-request-event redirect-tag)
  (with-replies ((attributes get-window-attributes (current-root)))
    (define old-events (xref attributes 'your-event-mask))
    (if (memq 'substructure-redirect old-events)
     (let ((new-events (delq 'substructure-redirect old-events)))
       (change-window-attributes (current-root) #:event-mask new-events)))))
