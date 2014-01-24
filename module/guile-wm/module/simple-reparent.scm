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
  #:use-module (guile-wm focus)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm redirect)
  #:use-module (guile-wm reparent)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto))

(define-public (click-to-focus button-press)
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
