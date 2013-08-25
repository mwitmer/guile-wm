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

(define-module (guile-wm reparent)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm redirect))

(define-public (wm-reparent-window child parent x y)
  (hashv-set! reparents (xid->integer child) parent)
  (with-replies ((geom get-geometry child)
                 (parent-geom get-geometry parent))
    (define child-border (* 2 (xref geom 'border-width)))
    (define parent-border (* 2 (xref parent-geom 'border-width)))
    (configure-window parent
      #:x (xref geom 'x) #:y (xref geom 'y)
      #:width (+ child-border (xref geom 'width))
      #:height (+ child-border (xref geom 'height)))
    (change-window-attributes parent
      #:event-mask '(button-press substructure-redirect substructure-notify
                                  structure-notify visibility-change))
    (reparent-window child parent 0 0)
    (map-window child)
    (map-window parent)
    (set-focus child)
    (create-listener (stop!)
      ((destroy-notify-event destroy #:window child)
       (hashv-remove! reparents (xid->integer child))
       (destroy-window parent)
       (stop!))
      ((visibility-notify-event visibility #:window parent)
       (if (eq? (xref visibility 'state) 'fully-obscured)
           (hashv-set! obscured-windows (xid->integer child) #t)
           (hashv-remove! obscured-windows (xid->integer child))))
      ((unmap-notify-event unmap-notify #:window child) (unmap-window parent))
      ((map-request-event map-request #:window child)
       (map-window child)
       (map-window parent))
      ((configure-notify-event configure #:window parent)
       (configure-window child
         #:height (max (- (xref configure 'height) y) 0)
         #:width (max (- (xref configure 'width) x) 0)
         #:x x #:y y))
      ((configure-request-event configure #:window child)
       (configure-window parent
         #:height (max (+ (xref configure 'height) child-border y) 0)
         #:width (max (+ (xref configure 'width) child-border x) 0))))))
