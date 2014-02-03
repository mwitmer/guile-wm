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

(define-module (guile-wm module magnetic)
  #:use-module (guile-wm shared)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop))

(use-wm-modules tinywm tiling)

(wm-init
 (lambda ()
   (add-wm-hook!
    tinywm-drag-end-hook
    (lambda (win)
      (with-replies ((geom get-geometry win))
        (let ((new-tile (tile-at (xref geom 'x) (xref geom 'y))))
          (if (not (xid= blank-x-window win))
              (move-tile selected-tile new-tile))))))
   (add-wm-hook!
    tinywm-resize-end-hook
    (lambda (win)
      (let ((tile (tile-for win)))
        (move-tile tile tile))))))
