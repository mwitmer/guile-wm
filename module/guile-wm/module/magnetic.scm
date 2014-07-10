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
  #:use-module (srfi srfi-2)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop))

(use-wm-modules tinywm tiling)

(define (fit-to-bounds wind)
  (and-let* ((tile (tile-for win)))
    (move-tile tile tile)))

(define (snap-to-tile win)
  (with-replies ((geom get-geometry win))
    (and-let* ((new-tile (tile-at (xref geom 'x) (xref geom 'y)))
               ((tile-for win)))
      (if (not (xid= blank-x-window win))
          (move-tile selected-tile new-tile)))))

(define (start-magnetic!)
  (add-wm-hook! tinywm-drag-end-hook snap-to-tile)
  (add-wm-hook! tinywm-resize-end-hook fit-to-bounds))

(define (stop-magnetic!)
  (remove-wm-hook! tinywm-drag-end-hook snap-to-tile)
  (remove-wm-hook! tinywm-resize-end-hook fit-to-bounds))

(add-wm-hook! stop-tiling-hook stop-magnetic!)
(add-wm-hook! start-tiling-hook start-magnetic!)
