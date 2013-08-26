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

(define-module (guile-wm focus)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm log))

(define-public (set-focus new-focus)
  (with-replies ((input-focus get-input-focus))
    (define old-focus (xref input-focus 'focus))
    (focus-change old-focus new-focus)
    (set-input-focus 'pointer-root new-focus 0)
    (if (not (xcb= old-focus new-focus))
     (log! (format #f "Input focus: ~a" new-focus)))
    (configure-window (window-parent new-focus) #:stack-mode 'above)))

(define-public (focus-change old new) #t)
