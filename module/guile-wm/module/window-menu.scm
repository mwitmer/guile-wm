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

(define-module (guile-wm module window-menu)
  #:use-module (guile-wm icccm)
  #:use-module (guile-wm command)
  #:use-module (guile-wm log)
  #:use-module (guile-wm module menu)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm reparent)
  #:use-module (guile-wm shared)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop))

(define-public menu-select-window-hook (make-wm-hook 1))

(define-command (select-window)
  (define windows (or (reparented-windows) (top-level-windows)))
  (define choices (map cons (window-names windows) windows))
  (define (focus-window win)
    (cond
     ((wm-hook-empty? menu-select-window-hook)
      (configure-window (window-parent win) #:stack-mode 'above)
      (set-focus win))
     (else (run-wm-hook menu-select-window-hook win))))
  (define focused-window (xref (reply-for get-input-focus) 'focus))
  (if (not (null? choices))
   (menu "Select a window:" choices focus-window)))
