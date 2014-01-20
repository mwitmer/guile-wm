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

(define-module (guile-wm module message)
  #:use-module (guile-wm text)
  #:use-module (guile-wm command)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm shared)
  #:use-module (ice-9 threads)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:export ((font-string . message-font) message-window))

(define font-string "fixed")

(define-command (message msg #:string)
  (define screen (current-screen))
  (map-window message-window)
  (put-text (string-join msg) message-window 'white 'black font-string)
  (configure-window message-window #:stack-mode 'above)
  (make-thread (lambda () (sleep 8) (unmap-window message-window))))

(define message-active? (make-parameter #f))
(define-once message-window #f)
(define message-key-tag (make-tag 'message))

(register-guile-wm-module!
 (lambda () (set! message-window (fixed-window-create 0 0 200 20 0))))
