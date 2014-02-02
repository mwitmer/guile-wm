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

(define-public message-default-timeout 8)
(define hide-message-thread #f)

(define (show-message msg)
  (define screen (current-screen))
  (map-window message-window)
  (put-text msg message-window 'white 'black font-string)
  (configure-window message-window #:stack-mode 'above)
  (if hide-message-thread (cancel-thread hide-message-thread)))

(define-command (message msg #:string)
  (message-with-timeout (string-join msg) message-default-timeout))

(define-command (message-with-timeout (msg #:string) (timeout #:number))
  (show-message msg)
  (set!
   hide-message-thread
   (make-thread (lambda () (sleep timeout) (hide-message)))))

(define-command (sticky-message msg #:string)
  (show-message (string-join msg)))

(define-command (hide-message)
  (unmap-window message-window)
  (if hide-message-thread (cancel-thread hide-message-thread))
  (set! hide-message-thread #f))

(define-once message-window #f)

(wm-init
 (lambda () (set! message-window (fixed-window-create 0 0 200 20 0 #:focused? #f))))
