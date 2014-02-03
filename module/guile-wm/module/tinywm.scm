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

(define-module (guile-wm module tinywm)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm log)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm module randr)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml))

(define action (make-parameter 'none))
(define win (make-parameter #f))
(define screen-height (make-parameter #f))
(define screen-width (make-parameter #f))

(define-public tinywm-drag-end-hook (make-wm-hook 1))
(define-public tinywm-resize-end-hook (make-wm-hook 1))

(define (on-motion-notify motion-notify)
  (with-replies ((point query-pointer (current-root)) (geom get-geometry (win)))
    (define (box p g s) (if (> (+ p g) s) (- s g) p))
    (if (eq? (action) 'move)
        (configure-window (win)
          #:x (box (xref point 'root-x) (xref geom 'width) (screen-width))
          #:y (box (xref point 'root-y) (xref geom 'height) (screen-height)))
        (configure-window (win)
          #:width (- (xref point 'root-x) (xref geom 'x))
          #:height (- (xref point 'root-y) (xref geom 'y))))))

(define (on-button-release button-release)
  (ungrab-pointer xcb-current-time)
  (if (eq? (action 'move))
      (run-wm-hook tinywm-drag-end-hook (win))
      (run-wm-hook tinywm-resize-end-hook (win))))

(define (on-window-click window button-press)
  (define (start-action)
    (define dimens 
      (or (screen-dimensions) ;; Use randr screen dimensions where possible
          (cons (xref (current-screen) 'width-in-pixels)
                (xref (current-screen) 'height-in-pixels))))
    (screen-width (car dimens))
    (screen-height (cdr dimens))
    (configure-window window #:stack-mode 'above)
    (with-replies ((geom get-geometry window))
      (cond
       ((= (xref button-press 'detail) 1)
        (action 'move)
        (warp-pointer (xcb-none xwindow) window 0 0 0 0 1 1))
       (else
        (action 'resize)
        (warp-pointer (xcb-none xwindow) window 0 0 0 0 
                      (xref geom 'width) (xref geom 'height))))
      (grab-pointer 
       #f (current-root)
       '(button-release button-motion pointer-motion-hint)
       'async 'async (current-root) (xcb-none xcursor) xcb-current-time)))
  (if (equal? (xref button-press 'state) '(mod1)) (start-action)))

(define (on-button-press button-press)
  (win (xref button-press 'child))

  ;; Don't try to move/resize the root window.
  (unless (or (= (xid->integer (win)) 0) (fixed-window? (win)))
    (on-window-click (win) button-press)))

(wm-init
 (lambda ()
   (create-listener ()
     ((motion-notify-event #:event (current-root))  => on-motion-notify)
     ((button-release-event #:event (current-root)) => on-button-release)
     ((button-press-event #:event (current-root))   => on-button-press))

   (grab-button #f (current-root) '(button-press button-release) 'async 'async
                (current-root) (xcb-none xcursor) '#{1}# '(#{1}#))
   (grab-button #f (current-root) '(button-press button-release) 'async 'async
                (current-root) (xcb-none xcursor) '#{3}# '(#{1}#))))
