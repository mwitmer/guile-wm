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

(define-module (guile-wm module fullscreen)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm command)
  #:use-module (guile-wm log)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm module randr)
  #:use-module (guile-wm reparent))

(define (get-current-window)
  (define current-window
    (let ((focus-info (reply-for get-input-focus)))
      (if (memv (xid->integer (xref focus-info 'focus)) 
                (xenum-values input-focus))
          (let ((query-pointer (reply-for query-pointer (current-root))))
            (xref query-pointer 'child))
          (xref focus-info 'focus))))
  (if (= (xid->integer current-window) 0) #f current-window))

(define (get-matching-display-bounds win-x win-y)
  (let find-dimens ((possible (get-output-dimensions)))
    (if (null? possible)
        (values #f #f #f #f)
        (let ((x (assq-ref (car possible) 'x))
              (y (assq-ref (car possible) 'y))
              (height (assq-ref (car possible) 'height))
              (width (assq-ref (car possible) 'width)))
          (if (and (>= win-x x) (>= win-y y)
                   (< win-x (+ x width)) (< win-y (+ y height)))
              (values width height x y)
              (find-dimens (cdr possible)))))))

(define-command (fullscreen)
  (define (fullscreen-it win)
    (with-replies ((geom get-geometry win))
      (define border (* 2 (xref geom 'border-width)))
      (call-with-values
          (lambda () (get-matching-display-bounds 
                      (xref geom 'x) (xref geom 'y)))
        (lambda (width height x y)
          (if width (configure-window win
                      #:x x #:y y
                      #:height (- height border) 
                      #:width (- width border)))))))
  (and=> (and (get-current-window) (window-parent (get-current-window)))
         fullscreen-it))
