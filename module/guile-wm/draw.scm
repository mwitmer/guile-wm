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

(define-module (guile-wm draw)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module (guile-wm color)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm icccm)
  #:use-module (guile-wm shared)
  #:export (with-gc with-font with-pixmap basic-window-create
                    fixed-window-create))

(define-syntax-rule (with-gc (gc drawable prop ...) stmt stmt* ...)
  (let* ((gc (make-new-xid xgcontext)))
    (dynamic-wind
      (lambda () (create-gc gc drawable prop ...))
      (lambda () stmt stmt* ...)
      (lambda () (free-gc gc)))))

(define-syntax-rule (with-font (name font) stmt stmt* ...)
  (let ((font (make-new-xid xfont)))
    (dynamic-wind
      (lambda () (open-font font name))
      (lambda () stmt stmt* ...)
      (lambda () (close-font font)))))

;; Don't use dynamic-wind here, since pixmaps reference volatile state
(define-syntax-rule (with-pixmap (pixmap win color width height) stmt stmt* ...)
  (let* ((pixmap (make-new-xid xpixmap))
         (cmap (xref (current-screen) 'default-colormap))
         (pixel (pixel-for-color cmap color)))
    (define (action) stmt stmt* ...)
    (create-pixmap 24 pixmap win width height)
    (with-gc (gc pixmap #:foreground pixel)
      (poly-fill-rectangle pixmap gc `#(,(make-xrectangle 0 0 width height))))
    (action)
    (free-pixmap pixmap)))

(define basic-events
  '(key-press structure-notify exposure))

(define* (basic-window-create x y width height border
                              #:optional (events basic-events))
  "Create a window with dimensions (X, Y), size (WIDTH, HEIGHT) and
border width BORDER. Takes an optional event mask, which by default
consists of key-press, structure-notify, and exposure."
  (define window (make-new-xid xwindow))
  (create-window
   24 window (current-root) x y width height border 'copy-from-parent 0
   #:back-pixel (xref (current-screen) 'black-pixel)
   #:bit-gravity 'north-west #:event-mask events #:override-redirect #t)
  window)

(define (fixed-window-atom)
  (xref (reply-for intern-atom #f "_GUILE_WM_FIXED") 'atom))

(define* (fixed-window-create x y width height border
                              #:optional (events basic-events)
                              #:key (focused? #t))
  "Create a window that will always remain on top as long as it is
mapped. Takes the same arguments as `basic-window-create', with the
addition of keyword FOCUSED?, which defaults to true. When it is true,
the window will also keep the focus."
  (define window
    (basic-window-create
     x y width height border
     (cons 'visibility-change events)))
  (create-listener (stop!)
    ((map-notify-event map-notify #:window window)
     (when focused?
       (grab-keyboard #f window xcb-current-time 'async 'async)))
    ((unmap-notify-event unmap-notify #:window window)
     (when focused?
       (ungrab-keyboard xcb-current-time)))
    ((visibility-notify-event visibility #:window window)
     (when (not (eq? (xref visibility 'state) 'unobscured))
       (configure-window (xref visibility 'window) #:stack-mode 'above))))
  (let ((fixed-atom (fixed-window-atom)))
   (change-property 'replace window fixed-atom
                    (pre-defined-atom 'integer) 8 #(1 0 0 0)))
  window)

(define-public (fixed-window? window)
  (xid=
   (xref (get-window-property window (fixed-window-atom)) 'type)
   (pre-defined-atom 'integer)))
