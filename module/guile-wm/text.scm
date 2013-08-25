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

(define-module (guile-wm text)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 q)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm color)
  #:use-module (guile-wm shared)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:export (show-lines))

(define escapes "\n^")

(define (read-escaped-text)
  (define port (current-input-port))
  (define text (read-delimited escapes port 'peek))
  (define exp
   (cond
    ((eof-object? text) text)
    ((= (string-length text) 0)
     (case (read-char port)
       ((#\newline) '(start-new-line))
       ((#\^) 
        (if (char=? #\^ (peek-char port))
            `(output-text ,(string (read-char port)))
            (read port)))))
    (else `(output-text ,text))))
  exp)

(define-public (measure-text text font-string)
  (define font-info (with-font (font-string font) (reply-for query-font font)))
  (define ascent (xref font-info 'font-ascent))
  (define descent (xref font-info 'font-descent))
  (define height (+ ascent descent 2))
  (with-input-from-string text
    (lambda ()
      (let measure ((dimens (make-q)) (widest 0) (x 2) (y ascent))
        (match (read-escaped-text)
          ((? eof-object?) 
           (values (+ (max x widest) 2) (+ (+ y descent) 2) (car dimens)))
          (('color co1 co2) (measure dimens widest x y))
          (('color co) (measure dimens widest x y))
          (('invert) (measure dimens widest x y))
          (('start-new-line) (measure dimens (max x widest) 2 (+ y height)))
          (('output-text m)
           (with-font (font-string font)
             (let ((width (xref (reply-for query-text-extents font m)
                                'overall-width)))
               (measure (enq! dimens (cons x y)) widest (+ x width) y)))))))))

(define-public (unescape-text text)
  (with-input-from-string text
    (lambda ()
      (let unescape ((unescaped ""))
        (match (read-escaped-text)
          ((? eof-object?) unescaped)
          (('color co1 co2) (unescape unescaped))
          (('color co) (unescape unescaped))
          (('invert) (unescape unescaped))
          (('start-new-line) (unescape (string-append unescaped "\n")))
          (('output-text m) (unescape (string-append unescaped m))))))))

(define-public (display-text text dimens target font-name fg bg)
  (define cmap (xref (current-screen) 'default-colormap))
  (define (->pixel sym) (pixel-for-color cmap sym))
  (with-input-from-string text
    (lambda ()
      (let disp ((dimens dimens) (fg (->pixel fg)) (bg (->pixel bg)))
        (define escaped (read-escaped-text))
        (match escaped
          ((? eof-object?) #t)
          (('color nfg nbg) (disp dimens (->pixel nfg) (->pixel nbg)))
          (('color nfg) (disp dimens (->pixel nfg) bg))
          (('invert) (disp dimens bg fg))
          (('start-new-line) (disp dimens fg bg))
          (('output-text m) 
           (with-font (font-name font)
             (with-gc (gc target #:foreground fg #:background bg #:font font)
               (image-text16 target gc (caar dimens) (cdar dimens) m)))
           (disp (cdr dimens) fg bg)))))))

(define-public (put-text text win fg bg font-string)
  (receive (width height positions) (measure-text text font-string) 
    (define (draw!)
      (with-pixmap (pixmap win bg width height)
        (display-text text positions pixmap font-string fg bg)
        (with-gc (gc pixmap) (copy-area pixmap win gc 0 0 0 0 width height))))
   (configure-window win #:width width #:height height)
   (create-tagged-listener win (stop!)
     ((expose-event ex #:window win #:count 0) (draw!))
     ((unmap-notify-event un #:event win) (stop!)))
   (draw!)))
