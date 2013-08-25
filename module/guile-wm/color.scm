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

(define-module (guile-wm color)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (guile-wm shared))

(define colormap-colors (make-hash-table))

(define-public (pixel-for-color cmap color)
  (define str (string-downcase
               (string-join
                (string-split
                 (symbol->string color) (char-set #\-)) " ")))
  (define (alloc-color ht)
    (define reply (reply-for alloc-named-color cmap str))
    (hashq-set! ht color (xref reply 'pixel)))
  (define colors
    (or (hashv-ref colormap-colors (xid->integer cmap))
        (hashv-set! colormap-colors (xid->integer cmap) (make-hash-table))))
  (or (hashq-ref colors color)
      (hashv-set! colors color (alloc-color colors))))

