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

(define-module (guile-wm module time)
  #:use-module (ice-9 format)
  #:use-module (guile-wm command)
  #:use-module (guile-wm shared))

(use-wm-modules message)

(define month-alist
  '((0 . Jan)
    (1 . Feb)
    (2 . Mar)
    (3 . Apr)
    (4 . May)
    (5 . Jun)
    (6 . Jul)
    (7 . Aug)
    (8 . Sep)
    (9 . Oct)
    (10 . Nov)
    (11 . Dec)))

(define-command (show-time)
  (define tm (localtime (current-time)))
  (message (format #f "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~a ~a ~a" 
                   (tm:hour tm)
                   (tm:min tm)
                   (tm:sec tm)
                   (tm:mday tm)
                   (assv-ref month-alist (tm:mon tm))
                   (+ (tm:year tm) 1900))))

