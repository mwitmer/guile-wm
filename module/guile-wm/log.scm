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

(define-module (guile-wm log)
  #:use-module (ice-9 format)
  #:use-module (xcb event-loop))

(define-public logfile (format #f "~a/guile-wm.log" (getenv "HOME")))
(define port (open-file logfile "w0"))
(current-output-port port)
(current-error-port port)

(define-public (log! message)
  (let* ((tm (localtime (current-time))))
    (format #t "~2,'0d:~2,'0d:~2,'0d  ~a\n"
            (tm:hour tm) (tm:min tm) (tm:sec tm) message)))

(define-public (close-log)
  (close-port port))
