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

(define-module (guile-wm keystroke)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:use-module (guile-wm keysyms)
  #:use-module (guile-wm log))

(define modifier-keys
  '(left-control right-control left-shift
                 left-meta right-meta
                 right-shift left-super right-super menu))

(define-public (keystroke-listen! win tag)
  "Returns a procedure that will solicit a keypress with WIN focused
and return the associated key symbol. The returned procedure takes the
same arguments as `solicit'. TAG is a unique name for the listener, as
described in `create-tagged-listener'."
  (receive (stop! reset!)
   (create-tagged-listener tag
       (stop! reset!
              (define keysym-table (make-keysym-table))
              (define modifier-table (make-modifier-table))
              (define num-lock-modifier
                (get-num-lock-modifier keysym-table modifier-table)))
     ((key-press-event key-press #:event win)
      (define sym 
        (key->symbol modifier-table keysym-table (xref key-press 'detail)
                     (xref key-press 'state) num-lock-modifier))
      (if (not (memq sym modifier-keys)) (notify tag sym)))
     ((destroy-notify-event destroy-notify #:event win) (unsolicit tag) (stop!))
     ((unmap-notify-event notify #:window win) (stop!))
     ((mapping-notify-event mapping) (reset!)))
   (case-lambda
     (() (solicit tag))
     ((proc) 
      (if (not proc) (stop!))
      (solicit tag proc)))))

