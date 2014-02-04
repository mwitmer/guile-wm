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

(define-module (guile-wm module root-keymap)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm keymap)
  #:use-module (guile-wm keysyms)
  #:use-module (guile-wm keystroke)
  #:use-module (guile-wm log)
  #:use-module (guile-wm command)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm shared)
  #:use-module (srfi srfi-11)
  #:export (root-keymap keymap-cursor with-root-keymap-disabled))

(use-wm-modules message cursor)

(define-once root-key-val 'C-t)
(define (root-key-ref) root-key-val)
(define (root-key-set! k)
  (define keysyms (make-keysym-table))
  (let-values (((old-codes old-mods) (symbol->key keysyms root-key-val))
               ((new-codes new-mods) (symbol->key keysyms k)))
    (if old-codes (ungrab-key (car old-codes) target-win old-mods))
    (grab-key #t target-win new-mods (car new-codes) 'async 'async)
    (set! root-key-val k)))

(define-public root-key (make-procedure-with-setter root-key-ref root-key-set!))

(define-command (bind-root-key! (key #:symbol) (str #:string))
  "Bind KEY to command string STR in the root keymap."
  (bind-key-command! root-keymap key str))

(define-command (unbind-root-key! (key #:symbol))
  "Remove the binding for KEY in the root keymap."
  (unbind-key! root-keymap key))

(define (run-keymap get)
  (define (default d) (message (format #f "Unknown key: ~a" d)))
  (define keymap (keymap-with-default root-keymap default))
  (do-keymap keymap get))

(define-keymap-once root-keymap)
(define-once target-win #f)

(define-once keymap-cursor-val #f)
(define root-keymap-enabled? (make-parameter #t))

(define-syntax with-root-keymap-disabled
  (syntax-rules ()
    "Evaluate statements STMT ... and return the value of the last one
with the root keymap disabled. Use this macro to disable the root
keymap while minibuffers, menus, and so on are active."
    ((_ stmt ...)
     (begin
       (root-keymap-enabled? #f)
       (let ((result ((lambda () stmt ...))))
         (root-keymap-enabled? #t)
         result)))))

(define (get-next-key get-now get-later)
  (define (process-root-key key)
    (if (and (eq? key (root-key)) (root-keymap-enabled?))
        (run-keymap get-now))
    (get-later process-root-key))
  (get-later process-root-key))

(define-command (set-root-key! (key #:symbol))
  "Set the root key to KEY."
  (set! (root-key) key))

(define-public keymap-cursor
  (make-procedure-with-setter
   (lambda () keymap-cursor-val)
   (lambda (sym) (set! keymap-cursor-val (make-cursor sym)))))

(wm-init
 (lambda ()
   (set! target-win (current-root))
   (set! (root-key) root-key-val)
   (let* ((key-listener (keystroke-listen! target-win))
          (get-key-now
           (lambda ()
             (grab-pointer #t target-win '() 'async 'async (xcb-none xwindow)
                           (or keymap-cursor-val (xcb-none xcursor)) 0)
             (grab-keyboard #f target-win 0 'async 'async)
             (let ((key (key-listener)))
               (ungrab-pointer 0)
               (ungrab-keyboard 0)
               key)))
          (get-key-later (lambda (proc) (key-listener proc))))
     (get-next-key get-key-now get-key-later))))
