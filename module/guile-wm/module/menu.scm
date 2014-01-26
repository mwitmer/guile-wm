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

(define-module (guile-wm module menu)
  #:use-module (guile-wm keymap)
  #:use-module (guile-wm log)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm keystroke)
  #:use-module (guile-wm module root-keymap)
  #:use-module (guile-wm text)
  #:use-module (guile-wm command)
  #:use-module (guile-wm draw)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 vlist)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:export (menu menu-keymap (font-string . menu-font)))

(define font-string "fixed")

(define (cancel state row-count row) (values 'cancel row-count row))
(define (execute state row-count row) (values 'execute row-count row))
(define (point-down state row-count row)
  (values state row-count (min (+ row 1) (- row-count 1))))
(define (point-up state row-count row)
  (values state row-count (max 0 (- row 1))))
(define (point-circulate state row-count row)
  (values state row-count (if (= (+ row 1) row-count) 0 (+ row 1))))

(define-keymap menu-keymap
  (C-g         => cancel)
  (escape      => cancel)
  (C-n         => point-down)
  (down        => point-down)
  (tab         => point-circulate)
  (C-p         => point-up)
  (up          => point-up)
  (return      => execute))

(define menu-active? (make-parameter #f))
(define-once menu-window #f)

(define (run-keymap get put prompt choice-alist action default)
  (define choice-vlist (list->vlist (map car choice-alist)))
  (define keymap (keymap-with-default menu-keymap keymap-ignore))
  (define (loop state row-count row)
    (put (vlist-cons prompt choice-vlist) (+ 1 row))
    (process (keymap-lookup keymap get state row-count row)))
  (define ((finish row) state)
    (case state 
      ((execute) (action (cdr (list-ref choice-alist row))))
      ((cancel) (action default))))
  (define (process get-data)
    (call-with-values get-data
      (lambda (state row-count row)
        (case state
          ((select) (loop state row-count row))
          (else => (finish row))))))
  (with-root-keymap-disabled
   (loop 'select (vlist-length choice-vlist) 0)))

(define (prepare-text lines row)
  (define with-highlight
    (vlist-append
     (vlist-take lines row)
     (vlist-cons
      (string-append "^(invert)" (vlist-ref lines row) "^(invert)")
      (vlist-drop lines (+ row 1)))))
  (string-join (vlist->list with-highlight) "\n"))

(define-public (menu prompt choice-alist action default)
  (define (run-menu)
    (define get-next-key (keystroke-listen! menu-window))
    (define (update-text text row)
      (put-text (prepare-text text row) menu-window 'white 'black font-string))
    (map-window menu-window)
    (configure-window menu-window #:stack-mode 'above)
    (run-keymap get-next-key update-text prompt choice-alist action default)
    (unmap-window menu-window))
  (if (not (menu-active?)) (parameterize ((menu-active? #t)) (run-menu))))

(wm-init
 (lambda () (set! menu-window (fixed-window-create 0 0 200 20 0))))
