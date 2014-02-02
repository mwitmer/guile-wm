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

(use-wm-modules message)

(define font-string "fixed")

(define-public (menu-point-down row-count row)
  (values row-count (min (+ row 1) (- row-count 1))))
(define-public (menu-point-up row-count row)
  (values row-count (max 0 (- row 1))))
(define-public (menu-point-circulate row-count row)
  (values row-count (if (= (+ row 1) row-count) 0 (+ row 1))))

(define-prompt-keymap menu-keymap
  (C-n         => menu-point-down)
  (down        => menu-point-down)
  (tab         => menu-point-circulate)
  (C-p         => menu-point-up)
  (up          => menu-point-up))

(define-once menu-window #f)

(define (prepare-text lines row)
  (define with-highlight
    (vlist-append
     (vlist-take lines row)
     (vlist-cons
      (string-append "^(invert)" (vlist-ref lines row) "^(invert)")
      (vlist-drop lines (+ row 1)))))
  (string-join (vlist->list with-highlight) "\n"))

(define* (menu prompt choice-alist action #:optional default)
  (define (continue-menu row-count row)
    (define text (vlist-cons prompt choice-vlist))
    (put-text (prepare-text text (+ 1 row))
              menu-window 'white 'black font-string))
  (define (confirm-menu row-count row)
    (unmap-window menu-window)
    (action (cdr (list-ref choice-alist row))))
  (define (cancel-menu row-count row)
    (unmap-window menu-window)
    (if default (action default)))
  (define choice-vlist (list->vlist (map car choice-alist)))
  (unmap-window message-window)
  (configure-window menu-window #:stack-mode 'above)
  (map-window menu-window)
  (with-root-keymap-disabled
   (do-prompt-keymap
    (prompt-keymap-with-default
     menu-keymap (lambda (key row-num row) (values row-num row)))
    (keystroke-listen! menu-window)
    continue-menu confirm-menu cancel-menu
    (vlist-length choice-vlist) 0)))

(wm-init
 (lambda () (set! menu-window (fixed-window-create 0 0 200 20 0))))
