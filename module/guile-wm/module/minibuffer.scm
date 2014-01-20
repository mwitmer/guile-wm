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

(define-module (guile-wm module minibuffer)
  #:use-module (guile-wm keymap)
  #:use-module (guile-wm log)
  #:use-module (guile-wm text)
  #:use-module (guile-wm keystroke)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm keysyms)
  #:use-module (guile-wm command)
  #:use-module (guile-wm module message)
  #:use-module (guile-wm module root-keymap)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm text-edit)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 vlist)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:export (minibuffer minibuffer-keymap (font-string . minibuffer-font)))

(define font-string "fixed")

(define (cancel data) (set-data-state data 'cancel))
(define (execute data) (set-data-state data 'execute))

(define-keymap minibuffer-keymap ()
  (C-g         => cancel)
  (escape      => cancel)
  (return      => execute)
  (left        => point-left)
  (C-b         => point-left)
  (right       => point-right)
  (C-f         => point-right)
  (C-e         => point-end)
  (C-a         => point-start)
  (M-f         => point-forwards-word)
  (M-b         => point-backwards-word)
  (C-d         => delete-forwards)
  (backspace   => delete-backwards)
  (M-backspace => delete-backwards-word)
  (M-d         => delete-forwards-word)
  (C-n         => point-down)
  (down        => point-down)
  (C-p         => point-up)
  (up          => point-up)
  (C-j         => insert-newline)
  (C-k         => kill-to-end-of-line))

(define (run-keymap get put prompt action)
  (define keymap (keymap-attach minibuffer-keymap default show process))
  (define (default key data)
    (or (and=> (sym->printable key) (lambda (p) (point-insert data p))) data))
  (define (loop data) (keymap-lookup keymap get data))
  (define (prepare-and-put data)
    (let ((x (car (data-point data))) (y (cdr (data-point data)))
          (first-line (string-append prompt (vlist-head (data-text data)))))
      (put (vlist-cons first-line (vlist-tail (data-text data)))
           (cons (if (= y 0) (+ x (string-length prompt)) x) y))))
  (define (show data) (prepare-and-put data) data)
  (define ((finish data) state)
    (define command (string-join (vlist->list (data-text data))))
    (case state
      ((execute) (action command))
      ((cancel) (log! "Minibuffer cancelled."))))
  (define (process get-data)
    (define data (get-data))
    (case (data-state data)
      ((read) (loop data))
      (else => (finish data))))
  (with-root-keymap-disabled
   (loop (make-text-edit-data 'read (cons 0 0) (vlist-cons "" vlist-null)))))

(define (prepare-text unescaped-lines point)
  (define row (cdr point))
  (define (escape-line line) (string-join (string-split line #\^) "^^"))
  (define lines (vlist-map escape-line unescaped-lines))
  (define point-line (vlist-ref lines row))
  (define col (car point))
  (define escaped-col
    (+ (let look-for-carat ((n 0) (carats 0) 
                            (str (vlist-ref unescaped-lines row)))
         (cond
          ((= n col) carats)
          ((char=? (string-ref str n) #\^)
           (look-for-carat (+ n 1) (+ carats 1) str))
          (else (look-for-carat (+ n 1) carats str))))
       col))
  (define (with-invert n)
    (string-append
     (substring point-line 0 escaped-col)
     (format #f "^(invert)~a^(invert)"
             (substring point-line escaped-col (+ escaped-col n)))
     (substring point-line (+ escaped-col n))))
  (define line-with-point
    (cond
     ((= escaped-col (string-length point-line))
      (string-append point-line "^(invert) ^(invert)"))
     ((char=? (string-ref point-line escaped-col) #\^) (with-invert 2))
     (else (with-invert 1))))
  (string-join
   (vlist->list
    (vlist-append (vlist-take lines row)
                  (vlist-cons line-with-point vlist-null)
                  (vlist-drop lines (+ row 1))))
   "\n"))

(define-public (minibuffer prompt action)
  (define (run-minibuffer)
    (define get-next-key
      (keystroke-listen! minibuffer-window minibuffer-key-tag))
    (define (update-text text point)
      (put-text
       (prepare-text text point) minibuffer-window 'white 'black font-string))
    (map-window minibuffer-window)
    (configure-window minibuffer-window #:stack-mode 'above #:height 10 #:width 10)
    (run-keymap get-next-key update-text prompt action)
    (unmap-window minibuffer-window))
  (if (not (minibuffer-active?))
      (parameterize ((minibuffer-active? #t)) (run-minibuffer))))

(define-command (prompt-for-eval)
  (minibuffer "eval: " (lambda (cmd) (message (format #f "~a" (wm-eval cmd))))))
(define-command (prompt-for-command) (minibuffer "command: " run-command))
(define-command (prompt-for-shell-command)
  (minibuffer "/usr/bin/sh -c: " shell-command))
(define-command (prompt-for-no-reason)
  (minibuffer "type something: " identity))

(define minibuffer-active? (make-parameter #f))
(define-once minibuffer-window #f)
(define minibuffer-key-tag (make-tag 'minibuffer))

(register-guile-wm-module!
 (lambda () (set! minibuffer-window (fixed-window-create 0 0 200 20 0))))
