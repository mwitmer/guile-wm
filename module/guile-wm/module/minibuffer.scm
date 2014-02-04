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
  #:use-module (guile-wm module root-keymap)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm text-edit)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 vlist)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:export (minibuffer minibuffer-keymap (font-string . minibuffer-font)))

(use-wm-modules message)

(define font-string "fixed")

(define-prompt-keymap minibuffer-keymap
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

(define* (minibuffer prompt #:optional action)
  (define (put text point)
    (put-text
     (prepare-text text point) minibuffer-window 'white 'black font-string))
  (define (continue-minibuffer data)
    (let ((x (car (data-point data))) (y (cdr (data-point data)))
          (first-line (string-append prompt (vlist-head (data-text data)))))
      (put (vlist-cons first-line (vlist-tail (data-text data)))
           (cons (if (= y 0) (+ x (string-length prompt)) x) y))))
  (define (confirm-minibuffer data)
    (define str (string-join (vlist->list (data-text data)) "\n"))
    (unmap-window minibuffer-window)
    (if action (action str) str))
  (define (cancel-minibuffer data)
    (unmap-window minibuffer-window)
    (message "Minibuffer cancelled") *unspecified*)
  (hide-message)
  (configure-window minibuffer-window #:stack-mode 'above #:height 10 #:width 10)
  (map-window minibuffer-window)
  (with-root-keymap-disabled
   (do-prompt-keymap
    (prompt-keymap-with-default
     minibuffer-keymap
     (lambda (key data)
       (or (and=> (sym->printable key) (lambda (p) (point-insert data p)))
           data)))
    (keystroke-listen! minibuffer-window)
    continue-minibuffer confirm-minibuffer cancel-minibuffer
    (empty-text-edit-data))))

(define-public (prompt-for-additional-arg arg-name type)
  (minibuffer (format #f "~a [~a]: " arg-name (keyword->symbol type))))

(define-command (prompt-for-eval)
  "Prompt for a Guile scheme expression, evaluate it, and display the
result."
  (minibuffer "eval: " (lambda (cmd) (message (format #f "~a" (wm-eval cmd))))))

(define-command (prompt-for-command)
  "Prompt for a Guile-WM command and run it."
  (define cmd (minibuffer "command: "))
  (if (not (unspecified? cmd)) (run-command cmd prompt-for-additional-arg)))

(define-command (prompt-for-shell-command)
  "Prompt for a shell command and run it."
  (minibuffer "/usr/bin/sh -c: " shell-command))

(define-once minibuffer-window #f)

(wm-init
 (lambda () (set! minibuffer-window (fixed-window-create 0 0 200 20 0))))
