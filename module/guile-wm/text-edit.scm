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

(define-module (guile-wm text-edit)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-text-edit-data
            set-data-state
            data-text
            data-point
            data-state))

(define-immutable-record-type text-edit-data
  (make-text-edit-data state point text)
  minibuffer-data?
  (state data-state set-data-state)
  (point data-point set-data-point)
  (text data-text set-data-text))

(define (constrain start end n)
  (cond ((< n start) start)
        ((> n end) end)
        (else n)))

(define (move-n lines x y n)
  (define (move-right x y n)
    (define line (vlist-ref lines y))
    (define len (string-length line))
    (define x+n (+ x n))
    (cond
     ((= (+ y 1) (vlist-length lines)) (cons (min len x+n) y)) ;; On the last line
     ((<= x+n len) (cons x+n y)) ;; Fits in the line
     (else (move-right 0 (+ y 1) (- n (- len (- x 1))))))) ;; Move to the next line
  (define (move-left x y n)
    (define line (vlist-ref lines y))
    (define x-n (- x n))
    (cond
     ((= y 0) (cons (max 0 x-n) y)) ;; On the first line
     ((>= x-n 0) (cons x-n y)) ;; Fits in the line
     (else ;; Move to the previous line
      (move-left (+ 1 (string-length (vlist-ref lines (- y 1)))) (- y 1) (- n x)))))
  (if (>= n 0) (move-right x y n) (move-left x y (- n))))

(define-syntax define-text-modifier
  (syntax-rules ()
    ((_ (name text point-x point-y other ...) stmt ...)
     (define-public (name data other ...)
       (call-with-values
           (lambda ()
             (let* ((text (data-text data))
                    (point-x (car (data-point data)))
                    (point-y (cdr (data-point data))))
               stmt ...))
         (lambda (text point-x point-y)
           (set-data-point (set-data-text data text)
                           (cons point-x point-y))))))))

(define (insert-text text str x y)
  (define line (vlist-ref text y))
  (string-append
   (substring line 0 x)
   str (if (= x (string-length line)) "" (substring line x))))

(define (kill-newline text y)
  (if (= y (- (vlist-length text) 1))
      text
      (vlist-append 
       (vlist-take text y)
       (vlist-cons
        (string-append (vlist-ref text y) (vlist-ref text (+ 1 y)))
        (if (< y (- (vlist-length text) 2))
            (vlist-drop text (+ 2 y)) vlist-null)))))
  
(define* (kill-inline text y start-x #:optional end-x)
  (define line (vlist-ref text y))
  (replace-line 
   text y
   (string-append 
    (substring line 0 start-x)
    (if end-x (substring line end-x) ""))))

(define (kill-text text first-start first-end)
  (let kill ((text text) (start first-start) (end first-end))
   (define start-y (cdr start))
   (define end-y (cdr end))
   (define lines-left (- end-y start-y))
   (cond ((= lines-left 0) 
          (kill-inline 
           text start-y (car start) 
           (if (eq? first-end end) (car end) (+ (car start) (car end)))))
         (else (kill 
                (kill-newline (kill-inline text start-y (car start)) start-y)
                start (cons (car end) (- end-y 1)))))))

(define (kill-line text y)
  (vlist-append (vlist-take text y) (vlist-drop text (+ y 1))))

(define (forward-word text x y)
  (define line (vlist-ref text y))
  (define at-end? (= x (string-length line)))
  (define (try-next)
    (if (= y (- (vlist-length text) 1))
        (cons (string-length line) y) (forward-word text 0 (+ y 1))))
  (define (look)
    (define match (string-match "\\W" (substring line (+ x 1))))
    (cond (match (cons (+ x (match:start match) 1) y))
          (at-end? (try-next))
          (else (cons (string-length line) y))))
  (if at-end? (try-next) (look)))

(define (back-word text x y)
  (define line (vlist-ref text y))
  (define at-start? (= x 0))
  (define (try-previous)
    (if (= y 0) (cons 0 y)
        (back-word text (string-length (vlist-ref text (- y 1))) (- y 1))))
  (define (look)
   (define match
     (string-match ".*(\\W)\\w+" (substring line 0 (- x 1))))
   (cond (match (cons (+ 1 (match:start match 1)) y))
         (at-start? (try-previous))
         (else (cons 0 y))))
  (if at-start? (try-previous) (look)))

(define (replace-line text y new-line)
  (vlist-append (vlist-take text y)
                (vlist-cons new-line 
                            (vlist-drop text (+ 1 y)))))

(define (add-newline text x y)
  (define line (vlist-ref text y))
  (vlist-append
   (vlist-take text y)
   (vlist-cons
    (string-take line x)
    (vlist-cons
     (string-drop line x)
     (vlist-drop text (+ y 1))))))

(define-text-modifier (point-start text x y) (values text 0 y))
(define-text-modifier (point-end text x y)
  (values text (string-length (vlist-ref text y)) y))
(define-text-modifier (point-left text x y)
  (define new-point (move-n text x y -1))
  (values text (car new-point) (cdr new-point)))
(define-text-modifier (point-right text x y)
  (define new-point (move-n text x y 1))
  (values text (car new-point) (cdr new-point)))
(define-text-modifier (point-insert text x y str)
  (values (replace-line text y (insert-text text str x y)) (+ x 1) y))
(define-text-modifier (delete-backwards text x y)
  (define new-point (move-n text x y -1))
  (values (kill-text text new-point (cons x y)) 
          (car new-point) (cdr new-point)))
(define-text-modifier (delete-forwards text x y)
  (values (kill-text text (cons x y) (move-n text x y 1)) x y))
(define-text-modifier (kill-to-end-of-line text x y)
  (define line (vlist-ref text y))
  (if (= x (string-length line)) 
      (if (< y (- (vlist-length text) 1))
          (values (kill-line text (+ y 1)) x y)
          (values text x y))
      (values (kill-inline text y x) x y)))
(define-text-modifier (insert-newline text x y)
  (values (add-newline text x y) 0 (+ y 1)))
(define-text-modifier (point-forwards-word text x y)
  (define new-point (forward-word text x y))
  (values text (car new-point) (cdr new-point)))
(define-text-modifier (point-backwards-word text x y)
  (define new-point (back-word text x y))
  (values text (car new-point) (cdr new-point)))
(define-text-modifier (delete-forwards-word text x y)
  (define new-point (forward-word text x y))
  (values (kill-text text (cons x y) new-point) x y))
(define-text-modifier (delete-backwards-word text x y)
  (define new-point (back-word text x y))
  (values 
   (kill-text text new-point (cons x y)) (car new-point) (cdr new-point)))
(define-text-modifier (point-up text x y)
  (define n (- y 1))
  (if (>= n 0)
      (values text (constrain 0 (string-length (vlist-ref text n)) x) n)
      (values text x y)))
(define-text-modifier (point-down text x y)
  (define n (+ y 1))
  (if (< n (vlist-length text))
      (values text (constrain 0 (string-length (vlist-ref text n)) x) n)
      (values text x y)))
