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

(define-module (guile-wm reparent)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 curried-definitions)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm icccm)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm redirect))

;; Call with: unmap-notify event, parent window
(define-public unmap-notify-hook (make-wm-hook 2))
;; Call with: child window, parent window
(define-public after-reparent-hook (make-wm-hook 2))
;; Call with: configure-request event
(define-public configure-request-hook (make-wm-hook 1))
;; Call with: circulate-request event
(define-public circulate-request-hook (make-wm-hook 1))

(define reparents (make-hash-table))
(define obscured (make-hash-table))

(define-public (reparented-windows)
  (define reparented
    (hash-map->list (lambda (k v) (make-xid k xwindow)) reparents))
  (if (null? reparented) #f reparented))

(define-public (window-obscured? win)
  (hashv-ref obscured (xid->integer win)))

(define-public (window-child win)
  (define (xwcons k v) (cons (xid->integer v) (make-xid k xwindow)))
  (or (assv-ref (hash-map->list xwcons reparents) (xid->integer win)) win))

(define-public (window-parent win)
  (or (hashv-ref reparents (xid->integer win)) win))

(define-public (reparented? win)
  (not (not (hashv-ref reparents (xid->integer win)))))

(define (wm-reparent-window child parent x y)
  (hashv-set! reparents (xid->integer child) parent)
  (with-replies ((geom get-geometry child))
    (define child-border (* 2 (xref geom 'border-width)))
    (configure-window parent
      #:x (xref geom 'x) #:y (xref geom 'y)
      #:width (+ child-border (xref geom 'width))
      #:height (+ child-border (xref geom 'height)))
    (change-window-attributes parent
      #:event-mask '(button-press substructure-redirect substructure-notify
                                  structure-notify visibility-change))
    (reparent-window child parent x y)
    (change-window-attributes child #:event-mask '(structure-notify))
    (map-window child)
    (create-listener (stop!)
      ((visibility-notify-event visibility #:window parent)
       (if (eq? (xref visibility 'state) 'fully-obscured)
           (hashv-set! obscured (xid->integer child) #t)
           (hashv-remove! obscured (xid->integer child))))
      ((unmap-notify-event unmap-notify #:window child)
       (when (not (or (= (xref unmap-notify 'sequence-number) 0)
                      (xid= (xref unmap-notify 'window)
                            (xref unmap-notify 'event))))
         (if (and current-focus (xid= child current-focus)) (set! current-focus #f))
         (if (wm-hook-empty? unmap-notify-hook)
             (unmap-window parent)
             (run-wm-hook unmap-notify-hook unmap-notify parent))))
      ((destroy-notify-event destroy-notify #:window child)
       (hashv-remove! reparents (xid->integer child))
       (destroy-window parent)
       (stop!))
      ((configure-notify-event configure #:window parent)
       (configure-window child
         #:height (max (- (xref configure 'height) y) 0)
         #:width (max (- (xref configure 'width) x) 0))))))

(define ((on-map create-parent child-x child-y) map-request)
  (define child (xref map-request 'window))
  (when (not (hashv-ref reparents (xid->integer child)))
    (let ((new-parent (create-parent)))
      (grab-button #f new-parent '(button-press) 'sync 'async
                   (xcb-none xwindow) (xcb-none xcursor) '#{1}# '())
      (wm-reparent-window child new-parent child-x child-y)
      (cond
       ((wm-hook-empty? after-reparent-hook)
        (map-window new-parent)
        (set-window-state! child window-state-normal)
        (set-focus child))
       (else (run-wm-hook after-reparent-hook child new-parent))))))

(define (disallow-configure configure-request)
  (run-wm-hook configure-request-hook configure-request))

(define (allow-configure configure-request)
  (define value-mask (xref configure-request 'value-mask))
  (define win (xref configure-request 'window))
  (define (get-prop prop)
    (define val (xref configure-request prop))
    (cons (symbol->keyword prop)
          (case prop
            ((sibling) (xid->integer val))
            ((stack-mode) (xenum-ref stack-mode val))
            (else val))))
  (run-wm-hook configure-request-hook configure-request)
  (apply configure-window win
         (let flatten ((i (map get-prop value-mask)) (o '()))
           (if (null? i) o (flatten (cdr i) `(,(caar i) ,(cdar i) ,@o))))))

(define (disallow-circulate circulate-request)
  (run-wm-hook circulate-request-hook circulate-request))

(define (allow-circulate circulate-request)
  (define win (xref circulate-request 'window))
  (define dir (case (xref circulate-request 'place)
                ((on-bottom) 'below)
                ((on-top) 'above)))
  (run-wm-hook circulate-request-hook circulate-request)
  (configure-window (window-parent win) #:stack-mode dir))

(define-public (begin-reparent-redirect!
                create-parent child-x child-y
                allow-configure? allow-circulate?)
  (hash-clear! reparents)
  (end-redirect!)
  (begin-redirect!
   (on-map create-parent child-x child-y)
   (if allow-configure? allow-configure disallow-configure)
   (if allow-circulate? allow-circulate disallow-circulate)))

(define-public (end-reparent-redirect!)
  (end-redirect!)
  (set! reparents #f))
