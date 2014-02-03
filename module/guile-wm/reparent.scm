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
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm icccm)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm redirect))

(define-public (wm-reparent-window child parent x y)
  "Reparent window CHILD inside of window PARENT with offset
coordinates (X, Y), and set up event handlers for size, visibility,
and mapping state changes. The parent window will be destroyed when
the child window is unmapped."
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
    (create-listener (stop!)
      ((visibility-notify-event visibility #:window parent)
       (if (eq? (xref visibility 'state) 'fully-obscured)
           (hashv-set! obscured-windows (xid->integer child) #t)
           (hashv-remove! obscured-windows (xid->integer child))))
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
         #:width (max (- (xref configure 'width) x) 0)))
      ((configure-request-event configure #:window child)
       (configure-window parent
         #:height (max (+ (xref configure 'height) child-border y) 0)
         #:width (max (+ (xref configure 'width) child-border x) 0))))))

;; Call with: unmap-notify event, parent window
(define-public unmap-notify-hook (make-wm-hook 2))
;; Call with: child window, parent window
(define-public after-reparent-hook (make-wm-hook 2))
;; Support for basic reparenting

(define-public (on-map map-request)
  (define xcb-conn (current-xcb-connection))
  (define original-parent (xref map-request 'parent))
  (define child (xref map-request 'window))
  (if (not (hashv-ref reparents (xid->integer child)))
      (let ((new-parent (basic-window-create 0 0 1 1 2 '())))
        (grab new-parent)
        (wm-reparent-window child new-parent 0 0)
        (map-window child)
        (cond
         ((wm-hook-empty? after-reparent-hook)
          (map-window child)
          (map-window new-parent)
          (set-window-state! child window-state-normal)
          (set-focus child))
         (else (run-wm-hook after-reparent-hook child new-parent))))))

(define-public (on-configure configure-request)
  (define value-mask (xref configure-request 'value-mask))
  (define win (xref configure-request 'window))
  (define (get-prop prop)
    (define val (xref configure-request prop))
    (cons (symbol->keyword prop)
          (case prop
            ((sibling) (xid->integer val))
            ((stack-mode) (xenum-ref stack-mode val))
            (else val))))
  (apply configure-window win
         (let flatten ((i (map get-prop value-mask)) (o '()))
           (if (null? i) o (flatten (cdr i) `(,(caar i) ,(cdar i) ,@o))))))

(define-public (on-circulate circulate-request) #f)

(define (grab win)
  (grab-button #f win '(button-press) 'sync 'async (xcb-none xwindow)
               (xcb-none xcursor) '#{1}# '()))
