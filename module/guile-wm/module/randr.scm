;; This file is part of Guile WM

;;    Guile WM is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    Guile WM is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with Guile WM.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guile-wm module randr)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format) 
  #:use-module (ice-9 receive) 
  #:use-module (ice-9 curried-definitions) 
  #:use-module (xcb xml) 
  #:use-module (xcb event-loop) 
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml ext randr)
  #:use-module (guile-wm command)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm log))

;; Handling modes

(define ((mode-res= info1) info2)
  (and
   (= (xref info1 'width) (xref info2 'width))
   (= (xref info1 'height) (xref info2 'height))))

(define (mode-refresh mode)
  (define (get n) (xref mode n))
  (define vtotal
    (cond
     ((memq 'double-scan (get 'mode-flags)) (* (get 'vtotal) 2))
     ((memq 'interlace (get 'mode-flags)) (/ (get 'vtotal) 2))
     (else (get 'vtotal))))
  (exact->inexact
   (if (logand (get 'htotal) vtotal) 
       (/ (get 'dot-clock) (* (get 'htotal) vtotal))
       0)))

(define (sort-rotations current-rotations)
  (define rotation-names (xenum-keys rotation))
  (sort current-rotations
        (lambda (rot1 rot2) 
          (> (length (memq rot1 rotation-names))
             (length (memq rot2 rotation-names))))))

(define rotation-names
  '((rotate-0 . "normal")
    (reflect-x . "x axis")
    (reflect-y . "y axis")
    (rotate-90 . "right")
    (rotate-270 . "left")
    (rotate-180 . "inverted")))

(define-command (set-resolution! (output #:string) 
                                 (width #:number)
                                 (height #:number))
  (resolution output width height))

(define-command (set-offset! (output #:string) (x #:number) (y #:number))
  (offset output x y))

(define-command (disable-screen! (output #:string))
  (disable output))

(define-command (rotate-screen! (output #:string) (rotation #:symbol))
  (rotate output rotation))

(define (mode-pair mode-info) (cons (xref mode-info 'id) mode-info))

(define (xid-lookup info-alist xid) (assv-ref info-alist (xid->integer xid)))

(define (get-current-mode output-info)
  (define crtc-info (xid-lookup (crtc-infos) (xref output-info 'crtc)))
  (if (get-crtc-info-reply? crtc-info) 
      (xid-lookup (mode-infos) (xref crtc-info 'mode)) 
      #f))

(define (info-lookup info-alist info xid-type)
  (define (swap p) (cons (cdr p) (car p)))
  (define (=>xid n) (make-xid n xid-type))
  (and=> (assq-ref (map swap info-alist) info) =>xid))

(define (get-infos request xids)
  (define solicit-cdr (lambda (p) (cons (car p) (solicit (cdr p)))))
  (define (make-request xid)
    (cons (xid->integer xid) (delay-reply request xid xcb-current-time)))
  (map solicit-cdr (map make-request (vector->list xids))))

(define (get-mode-infos) 
  (map mode-pair (vector->list (xref (screen-resources) 'modes))))

(define (get-mode-by-resolution width height output-info)
  (define (match mode) 
    (and (= (xref mode 'width) width)
         (= (xref mode 'height) height)
         (memv (xref mode 'id)
               (map xid->integer 
                    (vector->list (xref output-info 'modes))))))
  (find match (map cdr (mode-infos))))

(define (get-output-info-by-name str)
  (define (oi= oi) (string= (xref-string oi 'name) str))
  (find oi= (map cdr (output-infos))))

(define (get-crtc-for-output output-info)
  (define (crtc-lookup xid) (xid-lookup (crtc-infos) xid))
  (define (first-available-crtc crtcs)
    (define (available? crtc) (= (vector-length (xref crtc 'outputs)) 0))
    (find available? crtcs))
  (or (xid-lookup (crtc-infos) (xref output-info 'crtc))
      (first-available-crtc
       (map crtc-lookup (vector->list (xref output-info 'crtcs))))))

(define (get-crtc-xid crtc-info) (info-lookup (crtc-infos) crtc-info xcrtc))

(define (crtc-dimens ci)
  (define (get p) (xref ci p))
  (if (or (not ci) (bad-crtc-error? ci)) 
      '((x . 0) (y . 0) (height . 0) (width . 0))
      `((x . ,(get 'x)) (y . ,(get 'y))
        (height . ,(get 'height)) (width . ,(get 'width)))))

(define-public (get-output-dimensions)
  (define (get-crtc-info oi) (xid-lookup (crtc-infos) (xref oi 'crtc)))
  (map (lambda (oi) (crtc-dimens (get-crtc-info oi))) (map cdr (output-infos))))

(define (get-screen-size dimens)
  (let current ((width 0) (height 0) (dimens dimens))
    (if (null? dimens) (cons width height)
        (let ((dimen (car dimens)))
          (current
           (max (+ (assoc-ref dimen 'width) (assoc-ref dimen 'x)) width)
           (max (+ (assoc-ref dimen 'height) (assoc-ref dimen 'y)) height)
           (cdr dimens))))))

(define (crtc-modify! ci-entry transform)
  (define result (transform (cdr ci-entry) (make-xid (car ci-entry) xcrtc)))
  (if (set-crtc-config-reply? result)
      (case (xref result 'status)
        ((success) result)
        ((invalid-time) (error "Invalid time provided in call \
to SetCrtcConfig"))
        ((failed) (error "Call to SetCrtcConfig failed")))
      result))

(define (dimensions-too-small? dimensions)
  (define current-dimensions (screen-dimensions))
  (or (< (car dimensions) (car current-dimensions))
      (< (cdr dimensions) (cdr current-dimensions))))

(define-public (screen-dimensions) 
  (if (screen-resources) 
      (get-screen-size (get-output-dimensions))
      #f))

(define (randr-update! change-xids)
  (define screen (current-screen))
  (define root (current-root))
  (define ((if-changed proc) entry) 
    (if (hashv-ref change-xids (car entry)) (proc entry)))
  (define (update-screen-size! dimens)
    (set-screen-size root (car dimens) (cdr dimens)
                     (xref screen 'width-in-millimeters)
                     (xref screen 'height-in-millimeters)))    
  (define (update-crtc! ci-entry)
    (define (update ci xid)
      (reply-for set-crtc-config
                 xid
                 (xref ci 'timestamp) (xref (screen-resources) 'timestamp)
                 (xref ci 'x) (xref ci 'y) (xref ci 'mode)
                 (xref ci 'rotation) (xref ci 'outputs)))
    (define new-screen-size (get-screen-size (get-output-dimensions)))
    (if (dimensions-too-small? new-screen-size)
        (disable-crtc! ci-entry))
    (update-screen-size! new-screen-size)
    (crtc-modify! ci-entry update))
  (grab-server)
  (for-each (if-changed update-crtc!) (crtc-infos))
  (ungrab-server))

(define (disable-crtc! ci-entry)
  (define (disable ci xid)
    (reply-for set-crtc-config
               xid xcb-current-time xcb-current-time
               (xref ci 'x) (xref ci 'y) (xcb-none xmode) 
               (xref ci 'rotation) #()))
  (crtc-modify! ci-entry disable))

(define (randr-print)
  (define (format-screen-info)
    (define screen-sizes (xref (screen-info) 'sizes))
    (define size-range (reply-for get-screen-size-range (current-root)))
    (define current-size 
      (get-screen-size (get-output-dimensions)))

    (let ((get (lambda (n) (xref size-range n))))
      (format #f "Screen ~a: minimum ~a x ~a, current ~a x ~a, maximum ~a x ~a"
              (string-take-right
               (xcb-connection-display (current-xcb-connection)) 1)
              (get 'min-width) (get 'min-height)
              (car current-size) (cdr current-size)
              (get 'max-width) (get 'max-height))))

  (define (format-connected-output-info output-info)
    (define output-name (xref-string output-info 'name))
    (define crtc-info (get-crtc-for-output output-info))
    (define rotations 
      (map (lambda (rotation) (assq-ref rotation-names rotation))
           (sort-rotations (xref crtc-info 'rotations))))
    (format #f "~a connected ~ax~a+~a+~a ~a ~amm x ~amm" 
            output-name 
            (xref crtc-info 'width) (xref crtc-info 'height) 
            (xref crtc-info 'x) (xref crtc-info 'y) 
            rotations
            (xref output-info 'mm-width) (xref output-info 'mm-height)))

  (define (format-disconnected-output-info output-info)
    (define output-name (xref-string output-info 'name))
    (format #f "~a disconnected ~a" output-name (map cdr rotation-names)))

  (define (print-output-info output-info)
    (define current-mode (get-current-mode output-info))
    (define mode-xids (xref output-info 'modes))
    (define (mode-match xid) (xid-lookup (mode-infos) xid))
    (define (preferred-mode? mode-info) 
      (define num-preferred (xref output-info 'num-preferred))
      (let get-preferred ((i 0))
        (cond
         ((= i num-preferred) #f)
         ((= (xid->integer (vector-ref mode-xids i)) (xref mode-info 'id)) #t)
         (else (get-preferred (+ i 1))))))
    (define (print-modes mode-info-list)
      (define info (car mode-info-list))
      (define (print-refresh info) 
        (format #t " ~6,1f~a~a" (mode-refresh info)
                (if (eq? info current-mode) "*" " ")
                (if (preferred-mode? info) "+" " ")))
      (receive (infos rest) (partition (mode-res= info) mode-info-list)
        (format #t "   ~12a" 
                (format #f "~ax~a" (xref info 'width) (xref info 'height)))
        (for-each print-refresh infos) (newline)
        (if (not (null? rest)) (print-modes rest))))   
    (format #t "~a\n"
            (case (xref output-info 'connection) 
              ((connected) (format-connected-output-info output-info))
              (else (format-disconnected-output-info output-info))))
    (if (> (vector-length mode-xids) 0) 
        (print-modes (map mode-match (vector->list mode-xids)))))
  (format #t "~a\n" (format-screen-info))
  (for-each print-output-info (map cdr (output-infos))))

(define (call-if-crtc-present crtc-infos output-infos output-name proc)
  (define (with-output output-info)
    ((and=> (get-crtc-for-output output-info) proc) output-info))
  (and=> (get-output-info-by-name output-name) with-output))

(define (rotate output-name rotation)
  (define change-xids (make-hash-table))
  (define (mark-xid! xid) (hashv-set! change-xids (xid->integer xid) #t))
  (define ((do-it crtc-info) display-info)
    (xset! crtc-info 'rotation rotation)
    (mark-xid! (get-crtc-xid crtc-info)))
  (call-if-crtc-present (crtc-infos) (output-infos) output-name do-it)
  (randr-update! change-xids))

(define (disable output-name) 
  (define change-xids (make-hash-table))
  (define (mark-xid! xid) (hashv-set! change-xids (xid->integer xid) #t))
  (define ((do-it crtc-info) display-info)
    (xset! crtc-info 'mode (xcb-none xmode))
    (xset! crtc-info 'outputs #())
    (mark-xid! (get-crtc-xid crtc-info)))
  (call-if-crtc-present (crtc-infos) (output-infos) output-name do-it)
  (randr-update! change-xids))

(define (offset output-name x y)
  (define change-xids (make-hash-table))
  (define (mark-xid! xid) (hashv-set! change-xids (xid->integer xid) #t))
  (define ((do-it crtc-info) output-info)
    (define crtc-xid (info-lookup (crtc-infos) crtc-info xcrtc))
    (xset! crtc-info 'x x)
    (xset! crtc-info 'y y)
    (mark-xid! (get-crtc-xid crtc-info)))
  (call-if-crtc-present (crtc-infos) (output-infos) output-name do-it)
  (randr-update! change-xids))

(define (resolution output-name width height)
  (define change-xids (make-hash-table))
  (define (mark-xid! xid) (hashv-set! change-xids (xid->integer xid) #t))
  (define ((do-it crtc-info) output-info)
    (define output-xid (info-lookup (output-infos) output-info xoutput))
    (define new-mode (get-mode-by-resolution width height output-info))
    (xset! crtc-info 'mode (make-xid (xref new-mode 'id) xmode))
    (xset! crtc-info 'height (xref new-mode 'height))
    (xset! crtc-info 'width (xref new-mode 'width))
    (if (= (vector-length (xref crtc-info 'outputs)) 0)
        (xset! crtc-info 'outputs (vector output-xid)))
    (mark-xid! (get-crtc-xid crtc-info)))
  (call-if-crtc-present (crtc-infos) (output-infos) output-name do-it)
  (randr-update! change-xids))

(define-once current-screen-dimensions (make-parameter #f))
(define-once screen-resources (make-parameter #f))
(define-once crtc-infos (make-parameter #f))
(define-once output-infos (make-parameter #f))
(define-once mode-infos (make-parameter #f))
(define-once screen-info (make-parameter #f))

(define (setup-randr change)
  (screen-resources (reply-for get-screen-resources-current (current-root)))
  (let ((outputs (xref (screen-resources) 'outputs))
        (crtcs (xref (screen-resources) 'crtcs)))
    (screen-info (reply-for get-screen-info (current-root)))
    (output-infos (get-infos get-output-info outputs))
    (crtc-infos (get-infos get-crtc-info crtcs))
    (mode-infos (get-mode-infos))))

(register-guile-wm-module!
 (lambda ()
   (define enable-randr (solicit (delay-enable-extension 'randr)))
   (select-input (current-root) '(crtc-change screen-change output-change))
   (listen! screen-change-notify-event 'screen-change-notify setup-randr)
   (setup-randr #f)))
