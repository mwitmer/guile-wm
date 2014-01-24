>(define-module (guile-wm module tiling)
  #:use-module (guile-wm module randr)
  #:use-module (guile-wm module window-menu)
  #:use-module (guile-wm reparent)
  #:use-module (guile-wm command)
  #:use-module (guile-wm icccm)
  #:use-module (guile-wm log)
  #:use-module (guile-wm shared)
  #:use-module (guile-wm redirect)
  #:use-module (guile-wm focus)
  #:use-module (guile-wm draw)
  #:use-module (guile-wm color)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml)
  #:use-module ((xcb xml xproto)
                #:select (unmap-window map-window get-geometry configure-window
                                       destroy-notify-event allow-events
                                       change-window-attributes
                                       button-press-event
                                       get-window-attributes))
  #:export (horizontal-split vertical-split
                             reveal-window
                             select-down
                             clear-frame
                             select-up
                             select-left
                             select-right))

;;; Commentary:
;; This is a tiling window manager for guile-wm. It provides support
;; for dividing the screen up into virtual tiles and displaying X
;; windows inside of them. Tiles can be split, resized, and deleted.


;; Frames correspond to randr outputs or the whole screen if randr is
;; not in use. They can't be modified directly by the user

(define-record-type frame
  (make-frame x y height width content)
  frame?
  (x frame-x)
  (y frame-y)
  (height frame-height)
  (width frame-width)
  (content frame-content set-frame-content!))

;; Splits contain two elements, oriented either horizontally or
;; vertically. Those elements can be tiles or more splits

(define-record-type split
  (make-split height width orientation element1 element2)
  split?
  (height split-height set-split-height!)
  (width split-width set-split-width!)
  (orientation split-orientation)
  (element1 split-element1 set-split-element1!)
  (element2 split-element2 set-split-element2!)
  (container split-container set-split-container!))

;; Tiles store x windows. They can store an actual reparented window,
;; the blank window that shows when an empty tile is selected, or
;; they can just be empty.
(define-record-type tile
  (make-tile height width)
  tile?
  (height tile-height set-tile-height!)
  (width tile-width set-tile-width!)
  (window tile-window set-tile-window!)
  (container tile-container set-tile-container!))

;; This is the queue of windows that aren't visible
(define-once hidden-x-windows (make-q))
;; This is the transparent x window that gets displayed when an empty
;; tile is selected
(define-once blank-x-window #f)
;; This is the most recent x window to be hidden, for use by the
;; restore-window command
(define-once most-recent-x-window #f)
;; This is the tile that's currently selected
(define-once selected-tile #f)
;; This is the master list of frames
(define-once frame-list #f)

;; Helper procedures for managing tiles and splits

(define (container-of el)
  (if (split? el)
      (split-container el)
      (tile-container el)))

(define (frame-of el)
  (if (frame? el) el (frame-of (container-of el))))

(define (other-element split el)
  (if (eq? el (split-element1 split))
      (split-element2 split)
      (split-element1 split)))

(define (tile-empty? tile)
  (or (not (tile-window tile))
      (xid= (tile-window tile) blank-x-window)))

(define (width-of el)
  (cond
   ((split? el) (split-width el))
   ((tile? el) (tile-width el))
   (else (frame-width el))))

(define (height-of el)
  (cond
   ((split? el) (split-height el))
   ((tile? el) (tile-height el))
   (else (frame-height el))))

;; Tile and split locations are calculated dynamically. This is way
;; easier than storing them and trying to keep them consistent

(define (calculate-x el)
  (if (frame? el) (frame-x el)
      (let ((container (container-of el)))
        (if (and (split? container)
                 (eq? (split-orientation container) 'horizontal)
                 (eq? el (split-element2 container)))
            (+ (calculate-x container)
               (width-of (split-element1 container)))
            (calculate-x container)))))

(define (calculate-y el)
  (if (frame? el) (frame-y el)
      (let ((container (container-of el)))
      (if (and (split? container)
               (eq? (split-orientation container) 'vertical)
               (eq? el (split-element2 container)))
          (+ (calculate-y container)
             (height-of (split-element1 container)))
          (calculate-y container)))))

(define (coords-in? el x y)
  (let* ((left (calculate-x el))
         (top (calculate-y el))
         (right (+ left (width-of el)))
         (bottom (+ top (height-of el))))
    (and (>= x left)
         (< x right)
         (>= y top)
         (< y bottom))))

(define (frame-at x y)
  (let lp ((frames frame-list))
    (cond
     ((null? frames) #f)
     ((coords-in? (car frames) x y) (car frames))
     (else (lp (cdr frames))))))

(define (tile-at x y)
  (and-let* ((frame (frame-at x y)))
    (if (tile? (frame-content frame)) (frame-content frame)
        (let lp ((split (frame-content frame)))
          (let ((el (if (coords-in? (split-element1 split) x y)
                        (split-element1 split) (split-element2 split))))
            (if (split? el) (lp el) el))))))

;; Location of tiles relative to one another
;; TODO: Remove the assumption that tiles are all contiguous

(define (tile-below tile)
  (tile-at
   (+ (calculate-x tile) (floor (/ 2 (tile-width tile))))
   (+ (calculate-y tile) (tile-height tile) 1)))

(define (tile-above tile)
  (tile-at
   (+ (calculate-x tile) (floor (/ 2 (tile-width tile))))
   (- (calculate-y tile) 1)))

(define (tile-to-right tile)
  (tile-at
   (+ (calculate-x tile) (tile-width tile) 1)
   (+ (calculate-y tile) (floor (/ 2 (tile-height tile))))))

(define (tile-to-left tile)
  (tile-at
   (- (calculate-x tile) 1)
   (+ (calculate-y tile) (floor (/ 2 (tile-height tile))))))

;; Splitting tiles

(define (split-tile tile tile1 tile2 new-split)
  (if (frame? (tile-container tile))
      (set-frame-content! (tile-container tile) new-split)
      (if (eq? (split-element1 (tile-container tile)) tile)
          (set-split-element1! (tile-container tile) new-split)
          (set-split-element2! (tile-container tile) new-split)))
  (set-tile-container! tile1 new-split)
  (set-tile-container! tile2 new-split)
  (set-split-container! new-split (tile-container tile))
  (if (tile-window tile)
      (move-x-window! (tile-window tile) tile1))
  (when (eq? tile selected-tile)
    (select-tile tile1)
    (pop-and-unhide-x-window! tile2)))

(define (split-tile-horizontal! tile)
  (let* ((tile1 (make-tile (tile-height tile)
                           (ceiling (/ (tile-width tile) 2))))
         (tile2 (make-tile (tile-height tile)
                             (floor (/ (tile-width tile) 2))))
         (new-split (make-split (tile-height tile) (tile-width tile)
                                'horizontal tile1 tile2)))
    (split-tile tile tile1 tile2 new-split)))

(define (split-tile-vertical! tile)
  (let* ((tile1 (make-tile (ceiling (/ (tile-height tile) 2))
                           (tile-width tile)))
         (tile2 (make-tile (floor (/ (tile-height tile) 2))
                           (tile-width tile)))
         (new-split (make-split (tile-height tile) (tile-width tile)
                                'vertical tile1 tile2)))
    (split-tile tile tile1 tile2 new-split)))

(define (move-tile old new)
  (define has-backup? (q-empty? hidden-x-windows))
  (and=> (tile-window new) hide-x-window!)
  (move-x-window! (tile-window old) new)
  (if has-backup?
      (set-tile-window! old #f)
      (let ((hidden-window (deq! hidden-x-windows)))
        (move-x-window! hidden-window old)))
  (select-tile new))

(define (for-each-tile proc el)
  (let lp ((content el))
    (cond
     ((frame? content)
      (lp (frame-content content)))
     ((split? content)
      (lp (split-element1 content))
      (lp (split-element2 content)))
     (else (proc content)))))

;; Putting x windows into tiles

(define (move-x-window! x-window tile)
  (set-tile-window! tile x-window)
  (if (and (eq? selected-tile tile)
           (not (xid= x-window blank-x-window)))
      (unmap-window blank-x-window))
  (fit-x-window! x-window tile)
  (discard-hidden-x-window! x-window))

(define (fit-x-window! x-window tile)
  (define geom (reply-for get-geometry x-window))
  (define hints (window-hints x-window))
  (define height-inc (xref hints 'height-inc))
  (define width-inc (xref hints 'width-inc))
  (define target-height (- (tile-height tile) (* (xref geom 'border-width) 2)))
  (define target-width (- (tile-width tile) (* (xref geom 'border-width) 2)))
  (define height (if (= height-inc 0) target-height
                     (* height-inc (quotient target-height height-inc))))
  (define width (if (= width-inc 0) target-width
                    (* width-inc (quotient target-width width-inc))))
  (unmap-window x-window)
  (configure-window x-window
    #:x (calculate-x tile) #:y (calculate-y tile))
  (configure-window x-window #:height height #:width width)
  (map-window x-window))

;; Resizing tiles

(define (set-size-of! el dir val)
  ((cond ((tile? el)
          (if (equal? dir 'vertical) set-tile-height! set-tile-width!))
         ((split? el)
          (if (equal? dir 'vertical) set-split-height! set-split-width!)))
   el val))

(define (get-size-of el dir)
  ((cond ((tile? el)
          (if (equal? dir 'vertical) tile-height tile-width))
         ((split? el)
          (if (equal? dir 'vertical) split-height split-width)))
   el))

(define (resize! el amount dir op)
  (cond
   ((tile? el) (resize-tile! el amount dir op))
   (else (resize-split! el amount dir op))))

(define (resize-tile! tile amount dir op)
  (set-size-of! tile dir (op (get-size-of tile dir) amount)))

(define (resize-split! split amount dir op)
  (set-size-of! split dir (op (get-size-of split dir) amount))
  (cond
   ((eq? (split-orientation split) op)
    (resize! (split-element1 split) (floor (/ amount 2)) dir op)
    (resize! (split-element2 split) (ceiling (/ amount 2)) dir op))
   (else
    (resize! (split-element1 split) amount dir op)
    (resize! (split-element2 split) amount dir op))))

(define (resize-tile-in-context! tile dir op)
  (let lp ((container (tile-container tile)) (el tile) (resized? #f))
    (cond
     ((frame? container)
      (for-each-tile
       (lambda (tile)
         (if (tile-window tile)
             (fit-x-window! (tile-window tile) tile)))
       container))
     ((and (not resized?)
           (eq? (split-orientation container) dir)
           (> (if (eq? op +)
                  (- (get-size-of container dir) (get-size-of el dir))
                  (get-size-of el dir))
              50))
      (resize! el 40 dir op)
      (resize! (other-element container el) 40 dir (if (eq? op +) - +))
      (lp (container-of container) container #t))
     (else (lp (container-of container) container resized?)))))

;; Managing hidden x windows

(define (hide-x-window! x-window)
  (unmap-window x-window)
  (when (not (xid= x-window blank-x-window))
    (when (not (memq x-window (car hidden-x-windows)))
      (set! most-recent-x-window x-window)
      (enq! hidden-x-windows x-window))))

(define (pop-and-unhide-x-window! tile)
  (when (not (q-empty? hidden-x-windows))
    (if (not (tile-empty? tile))
        (hide-x-window! (tile-window tile)))
    (let ((hidden-window (deq! hidden-x-windows)))
      (move-x-window! hidden-window tile))))

(define (discard-hidden-x-window! x-window)
  (define new-q-list
    (reverse
     (let lp ((in (car hidden-x-windows)) (out '()))
       (cond
        ((null? in) out)
        ((xid= (car in) x-window) (lp (cdr in) out))
        (else (lp (cdr in) (cons (car in) out)))))))
  (set! hidden-x-windows
        (if (null? new-q-list)
            (make-q)
            (cons new-q-list (last-pair new-q-list)))))

(define (select-tile tile)
  (if (not (tile-empty? tile))
      (unmap-window blank-x-window)
      (move-x-window! blank-x-window tile))
  (set-focus (window-child (tile-window tile)))
  (set! selected-tile tile))

;; Make this module the window manager

(define (tiling-on-map map-request)
  (and=> (tile-window selected-tile) hide-x-window!)
  (let ((parent (on-map map-request)))
    (move-x-window! parent selected-tile)
    (select-tile selected-tile)
    (create-listener (stop!)
      ((destroy-notify-event destroy-notify #:window parent)
       (discard-hidden-x-window! parent)
       (when (and (tile-window selected-tile)
                  (xid= (tile-window selected-tile) parent))
         (set-tile-window! selected-tile #f)
         (reveal-window))
       (stop!)))))

(define (tiling-on-configure configure-request)
  (on-configure configure-request))

(define (tiling-click-to-focus button-press)
  (and-let* ((win (tile-at
                   (xref button-press 'root-x)
                   (xref button-press 'root-y))))
    (select-tile win))
  (allow-events 'replay-pointer (xref button-press 'time)))

(register-guile-wm-module!
 (lambda ()
   (set! frame-list (detect-frames))
   (set! blank-x-window (basic-window-create 0 0 200 20 2))
   (change-window-attributes blank-x-window #:back-pixmap 'parent-relative)
   (set! selected-tile (frame-content (car frame-list)))
   (listen! button-press-event 'click-to-focus tiling-click-to-focus)
   ;; Start redirecting map/configure/circulate requests right away so
   ;; that we don't miss any of them
   (solicit
    (with-replies ((attributes get-window-attributes (current-root)))
      (define old-events (xref attributes 'your-event-mask))
      (when (not (memq 'button-press old-events))
        (change-window-attributes (current-root)
          #:event-mask (cons 'button-press old-events))
        (solicit
         (begin-redirect! tiling-on-map tiling-on-configure on-circulate)))))))

;; This does the initial work of detecting the frames

(define (detect-frames)
  (if (screen-dimensions)
      (map
       (lambda (dims)
         (define win (make-tile
                      (assq-ref dims 'height)
                      (assq-ref dims 'width)))
         (define frame (make-frame
                        (assq-ref dims 'x) (assq-ref dims 'y)
                        (assq-ref dims 'height) (assq-ref dims 'width)
                        win))
         (set-tile-container! win frame)
         frame)
       (get-output-dimensions))
      (list
       (let* ((height (xref (current-screen) 'height-in-pixels))
              (width (xref (current-screen) 'width-in-pixels))
              (win (make-tile height width))
              (frame (make-frame
                      0 0
                      (xref (current-screen) 'height-in-pixels)
                      (xref (current-screen) 'width-in-pixels)
                      win)))
         (set-tile-container! win frame)
         frame))))

(if (module-defined? (current-module) 'reset-frames)
    (remove-wm-hook! screen-change-hook reset-frames))

;; This gets called after the display configuration changes. The
;; contents of an old frame will get moved to the first new frame that
;; meets the following criteria:

;; 1) It has the same position and size as the old frame

;; 2) It has not already been mapped to a different old frame.

;; If no such frame is found, the frame's windows just get stuck in
;; the hidden window queue.
(define (reset-frames)
  (define (hide! win) (and=> (tile-window win) hide-x-window!))
  (define (find-matching-frame old-frame unused-new-frames)
    (let lp ((unused unused-new-frames))
      (cond
       ((null? unused)
        (for-each-tile hide! old-frame)
        unused-new-frames)
       ((frames-match? old-frame (car unused))
        (let ((new-frame (car unused))
              (content (frame-content old-frame)))
          (set-frame-content! new-frame content)
          (if (split? content)
              (set-split-container! content new-frame)
              (set-tile-container! content new-frame))
          (delq new-frame unused-new-frames)))
       (else (lp (cdr unused-new-frames))))))
  (define (frames-match? frame1 frame2)
    (and (= (frame-x frame1) (frame-x frame2))
         (= (frame-y frame1) (frame-y frame2))
         (= (frame-height frame1) (frame-height frame2))
         (= (frame-width frame1) (frame-width frame2))))
  (and-let* ((frame-list) (new-frames (detect-frames)))
    (fold find-matching-frame new-frames frame-list)
    (set! frame-list new-frames)))

(add-wm-hook! screen-change-hook reset-frames)

(if (module-defined? (current-module) 'tiling-menu-select-window)
    (remove-wm-hook! screen-change-hook tiling-menu-select-window))

(define (tiling-menu-select-window win)
  (move-x-window! (window-parent win) selected-tile)
  (discard-hidden-x-window! (window-parent win))
  (select-tile selected-tile))

(add-wm-hook! menu-select-window-hook tiling-menu-select-window)

;; Command API. It uses "window" instead of tile because people care
;; about their X windows, not the tile abstraction used internally

(define-command (horizontal-split)
  (split-tile-horizontal! selected-tile))

(define-command (vertical-split)
  (split-tile-vertical! selected-tile))

(define-command (select-down)
  (and=> (tile-below selected-tile) select-tile))

(define-command (select-up)
  (and=> (tile-above selected-tile) select-tile))

(define-command (select-left)
  (and=> (tile-to-left selected-tile) select-tile))

(define-command (select-right)
  (and=> (tile-to-right selected-tile) select-tile))

(define-command (move-right)
  (and-let* ((new-tile (tile-to-right selected-tile)))
    (move-tile selected-tile new-tile)))

(define-command (move-left)
  (and-let* ((new-tile (tile-to-left selected-tile)))
    (move-tile selected-tile new-tile)))

(define-command (move-up)
  (and-let* ((new-tile (tile-above selected-tile)))
    (move-tile selected-tile new-tile)))

(define-command (move-down)
  (and-let* ((new-tile (tile-below selected-tile)))
    (move-tile selected-tile new-tile)))

(define-command (grow-window-vertical)
  (resize-tile-in-context! selected-tile 'vertical +))

(define-command (grow-window-horizontal)
  (resize-tile-in-context! selected-tile 'horizontal +))

(define-command (shrink-window-vertical)
  (resize-tile-in-context! selected-tile 'vertical -))

(define-command (shrink-window-horizontal)
  (resize-tile-in-context! selected-tile 'horizontal -))

(define-command (clear-frame)
  (let* ((current-frame (frame-of selected-tile))
         (new-tile (make-tile
                    (frame-height current-frame)
                    (frame-width current-frame))))
    (for-each-tile
     (lambda (tile) (and=> (tile-window tile) hide-x-window!)) current-frame)
    (set-frame-content! current-frame new-tile)
    (set-tile-container! new-tile current-frame)
    (if (tile-window selected-tile)
        (move-x-window! (tile-window selected-tile) new-tile))
    (unmap-window blank-x-window)
    (select-tile new-tile)))

(define-command (reveal-window)
  (pop-and-unhide-x-window! selected-tile)
  (select-tile selected-tile))

(define-command (restore-window)
  (define most-recent most-recent-x-window)
  (when most-recent
    (discard-hidden-x-window! most-recent-x-window)
    (if (not (tile-empty? selected-tile))
        (hide-x-window! (tile-window selected-tile)))
    (move-x-window! most-recent selected-tile)
    (select-tile selected-tile)))

(define-command (delete-split)
  (define container (tile-container selected-tile))
  (for-each-tile
   (lambda (tile) (and=> (tile-window tile) hide-x-window!)) container)
  (if (frame? container) #f
      (let ((new-tile
             (make-tile (split-height container) (split-width container)))
            (super (container-of container)))
        (cond
         ((split? super)
          (if (eq? (split-element1 super) container)
              (set-split-element1! super new-tile)
              (set-split-element2! super new-tile)))
         (else (set-frame-content! super new-tile)))
        (set-tile-container! new-tile super)
        (if (tile-window selected-tile)
            (move-x-window! (tile-window selected-tile) new-tile)
            (move-x-window! blank-x-window new-tile))
        (select-tile new-tile))))
