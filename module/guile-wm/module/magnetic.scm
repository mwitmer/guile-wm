(define-module (guile-wm module magnetic)
  #:use-module (guile-wm shared)
  #:use-module (xcb xml)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop))

(use-wm-modules tinywm tiling)

(wm-init
 (lambda ()
   (add-wm-hook!
    tinywm-drag-end-hook
    (lambda (win)
      (with-replies ((geom get-geometry win))
        (let ((new-tile (tile-at (xref geom 'x) (xref geom 'y))))
          (if (not (xid= blank-x-window win))
              (move-tile selected-tile new-tile))))))
   (add-wm-hook!
    tinywm-resize-end-hook
    (lambda (win)
      (let ((tile (tile-for win)))
        (move-tile tile tile))))))
