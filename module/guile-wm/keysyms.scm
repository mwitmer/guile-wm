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

(define-module (guile-wm keysyms)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto))

(define (populate-hash! hsh alist)
  (for-each
   (lambda (keysym-name)
     (define k (car keysym-name))
     (if (number? k)
         (hashv-set! hsh k (cdr keysym-name))
         (hashq-set! hsh k (cdr keysym-name))))
   alist))

(define-public (sym->printable sym)
  (define str (symbol->string sym))
  (if (= (string-length str) 1)
      (substring str 0 1)
      (case sym
        ((space) " ")((bang) "!")((double-quote) "\"")((hash) "#")((dollar) "$")
        ((percent) "%")((ampersand) "&")((single-quote) "'")((left-paren) "(")
        ((right-paren) ")")((asterisk) "*")((plus) "+")((comma) ",")((dash) "-")
        ((period) ".")((slash) "-")((zero) "0")((one) "1")((two) "2")
        ((three) "3")((four) "4")((five) "5")((six) "6")((seven) "7")
        ((eight) "8")((nine) "9")((colon) ":")((semicolon) ";")
        ((greater-than) ">")((equal) "=")((less-than) "<")((question) "?")
        ((at-sign) "@")((left-bracket) "[")((backslash) "\\")
        ((right-bracket) "]")((carat) "^")((underscore) "_")((backtick) "`")
        ((left-brace) "{")((pipe) "|")((right-brace) "}")((tilda) "~")
        (else #f))))

(define human-friendly-latin1-names (make-hash-table))
(define human-friendly-latin1-names-rev (make-hash-table))

(define human-friendly-latin1-alist
  '((#x20 . space)
    (#x21 . bang)
    (#x22 . double-quote)
    (#x23 . hash)
    (#x24 . dollar)
    (#x25 . percent)
    (#x26 . ampersand)
    (#x27 . single-quote)
    (#x28 . left-paren)
    (#x29 . right-paren)
    (#x2A . asterisk)
    (#x2B . plus)
    (#x2C . comma)
    (#x2D . dash)
    (#x2E . period)
    (#x2F . slash)
    (#x30 . zero)
    (#x31 . one)
    (#x32 . two)
    (#x33 . three)
    (#x34 . four)
    (#x35 . five)
    (#x36 . six)
    (#x37 . seven)
    (#x38 . eight)
    (#x39 . nine)
    (#x3A . colon)
    (#x3B . semicolon)
    (#x3C . greater-than)
    (#x3D . equal)
    (#x3E . less-than)
    (#x3F . question)
    (#x40 . at-sign)
    (#x5B . left-bracket)
    (#x5C . backslash)
    (#x5D . right-bracket)
    (#x5E . carat)
    (#x5F . underscore)
    (#x60 . backtick)
    (#x7B . left-brace)
    (#x7C . pipe)
    (#x7D . right-brace)
    (#x7E . tilda)
    (#x7F . delete)))

(populate-hash! human-friendly-latin1-names human-friendly-latin1-alist)
(populate-hash!
 human-friendly-latin1-names-rev
 (hash-map->list (lambda (k v) (cons v k)) human-friendly-latin1-names))

(define keysym-lookup-table (make-hash-table))
(define keysym-lookup-table-rev (make-hash-table))

(define keysym-lookup-alist
  '((#xFF08 . backspace)
    (#xFF09 . tab)
    (#xFF0D . return)
    (#xFF0A . linefeed)
    (#xFF13 . pause)
    (#xFF14 . scroll-lock)
    (#xFF1B . escape)
    (#xFF50 . home)
    (#xFF51 . left)
    (#xFF52 . up)
    (#xFF53 . right)
    (#xFF54 . down)
    (#xFF55 . page-up)
    (#xFF56 . page-down)
    (#xFF57 . end)
    (#xFF61 . print)
    (#xFF63 . insert)
    (#xFF67 . menu)
    (#xFF9F . delete)
    (#xFF7F . num-lock)
    (#xFF80 . space)
    (#xFF89 . tab)
    (#xFF8D . enter)
    (#xFF9C . end)
    (#xFFBE . F1)
    (#xFFBF . F2)
    (#xFFC0 . F3)
    (#xFFC1 . F4)
    (#xFFC2 . F5)
    (#xFFC3 . F6)
    (#xFFC4 . F7)
    (#xFFC5 . F8)
    (#xFFC6 . F9)
    (#xFFC7 . F10)
    (#xFFC8 . F11)
    (#xFFC9 . F12)
    (#xFFCA . F13)
    (#xFFCB . F14)
    (#xFFCC . F15)
    (#xFFCD . F16)
    (#xFFCE . F17)
    (#xFFCF . F18)
    (#xFFD0 . F19)
    (#xFFD1 . F20)
    (#xFFD2 . F21)
    (#xFFD3 . F22)
    (#xFFD4 . F23)
    (#xFFD5 . F24)
    (#xFFD6 . F25)
    (#xFFD7 . F26)
    (#xFFD8 . F27)
    (#xFFD9 . F28)
    (#xFFDA . F29)
    (#xFFDB . F30)
    (#xFFDC . F31)
    (#xFFDD . F32)
    (#xFFDE . F33)
    (#xFFDF . F34)
    (#xFFE1 . left-shift)
    (#xFFE2 . right-shift)
    (#xFFE3 . left-control)
    (#xFFE4 . right-control)
    (#xFFE5 . caps-lock)
    (#xFFE6 . shift-lock)
    (#xFFE7 . left-meta)
    (#xFFE8 . right-meta)
    (#xFFEB . left-super)
    (#xFFEC . right-super)
    (#xFFFF . delete)))

(populate-hash! keysym-lookup-table keysym-lookup-alist)
(populate-hash!
 keysym-lookup-table-rev
 (hash-map->list (lambda (k v) (cons v k)) keysym-lookup-table))

(define-public (make-keysym-table)
  (define connection-setup (xcb-connection-setup (current-xcb-connection)))
  (define min-keycode (xref connection-setup 'min-keycode))
  (define max-keycode (xref connection-setup 'max-keycode))
  (define count (+ (- max-keycode min-keycode) 1))
  (solicit
   (with-replies ((keymap-reply get-keyboard-mapping min-keycode count))
     (define keysym-table (make-hash-table))
     (define keymaps-per-entry (xref keymap-reply 'keysyms-per-keycode))
     (define keymap (xref keymap-reply 'keysyms))
     (let keycode-map ((n min-keycode))
       (define keysyms
         (let group-keysyms ((i 0) (keysyms-for-keycode '()))
           (define new-keysym
             (vector-ref keymap (+ i (* keymaps-per-entry (- n min-keycode)))))
           (define group (cons new-keysym keysyms-for-keycode))
           (if (< i (- keymaps-per-entry 1))
               (group-keysyms (+ i 1) group) group)))
       (hashv-set! keysym-table n (reverse keysyms))
       (if (= n max-keycode) keysym-table (keycode-map (+ n 1)))))))

(define-public (make-modifier-table)
  (solicit
   (with-replies ((mod-reply get-modifier-mapping))
     (define modifier-table (make-hash-table))
     (define keycodes-per-entry (xref mod-reply 'keycodes-per-modifier))
     (define modmap (xref mod-reply 'keycodes))
     (let modmap-map ((n 0))
       (define modifiers
         (let group-keycodes ((i 0) (keycodes-for-modifier '()))
           (define new-keycode
             (vector-ref modmap (+ i (* keycodes-per-entry n))))
           (define group (cons new-keycode keycodes-for-modifier))
           (if (< i (- keycodes-per-entry))
               (group-keycodes (+ i 1) group) group)))
       (hashq-set! modifier-table
                   (xenum-key-ref key-but-mask (expt 2 n))
                   (reverse modifiers))
       (if (= n 7) modifier-table (modmap-map (+ n 1)))))))

(define (keysym->symbol keysym)
  (cond
   ((not keysym) #f)
   ((= 0 keysym) #f) ;; NoSymbol
   ((or
     (and (>= keysym #x20) (<= keysym #x7E))
     (and (>= keysym #xA0) (<= keysym #xFF))) ;; Latin1
    (or
     (hashv-ref human-friendly-latin1-names keysym)
     (string->symbol (string (integer->char keysym)))))
   ((and (>= keysym #x1000100) (<= keysym #x110FFFF)) ;; Unicode
    (string->symbol (string (integer->char (- keysym #x1000000)))))
   (else (hashv-ref keysym-lookup-table keysym))))

(define (symbol->keysym sym)
  (define str (symbol->string sym))
  (define ch (string-ref str 0))
  (define int (char->integer ch))
  (cond
   ((not sym) 0)
   ((= (string-length str) 1)
    (if (or (and (>= int #x20) (<= int #x7E))
            (and (>= int #xA0) (<= int #xFF)))
        (char->integer (string-ref str 0)) ;; Latin1 letter
        (+ #x1000000 (char->integer (string-ref str 0))))) ;; Unicode letter
   ((hashq-ref human-friendly-latin1-names-rev sym) => identity) ;; Latin1 number/symbol
   (else (hashq-ref keysym-lookup-table-rev sym)))) ;; Other keys

(define (get-lock-type modifier-table keysym-table)
  (define lock-mapping (hashq-ref modifier-table 'lock))
  (if (null? lock-mapping)
      #f
      (let search-keycodes ((keycodes lock-mapping))
        (or
         (let search-keysyms ((keysyms (hashq-ref keysym-table (car keycodes))))
           (cond
            ((not keysyms) #f)
            ((eq? (car keysyms) #xFFE5) 'caps-lock)
            ((eq? (car keysyms) #xFFE6) 'shift-lock)
            (else (search-keysyms (cdr keysyms)))))
         (if (null? (cdr keycodes))
             'no-lock (search-keycodes (cdr keycodes)))))))

(define (keysym-has-case? keysym case-proc)
  (define (convert)
    (define str (symbol->string (keysym->symbol keysym)))
    (and (= (string-length str) 1)
         (case-proc (string-ref str 0))))
  (if (> keysym 0) (convert) #f))

(define (to-case keysym proc)
  (define str (symbol->string (keysym->symbol keysym)))
  (char->integer (proc (string-ref str 0))))

(define (keypad-sym? keysym)
  (or (and (>= keysym #xFF80) (<= keysym #xFFBD))
      (and (>= keysym #x11000000) (<= keysym #x1100FFFF))))

(define-public (get-num-lock-modifier modifier-table keysym-table)
  (let look-in-modifiers ((modifiers (hash-map->list cons modifier-table)))
    (define modifier (caar modifiers))
    (cond
     ((let look-in-keycodes ((keycodes (cdar modifiers)))
        (cond
         ((null? keycodes) #f)
         ((= 0 (car keycodes)) #f)
         ((let look-in-keysyms
              ((keysyms (hashv-ref keysym-table (car keycodes))))
            (cond ((not keysyms) #f)
                  ((null? keysyms) #f)
                  ((= (car keysyms) #xFF7F) modifier)
                  (else (look-in-keysyms (cdr keysyms))))) => const)
         (else (look-in-keycodes (cdr keycodes))))) => const)
     ((null? (cdr modifiers)) #f)
     (else (look-in-modifiers (cdr modifiers))))))

(define-public
  (key->symbol modifier-table keysym-table keycode modifiers num-lock-modifier)
  (define shift? (memq 'shift modifiers))
  (define lock? (memq 'lock modifiers))
  (define num-lock? (memq num-lock-modifier modifiers))
  (define lock-type (get-lock-type modifier-table keysym-table))
  (define (pick-keysym keysyms)
   (cond
    ((and num-lock? (keypad-sym? (cadr keysyms)))
     (if (or shift? (and lock? (eq? lock-type 'shift-lock)))
         (car keysyms) (cadr keysyms)))
    ((and (not shift?) (not lock?)) (car keysyms))
    ((and (not shift?) lock? (eq? lock-type 'caps-lock))
     (if (keysym-has-case? (car keysyms) char-lower-case?)
         (to-case (car keysyms) char-upcase) (car keysyms)))
    ((and shift? lock? (eq? lock-type 'caps-lock))
     (if (keysym-has-case? (cadr keysyms) char-upper-case?)
         (to-case (cadr keysyms) char-downcase) (cadr keysyms)))
    ((or shift? (and lock? (eq? lock-type 'shift-lock))) (cadr keysyms))))
  (define (lookup-symbol)
    (define keysyms (hashv-ref keysym-table keycode))
    (let keysym-match ((keysyms keysyms))
      (if (null? keysyms) keycode
          (or (keysym->symbol (pick-keysym keysyms))
              (keysym-match (cdr keysyms))))))
  (define connection-setup (xcb-connection-setup (current-xcb-connection)))
  (cond
   ((= (hash-count (const #t) keysym-table) 0)
    (warn "keysym table is empty"))
   ((or (< keycode (xref connection-setup 'min-keycode))
        (> keycode (xref connection-setup 'max-keycode)))
    (error "keycode out of range" keycode))
   (else (with-modifiers (lookup-symbol) modifiers))))

(define (keysyms->keycodes keysym-table)
  (define keycodes (make-hash-table))
  (define (register-keysym code syms)
    (define (add-code codes) (cons code codes))
    (for-each 
     (lambda (sym)
       (hashv-set! 
        keycodes sym
        (or (and=> (hashv-ref keycodes sym) add-code)
            (list code))))
     (take syms 2)))
  (hash-for-each register-keysym keysym-table)
  keycodes)

(define-public (symbol->key keysym-table sym)
  (define str (symbol->string sym))
  (define keycodes (keysyms->keycodes keysym-table))
  (receive (remainder mods)
      (let process-mods ((str-left str) (mods '()))
        (define match (string-match "([MC][2345]?-)(.+)" str-left))
        (define (add-mod) 
          (process-mods
           (match:substring match 2)
           (cons
            (case (string->symbol (match:substring match 1))
              ((M-) '#{1}#)
              ((M2-) '#{2}#)
              ((M3-) '#{3}#)
              ((M4-) '#{4}#)
              ((M5-) '#{5}#)
              ((C-) 'control)
              (else => (lambda (m) (error "Unrecognized key prefix" m))))
            mods)))
        (if match (add-mod) (values str-left mods)))
    (values (hashv-ref keycodes (symbol->keysym (string->symbol remainder)))
            mods)))

(define (with-modifiers sym modifiers)
  (define modifier-prefixes
    '((mod5 . M5-) (mod4 . M4-)
      (mod3 . M3-) (mod2 . M2-)
      (mod1 . M-)  (control . C-)))
  (define (addmod mod s)
    (if (memq (car mod) modifiers)
        (symbol-append (cdr mod) s) s))
  (fold addmod sym modifier-prefixes))
