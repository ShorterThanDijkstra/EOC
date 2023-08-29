; #lang racket
(define (id [x : Integer]) : Integer x)

(let ([x (read)])
  (let ([y (while (< x 3)
                  (begin (set! x (read))
                         x))])
    (id 42)))

; x = read()
; goto loop
; loop:
; if (x<3) {
;   x = read();
;   x; // removable
;   goto loop;
; }
; y = void;
; y