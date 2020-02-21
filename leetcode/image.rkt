#lang racket

(require pict
         racket/draw
         file/convertible)

(define (save-file p filename)
  (let ([out (open-output-file filename
                               #:mode 'binary
                               #:exists 'replace)])
    (write-bytes (convert p 'png-bytes)
                 out)
    (close-output-port out)))

(define (rect-text t)
  (cc-superimpose (ghost (rectangle 100 20)) (text t)))
(define (box-text t)
  (frame (cc-superimpose (ghost (rectangle 20 20)) (text t))))

(define part-ak (frame (cc-superimpose (ghost (rectangle 100 20)) (text "a")) #:segment 5))
(define part-il (hc-append 20 (let ([a1 (box-text "a")])
                                (refocus (vc-append a1 (text "i")) a1))
                           (text "...")
                           (box-text "a")))
(define part-lmj (hc-append 10 (let ([a (box-text " ")])
                                 (refocus (vc-append a (text "i+l")) a))
                            (text "...") (box-text " ")
                            (let ([a (box-text "a")])
                              (refocus (vc-append a (text "m")) a))
                            (box-text " ") (text "...")
                            (let ([a (box-text " ")])
                              (refocus (vc-append a (text "j")) a))))

(define-syntax-rule (myvoid stx ...)
  (void))
;; curly brace
(myvoid
 (curly-brace 50 200)
 (curly-brace 10 20))

(define (curly-brace w h)
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen   (send dc get-pen))
        (let ([path (new dc-path%)])
          (send path move-to 0 0)
          (send path curve-to
                (* w 1/2) (* h -0/8)
                (* w 1/2) (* h -4/8)
                w (* h -1/2))
          (send path translate 0 (* h 1/2))
          (send dc draw-path path dx dy))
        (let ([path (new dc-path%)])
          (send path move-to 0 0)
          (send path curve-to
                (* w 1/2) (* h 0/8)
                (* w 1/2) (* h 4/8)
                w (* h 1/2))
          (send path translate 0 (* h 1/2))
          (send dc draw-path path dx dy))
        (send dc set-brush old-brush)
        (send dc set-pen   old-pen))
      w h))


(define whole (panorama
               (hc-append 5
                          (refocus
                           (vc-append (text "k")
                                      (rotate (curly-brace 20 100) (* pi -1/2))
                                      part-ak)
                           part-ak)
                          (refocus (vc-append (text "l")
                                              (rotate (curly-brace
                                                       (pict-height part-il)
                                                       (pict-width part-il)) (* pi -1/2))
                                              part-il)
                                   part-il)
                          part-lmj
                          )))

(myvoid
 (save-file (scale whole 2) "assets/a.png"))
;; save this to pdf
