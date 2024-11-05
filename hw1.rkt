#lang racket
(require 2htdp/image)
 
(define img (rectangle 1 2 "solid" "blue"))
 
(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color))))
 
(define (img->mat img)
  (define width (image-width img))
  (define lst (map RGB->grayscale (image->color-list img)))
    (create-matrix lst width)
  )
 
(define (create-matrix lst width [part '()] [count 0] [mat '()])
  (if (null? lst)
      (reverse (cons (reverse part) mat))
      (if (= count width)
      (create-matrix lst width '() 0 (cons (reverse part) mat))
      (create-matrix (cdr lst) width (cons (car lst) part) (+ count 1) mat))
      )  
  )
 
(define (ascii-art width height chars)
  (lambda (img) (change-matrix img width height chars)))
  
;returns normal matrix
(define (change-matrix img sub-width sub-height chars)
   
  (define (calc-index matrix-avgs [indices '()])
  (define len (string-length chars))
  (if (null? matrix-avgs)
      (reverse indices)
      (calc-index (cdr matrix-avgs) (cons (list-ref (string->list chars) (inexact->exact (floor (/ (* (- 255 (floor (car matrix-avgs))) len) 256)))) indices))
   )
  )
   
  (define matrix (img->mat img))
  (define width (length(car matrix)))
  (define height (length matrix))
  (define n-h (new-height sub-height height))
  (define n-w (new-width sub-width width))
  (define mat (cut-matrix matrix n-w n-h))
  (define total-sum (sum-of-submatrices mat n-w n-h sub-width sub-height))
  (define my-list (map list->string (map calc-index total-sum)))
  (if (null? my-list)
      ""
      (string-append (string-join my-list "\n") "\n"))
  )
 
;count new width for matrix
(define (new-width sub-width width [new-w 0])
  (if (>= width sub-width)
      (new-width sub-width (- width sub-width) (+ new-w sub-width))
      new-w
  )
  )
;count new height for matrix
(define (new-height sub-height height [new-h 0])
  (if (>= height sub-height)
      (new-height sub-height (- height sub-height) (+ new-h sub-height))
      new-h
  )
  )
;cuts matrix
(define (cut-matrix matrix n-w n-h [count-h 0] [acc '()])
  (if (empty? matrix)
    (reverse acc)
    (let ([part (take (car matrix) n-w)])
      (if (< count-h n-h)
      (cut-matrix (cdr matrix) n-w n-h (+ count-h 1) (cons part acc))
      (reverse acc)
  )))
  )
 
 
(define (sum-of-submatrices matrix n-w n-h sub-width sub-height [sums '()])
  (define (submatrix row col sub-width sub-height [wc 0] [hc 0] [sum 0])
    (if (= hc sub-height)
        sum
        (if (= wc sub-width)
            (submatrix row col sub-width sub-height 0 (+ 1 hc) sum)
            (submatrix row col sub-width sub-height (+ wc 1) hc (+ sum (list-ref (list-ref matrix (+ row hc)) (+ col wc))))
            ))
    )
 
 
  (define (sum-helper i j sums [part '()])
    (if (< i n-h)
        (if (< j n-w)
            (sum-helper i (+ j sub-width) sums (cons (/ (submatrix i j sub-width sub-height) (* sub-width sub-height)) part))
            (sum-helper (+ i sub-height) 0 (cons (reverse part) sums) '()))
        sums))
 
  (reverse (sum-helper 0 0 sums)))
 
(provide img->mat ascii-art)
