;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(display "starwisp.scm")(newline)

(alog "hello from starwisp.scm")

(define (mbutton id title fn)
  (button (make-id id) title 15 fillwrap fn))

(define maths (list "+" "-" "/" "*" "swizzle" "*v" "cross" "dot" "eq?" "<" ">" "mag" "magsq" "normalise" "floor" "sincos" "round"))
(define core (list "" "lambda" "define" "let" "cond" "loop" "set!" "write!" "read" "noise" "ignore"))
(define movement (list "rotate-x" "rotate-y" "rotate-z" "move" "animate"))
(define numbers (list "1" "2" "3" "4" "5"))

(define (maths-node id)
  (draggable
   (make-id (string-append id "in1")) 'vertical wrap (list 255 255 127 127)
   (list
    (spinner (make-id "sin1") maths wrap (lambda (v) '())))
   (lambda () (string-append "maths-" id))))

(define (core-node id)
  (draggable
   (make-id (string-append id "m1")) 'vertical wrap (list 225 127 80 127)
   (list
    (spinner (make-id "sm1") core wrap (lambda (v) '())))
   (lambda () (string-append "core-" id))))

(define (movement-node id)
  (draggable
   (make-id (string-append id "m1")) 'vertical wrap (list 225 127 80 127)
   (list
    (spinner (make-id "sm1") movement wrap (lambda (v) '())))
   (lambda () (string-append "core-" id))))

(define (vector-node id)
  (draggable
   (make-id (string-append id "m1")) 'horizonal wrap (list 225 127 80 127)
   (list
    (text-view (make-id "sm1") "vector" 10 wrap)
    (edit-text (make-id "t1") "0.0" 10 "numeric" wrap (lambda () (list)))
    (edit-text (make-id "t2") "0.0" 10 "numeric" wrap (lambda () (list)))
    (edit-text (make-id "t3") "0.0" 10 "numeric" wrap (lambda () (list)))
    )
   (lambda () (string-append "core-" id))))


(define did 100)
(define (new-id)
  (set! did (+ did 1))
  (number->string did))


(define-fragment-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction


(define-activity-list
  (activity
   "main"
   (horiz
    (vert
    (horiz
     (mbutton "eval" "Parse me"
              (lambda ()
                (list (walk-draggable
                       "eval" 99
                       (lambda (v)
                         (msg v)
                         (list (toast v)))))))
     (mbutton "add-maths" "Maths"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (maths-node (new-id))
                 )))))
     (mbutton "add-core" "Core"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (core-node (new-id))
                 )))))
     (mbutton "add-mov" "Movement"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (movement-node (new-id))
                 )))))
     (mbutton "add-vec" "Vector"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (vector-node (new-id))
                 ))))))
    (draggable
     99 'vertical (layout 'fill-parent 'fill-parent 1 'left 0) (list 255 255 255 127)
      (list
       (vector-node "1")
       )
     (lambda (v) '())))


    (nomadic (make-id "b2x") (layout 'fill-parent 'fill-parent 1 'left 0)
             (lambda ()
               (display "hello from nomadic callback")(newline)
               (clear)



  (set! jelly (build-jellyfish 512))
  (set! jelly2 (build-jellyfish 512))

  (with-primitive
   jelly
   (terrain-setup)
   (jelly-compiled (compile-program 10000 prim-triangles 1 terrain)))

  (define s1 (raw-obj (list-ref spider 0)))
  (define s2 (raw-obj (list-ref spider 1)))
  (define s3 (raw-obj (list-ref spider 2)))

  (msg s1 s2 s3)

  (with-primitive
   jelly2
   (scale (vector 0.2 0.2 0.2))
   (pdata-index-map! (lambda (i p) (with-primitive s2 (pdata-ref "p" i))) "p")
   (pdata-index-map! (lambda (i p) (with-primitive s2 (pdata-ref "p" i))) "t")
   (pdata-index-map! (lambda (i p) (with-primitive s3 (pdata-ref "p" i))) "c")

   (pdata-index-map! (lambda (i n) (with-primitive s1 (pdata-ref "n" i))) "n")
;;   (pdata-index-map! (lambda (i n) (with-primitive s2 (pdata-ref "n" i))) "n2")
;;   (pdata-index-map! (lambda (i n) (with-primitive s3 (pdata-ref "n" i))) "n3")

   (let ((p (compile-program 2000 prim-triangles 1 obj-test)))
     (disassemble p)
     (jelly-compiled p)
     ))


  (destroy s1)
  (destroy s2)
  (destroy s3)

  (every-frame
   (begin
     (with-primitive
      jelly 0
      (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
                                        ;(with-primitive
                                        ; jelly2 0
                                        ; (pdata-set! "x" reg-fling (vector (vx _fling) (vy _fling) 0)))
     ))

              )))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )

(msg "done....")
