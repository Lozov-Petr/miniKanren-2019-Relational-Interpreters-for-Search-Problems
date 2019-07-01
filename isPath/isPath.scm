(load "mk/test-check.scm")
(load "evalo-standard.scm")


(define toN (lambda (n) (if (= n 0) 'z `(s ,(toN (- n 1))))))

(time
  (test 'isPath_test
  (run 1 (q)
   (fresh (a0 a1 a2 a3 a4 a5)
    (== q `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5))
    (evalo
      `(letrec ([eq (lambda (a b)
                    (match a
                      [`z      (match b [`z #t] [`(s ,y) #f])]
                      [`(s ,x) (match b [`z #f] [`(s ,y) (eq x y)])]
                    )
                )])
        (letrec ([eq_pair (lambda (a b)
                    (match a
                        [`(,a1 ,a2) (match b [`(,b1 ,b2) (and (eq a1 b1) (eq a2 b2))])]
                    )
                )])

        (letrec ([elem (lambda (e g)
                    (match g
                      [`() #f]
                      [`(,x . ,xs) (if (eq_pair e x) #t (elem e xs))]
                    )
                )])

        (letrec ([is_path (lambda (p g)
                    (match p
                      [`(,x) #t]
                      [`(,x1 ,x2 . ,xs) (and (elem (list x1 x2) g) (is_path (cons x2 xs) g))]
                    )
                )])

         (is_path ' ,q '((,(toN  0) ,(toN  1))
                         (,(toN  1) ,(toN  2))
                         (,(toN  2) ,(toN  3))
                         (,(toN  3) ,(toN  4))
                         (,(toN  4) ,(toN  5))
                         (,(toN  5) ,(toN  6))
                         (,(toN  6) ,(toN  7))
                         (,(toN  7) ,(toN  8))
                         (,(toN  8) ,(toN  9))
                         (,(toN  9) ,(toN 10))
                         (,(toN 10) ,(toN 11))
                         (,(toN 11) ,(toN 12))
                         (,(toN 12) ,(toN 13))
                         (,(toN 13) ,(toN 14))
                         (,(toN 14) ,(toN 15))
                         (,(toN 15) ,(toN 16))
                         (,(toN 16) ,(toN 17))
                         (,(toN 17) ,(toN 18))
                         (,(toN 18) ,(toN 19))
                         (,(toN 19) ,(toN  0))
                        ))

        ))))
      #t)))

    ""))
