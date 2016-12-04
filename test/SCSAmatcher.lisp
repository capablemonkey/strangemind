(load (merge-pathnames "../lib/SCSAmatcher.lisp" *load-truename*))

(subtest "Testing SCSA matcher"
  (is t
    (2-color-checker-p '(A B A B A B))
    "2-color-checker-p returns T for (A B A B A B)")

  (is nil
    (2-color-checker-p '(A C B A C B))
    "2-color-checker-p returns Nil for (A C B A C B)")

  (is t
      (AB-checker-p '(A A A B B A B))
      "AB-checker-p returns T for (A A A B B A B)")

  (is nil
      (AB-checker-p '(A A C B B A B))
      "AB-checker-p returns nil for (A A C B B A B)")

  (is t
      (2-color-alt-checker-p '(E D E D E D E D))
      "2-color-alt-checker-p returns T for (E D E D E D E D)")

  (is nil
      (2-color-alt-checker-p '(A A B B A B))
      "2-color-alt-checker-p returns nil for (A A B B A B)")



  (is t
      (at-most-once-checker-p '(E C D F G B))
      "at-most-once-checker-p returns T for (E C D F G B)")

  (is nil
      (at-most-once-checker-p '(E C D F E B))
      "at-most-once-checker-p returns nil for (E C D F E B)")
  
  (is t
      (first-last-checker-p '(E C D F G E))
      "first-last-checker-p returns T for (E C D F G E)")

  (is nil
      (first-last-checker-p '(E C D F E B))
      "first-last-checker-p returns nil for (E C D F E B)")



  (subtest "Testing score-less-than-three"
	   (is 0.9
	       (score-less-than-three '(A C D D C A ))
	       "score-less-than-three returns 0.9 for (A C D D C A)")

	   (is 0.9
	       (score-less-than-three '(A C C A))
	       "score-less-than-three returns 0.9 for (A C C A)")

	   (is 0.1
	       (score-less-than-three '(A A))
	       "score-less-than-three returns 0.1 for (A A)")

	   (is 0.1
	       (score-less-than-three '(A D A F D A G C B G C ))
	       "score-less-than-three returns 0.1 for (A D A F D A G C B G C )"))

  (subtest "Testing score-prefer-fewer"
	   (is 0.5
	       (score-prefer-fewer '(E E E E E))
	       "score-prefer-fewer returns 0.5 for (E E E E E)")
	   (is 0.25
	       (score-prefer-fewer '(E E F F F))
	       "score-prefer-fewer returns 0.25 for (E E F F F)")
	   (is 0.13
	       (score-prefer-fewer '(E D A E D))
	       "score-prefer-fewer returns .13 for (E D A E D)")
	   (is 0.08
	       (score-prefer-fewer '(E F A G F))
	       "score-prefer-fewer returns .08 for (E F A G F)")
	   (is 0.03
	       (score-prefer-fewer '(F R I A C))
	       "score-prefer-fewer returns .03 for (F R I A C)")
	   (is 0.01
	       (score-prefer-fewer '(A C F D B E))
	       "score-prefer-fewer returns 0.01 for (A C F D B E)")
	   (is 0
	       (score-prefer-fewer '(A C F D B E G))
	       "score-prefer-fewer return 0 for (A C F D B E G)"))


  (is t
      (mystery-1-checker-p '(D E F D E F D E F))
      "mystery-1-checker-p returns T for (D E F D E F D E F)")

  (is nil
      (mystery-1-checker-p '(D E F A E C D E F))
      "mystery-1-checker-p returns nil for (D E F A E C D E F)")

  (subtest "Testing score-mystery-2"
	   (is 0.45
	       (score-mystery-2 '(E E E E E))
	       "score-mystery-2 returns 0.45 for (E E E E E)")
	   (is 0.39
	       (score-mystery-2 '(E E F F F))
	       "score-mystery-2 returns 0.39 for (E E F F F)")
	   (is 0.04
	       (score-mystery-2 '(E D A E D))
	       "score-mystery-2 returns .04 for (E D A E D)")
	   (is 0.08
	       (score-mystery-2 '(E F A G F))
	       "score-mystery-2 returns .08 for (E F A G F)")
	   (is 0.04
	       (score-mystery-2 '(F R I A C))
	       "score-mystery-2 returns .04 for (F R I A C)")
	   (is 0
	       (score-mystery-2 '(A C F D B E))
	       "score-mystery-2 returns 0 for (A C F D B E)"))
  (is t
      (mystery-3-checker-p '(A A C B A B C C B))
      "mystery-3-checker-p returns T for (A A C B A B C C B)")

  (is t
      (mystery-3-checker-p '(A A C B A B C C))
      "mystery-3-checker-p returns T for (A A C B A B C C)")

  (is t
      (mystery-3-checker-p '(A A C B A B C))
      "mystery-3-checker-p returns T for (A A C B A B C ")
  

  (is nil
      (mystery-3-checker-p '(D E D E D E C))
      "mystery-3-checker-p returns nil for (D E D E D E C)")

  (is t
      (mystery-4-checker-p '(E H E H E H))
      "mystery-4-checker-p returns T for (E H E H E H)")

  (is nil
      (mystery-4-checker-p '(E H E H E E))
      "mystery-4-checker-p returns nil for (E H E H E E)")

  (subtest "Testing score-mystery-5"
	   (is 0.0
	       (score-mystery-5 '(E E E E E))
	       "score-mystery-5 returns 0.0 for (E E E E E)")
	   (is 0.06
	       (score-mystery-5 '(E E F F F))
	       "score-mystery-5 returns 0.06 for (E E F F F)")
	   (is 0.45
	       (score-mystery-5 '(E D A E D))
	       "score-mystery-5 returns 0.45 for (E D A E D)")
	   (is 0.49
	       (score-mystery-5 '(E F A G F))
	       "score-mystery-5 returns 0.49 for (E F A G F)")
	   (is 0
	       (score-mystery-5 '(F R I A C))
	       "score-mystery-5 returns 0 for (F R I A C)")
	   (is 0
	       (score-mystery-5 '(A C F D B E))
	       "score-mystery-5 returns 0 for (A C F D B E)"))

  (subtest "Testing matches-scsa"
	   (is 1
	       (matches-scsa 'two-color '(A B A B A))
	       "matches-scsa 'two-color '(A B A B A) returned 1")

	   (is 0
	       (matches-scsa 'two-color '(A B C A B))	      
	       "matches-scsa 'two-color '(A B C A B) returned 0")

	   (is 1
	       (matches-scsa 'ab-color '(A B A B A))
	       "matches-scsa 'ab-color '(A B A B A) returned 1")

	   (is 0
	       (matches-scsa 'ab-color '(A B C A B))	      
	       "matches-scsa 'ab-color '(A B C A B) returned 0")

	   (is 1
	       (matches-scsa 'two-color-alternating '(C G C G C G C))
	       "matches-scsa 'two-color-alternating '(C G C G C G C) returned 1")

	   (is 0
	       (matches-scsa 'two-color-alternating '(A B C A B))	      
	       "matches-scsa 'two-color-alternating '(A B C A B) returned 0")

	   (is 1
	       (matches-scsa 'only-once '(E D A B C))
	       "matches-scsa 'only-once '(E D A B C) returned 1")

	   (is 0
	       (matches-scsa 'only-once '(F G A D A C))	      
	       "matches-scsa 'only-once '(F G A D A C) returedn 0")

	   (is 1
	       (matches-scsa 'first-and-last '(F A E D A C F))
	       "matches-scsa 'first-and-last '(F A E D A C F) returned 1")

	   (is 0
	       (matches-scsa 'first-and-last '(E A E C F E F))	      
	       "matches-scsa 'first-and-last '(E A E C F E F) returned 0")

	   (is 0.9
	       (matches-scsa 'usually-fewer '(C E D D E E))
	       "matches-scsa 'usually-fewer '(C E D D E E) returned 0.9")

	   (is 0.1
	       (matches-scsa 'usually-fewer '(C E B D E E))	      
	       "matches-scsa 'usually-fewer '(C E B D E E) returned 0.1")

	   (is 0.08
	       (matches-scsa 'prefer-fewer '(F E A D F E))
	       "matches-scsa 'prefer-fewer '(F E A D F E) returned 0.08")

	   (is 0
	       (matches-scsa 'prefer-fewer '(F A D E G A F C B))	      
	       "matches-scsa 'prefer-fewer '(F A D E G A F C B) returned 0")

	   (is 1
	       (matches-scsa 'mystery-1 '(C E D C E D))
	       "matches-scsa 'mystery-1 '(C E D C E D) returned 1")

	   (is 0
	       (matches-scsa 'mystery-1 '(C E B C E D))	      
	       "matches-scsa 'mystery-1 '(C E B C E D) returned 0")

	   (is 0.39
	       (matches-scsa 'mystery-2 '(F F G F G G))
	       "matches-scsa 'mystery-2 '(F F G F G G) returned 0.39")

	   (is 0
	       (matches-scsa 'mystery-2 '(E A B D F G))	      
	       "matches-scsa 'mystery-2 '(E A B D F G) returned 0")

	   (is 1
	       (matches-scsa 'mystery-3 '(A C C D A D C))
	       "matches-scsa 'mystery-3 '(A C C D A D C) returned 1")

	   (is 0
	       (matches-scsa 'mystery-3 '(C C C A D D))	      
	       "matches-scsa 'mystery-3 '(C C C A D D) returned 0")

	   (is 1
	       (matches-scsa 'mystery-4 '(F B F B F B F B))
	       "matches-scsa 'mystery-4 '(F B F B F B F B) returned 1")

	   (is 0
	       (matches-scsa 'mystery-4 '(F A F A F D))	      
	       "matches-scsa 'mystery-4 '(F A F A F D) returned 0")

	   (is 0.49
	       (matches-scsa 'mystery-5 '(C E D B E E))
	       "matches-scsa 'mystery-5 '(C E D B E E) returned 0.49")

	   (is 0.45
	       (matches-scsa 'mystery-5 '(C E C D E E))	      
	       "matches-scsa 'mystery-5 '(C E C D E E) returned 0")))
