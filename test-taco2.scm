(use gauche.test)
(test-start "taco2")
(use taco2)
(test-module 'taco2)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
;; EOF

