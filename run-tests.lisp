(ql:quickload "fiveam")
(ql:quickload "cl-peg-yapp")
(interpol:enable-interpol-syntax)
(5am:run! 'cl-peg-yapp:peg-suite)
