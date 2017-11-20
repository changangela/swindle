(cond
	[(and true false) "never gonna happen"]
	[(= 1 1) "ayy it works"])

(define x 1)

(define (f x y) (+ x y))

(f x 4)

(cond
	[(= 4 5) "angela"]
	[(and true false) "wendy"]
	[(= (+ 1 4) (f x 4)) "ARITHMETIC MAGIC"])