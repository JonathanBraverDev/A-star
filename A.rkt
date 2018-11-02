(define-struct state (board h g F parent))
(define B1 (list '(1 2 3)  '(4 5 6) '(7 8 0)))

(define (setUpGame L)
  (list (list (list-ref L 0) (list-ref L 1) (list-ref L 2)) (list (list-ref L 3) (list-ref L 4) (list-ref L 5)) (list (list-ref L 6) (list-ref L 7) (list-ref L 8))))

(define (printBoard B)
  (cond
   ((not (empty? (rest B))) (println (first B)) (printBoard (rest B)))
   ((empty? (rest B)) (println (first B)))))

(define (printState state)
  (printBoard (state-board state))
  (newline)
  (println (state-h state))
  (println (state-g state))
  (print (state-parent state)))


(define (findInBoard target B X Y)
  (cond
    ((equal? (getTileAt B (list X Y)) target) (list X Y))
    ((> Y (sub1 (length B))) #F)
    ((= X (sub1 (length (first B)))) (findInBoard target B 0 (add1 Y)))
    (else (findInBoard target B (add1 X) Y))))

(define (findInList target L)
  (cond
    ((empty? L) #F)
    ((equal? (first L) target) #T)
    (else (findInList target (rest L)))))

(define (outOfBounds? B X Y)
  (cond
    ((or (> 0 X) (> 0 Y) (< (sub1 (length (first B))) X) (< (sub1 (length B)) Y)) #F)
    (else #T)))

(define  (tryAllMoves B) ;for debug
  (println 'start:)
  (printBoard B)
  (println 'L)
  (cond
    ((moveLeft B (findInBoard 0 B 0 0)) (printBoard (moveLeft B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  (println 'R)
  (cond
    ((moveRigth B (findInBoard 0 B 0 0)) (printBoard (moveRigth B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  (println 'U)
  (cond
    ((moveUp B (findInBoard 0 B 0 0)) (printBoard (moveUp B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  (println 'D)
  (cond
    ((moveDown B (findInBoard 0 B 0 0)) (printBoard (moveDown B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline))))


(define (moveLeft W parent emptyTilePos)
  (cond
    ((not (outOfBounds? (state-board parent) (add1 (getX emptyTilePos)) (getY emptyTilePos))) #F)
    (else (createState W (updateBoard (updateBoard (state-board parent) (getX emptyTilePos) (getY emptyTilePos) (getTileAt (state-board parent) (list (add1 (getX emptyTilePos)) (getY emptyTilePos)))) (add1 (getX emptyTilePos)) (getY emptyTilePos) '0) parent))))

(define (moveRigth W parent emptyTilePos)
  (cond
    ((not (outOfBounds? (state-board parent) (sub1 (getX emptyTilePos)) (getY emptyTilePos))) #F)
    (else (createState W (updateBoard (updateBoard (state-board parent) (getX emptyTilePos) (getY emptyTilePos) (getTileAt (state-board parent) (list (sub1 (getX emptyTilePos)) (getY emptyTilePos)))) (sub1 (getX emptyTilePos)) (getY emptyTilePos) '0) parent))))

(define (moveDown W parent emptyTilePos)
  (cond
    ((not (outOfBounds? (state-board parent) (getX emptyTilePos) (sub1 (getY emptyTilePos)))) #F)
    (else (createState W (updateBoard (updateBoard (state-board parent) (getX emptyTilePos) (getY emptyTilePos) (getTileAt (state-board parent) (list (getX emptyTilePos) (sub1 (getY emptyTilePos))))) (getX emptyTilePos) (sub1 (getY emptyTilePos)) '0) parent))))

(define (moveUp W parent emptyTilePos)
  (cond
    ((not (outOfBounds? (state-board parent) (getX emptyTilePos) (add1 (getY emptyTilePos)))) #F)
    (else (createState W (updateBoard (updateBoard (state-board parent) (getX emptyTilePos) (getY emptyTilePos) (getTileAt (state-board parent) (list (getX emptyTilePos) (add1 (getY emptyTilePos))))) (getX emptyTilePos) (add1 (getY emptyTilePos)) '0) parent))))

(define (createState W B parent)
  (make-state B (totalDistance 0 (state-board parent) 0 0) (add1 (state-g parent)) (calcF W (totalDistance (state-board parent) 0 0) (add1 (state-g parent))) parent))

(define (getDestPosFor B pos)
  (cond
    ((equal? (getTileAt B pos) 1) '(0 0))
    ((equal? (getTileAt B pos) 2) '(1 0))
    ((equal? (getTileAt B pos) 3) '(2 0))
    ((equal? (getTileAt B pos) 4) '(0 1))
    ((equal? (getTileAt B pos) 5) '(1 1))
    ((equal? (getTileAt B pos) 6) '(2 1))
    ((equal? (getTileAt B pos) 7) '(0 2))
    ((equal? (getTileAt B pos) 8) '(1 2))
    (else  pos)))


(define (distanceFromTarget B posTile)
  (+ (abs (- (getX posTile) (getX (getDestPosFor B posTile)))) (abs (- (getY posTile) (getY (getDestPosFor B posTile))))))

(define (totalDistance B X Y)
  (cond
    ((= Y (length B)) 0)
    ((= X (sub1 (length (first B)))) (+ (distanceFromTarget B (list X Y)) (totalDistance B 0 (add1 Y))))
    (else (+ (distanceFromTarget B (list X Y)) (totalDistance B (add1 X) Y)))))


(define (updateBoard B Xpos Ypos input)
  (cond
    ((= Ypos 0) (cons (updateCol (first B) Xpos input) (rest B)))
    (else (cons (first B) (updateBoard (rest B) Xpos (sub1 Ypos) input)))))

(define (updateCol L Xpos input)
  (cond
    ((= Xpos 0) (cons input (rest L)))
    (else (cons (first L) (updateCol (rest L) (sub1 Xpos) input)))))

(define (getX input)
  (first input))

(define (getY input)
  (second input))

(define (getTileAt B pos)
  (list-ref (list-ref B (getY pos)) (getX pos)))


(define (letPlayeredit B)
  (printBoard B)
  (newline)
  (println '(press w/a/s/d to move tiles and q when done))
  (letPlayeredit2 B (findInBoard 0 B 0 0) (read)))

(define (letPlayeredit2 B emptyTilePos input)
  (printBoard B)
  (cond
    ((equal? input 'q) B)
    ((and (equal? input 'w) (< (getY emptyTilePos) 2)) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 (moveUp B emptyTilePos) (list (first emptyTilePos) (sub1 (second emptyTilePos))) (read)))
    ((and (equal? input 'a) (< (getY emptyTilePos) 2)) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 (moveLeft B emptyTilePos) (list (sub1 (first emptyTilePos)) (second emptyTilePos)) (read)))
    ((and (equal? input 's) (> (getY emptyTilePos) 0)) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 (moveDown B emptyTilePos) (list (first emptyTilePos) (add1 (second emptyTilePos))) (read)))
    ((and (equal? input 'd) (> (getY emptyTilePos) 0)) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 (moveRigth B emptyTilePos) (list (add1 (first emptyTilePos)) (second emptyTilePos)) (read)))
    (else (println 'wrong...) (letPlayeredit2 B emptyTilePos (read)))))

(define (sortByMinF statesL newOpen) ;WIP
  (cond
    ((> (state-F (first statesL)) (state-F (second statesL))) '1)
    (else 2)))
    

(define (sort closed toSortL newMoves) ;WIP
  (cond
    ((empty? toSortL) (list newMoves closed))
    ((not (first toSortL)) (sort closed (rest toSortL)))
    ((not (findInList (first toSortL))) (sort (cons (first toSortL) closed) (rest toSortL) (cons (first toSortL) newMoves)))
    (else 'ERR-sort)))


(define (A* open closed W) ;WIP
  (cond
    ((empty? open) 'FUCK)
    ((equal? (first open) B1) 'DONE)
    (else (A* (sortByMinF (cons (first (sort closed (list (moveUp W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                          (moveDown W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                          (moveRigth W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                          (moveLeft W (first open) (findInBoard 0 (state-board (first open)) 0 0))) '())) open)  0 W) (second (sort closed (sort closed (list (moveUp W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                                                                                                                                                          (moveDown W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                                                                                                                                                          (moveRigth W (first open) (findInBoard 0 (state-board (first open)) 0 0))
                                                                                                                                                                                          (moveLeft W (first open) (findInBoard 0 (state-board (first open)) 0 0)))) '())) W))))

(define (calcF W h g)
  (+ (* W h) (* (- 1 W) g)))

;(general function: (f=w*h+(1-w)*g))
;(greedy: w=1)
;(A*:w=0.5)
;(lowestCost: w=0)

(define (start)
  (printBoard (letPlayeredit B1)))
