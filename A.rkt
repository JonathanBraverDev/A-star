(define-struct F (g h))
(define-struct state (board F parent))
(define B1 (list '(1 2 3)  '(0 5 6) '(4 7 8)))


(define (setUpGame L)
  (list (list (list-ref L 0) (list-ref L 1) (list-ref L 2)) (list (list-ref L 3) (list-ref L 4) (list-ref L 5)) (list (list-ref L 6) (list-ref L 7) (list-ref L 8))))


(define (printBoard B)
  (cond
   ((not (empty? (rest B))) (println (first B)) (printBoard (rest B)))
   ((empty? (rest B)) (println (first B)))))


(define (printState state)
  (printBoard (state-board state))
  (print (F-g (state-F state))) (print '_)
  (println (F-h (state-F state))) (newline))
;  (print (state-parent state)))

(define (printStates L)
  (cond
    ((empty? L) 'done)
    (else (printState (first L)) (printStates (rest L)))))


(define (findInBoard target B X Y)
  (cond
    ((equal? (getTileAt B (list X Y)) target) (list X Y))
    ((> Y (sub1 (length B))) #F)
    ((= X (sub1 (length (first B)))) (findInBoard target B 0 (add1 Y)))
    (else (findInBoard target B (add1 X) Y))))


(define (findInList target L)
  (cond
    ((empty? L) #F)
    ((equal? (state-board (first L)) (state-board target)) #T)
    (else (findInList target (rest L)))))


(define (outOfBounds? B X Y)
  (cond
    ((or (> 0 X) (> 0 Y) (< (sub1 (length (first B))) X) (< (sub1 (length B)) Y)) #T)
    (else #F)))


(define (moveLeft B state emptyTilePos)
  (cond
    ((outOfBounds? B (add1 (getX emptyTilePos)) (getY emptyTilePos)) #F)
    (else (stateBuilder (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (add1 (getX emptyTilePos)) (getY emptyTilePos)))) (add1 (getX emptyTilePos)) (getY emptyTilePos) '0) state))))


(define (moveRigth B state emptyTilePos)
  (cond
    ((outOfBounds? B (sub1 (getX emptyTilePos)) (getY emptyTilePos)) #F)
    (else (stateBuilder (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (sub1 (getX emptyTilePos)) (getY emptyTilePos)))) (sub1 (getX emptyTilePos)) (getY emptyTilePos) '0) state))))


(define (moveDown B state emptyTilePos)
  (cond
    ((outOfBounds? B (getX emptyTilePos) (sub1 (getY emptyTilePos))) #F)
    (else (stateBuilder (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (getX emptyTilePos) (sub1 (getY emptyTilePos))))) (getX emptyTilePos) (sub1 (getY emptyTilePos)) '0) state))))


(define (moveUp B state emptyTilePos)
  (cond
    ((outOfBounds? B (getX emptyTilePos) (add1 (getY emptyTilePos))) #F)
    (else (stateBuilder (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (getX emptyTilePos) (add1 (getY emptyTilePos))))) (getX emptyTilePos) (add1 (getY emptyTilePos)) '0) state))))


(define (stateBuilder B state)
  (make-state B (make-F (add1 (F-g (state-F state))) (totalDistance B 0 0)) state))


(define  (tryAllMoves B) ;for debug
  (println 'start:)
  (printBoard B)
  (println 'L)
  (cond
    ((moveLeft B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)) (printBoard (state-board (moveLeft B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)))) (newline))
    (else (println #F) (newline)))
  (println 'R)
  (cond
    ((moveRigth B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)) (printBoard (state-board (moveRigth B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)))) (newline))
    (else (println #F) (newline)))
  (println 'U)
  (cond
    ((moveUp B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)) (printBoard (state-board (moveUp B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)))) (newline))
    (else (println #F) (newline)))
  (println 'D)
  (cond
    ((moveDown B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)) (printBoard (state-board (moveDown B (make-state B (make-F 0 (totalDistance B 0 0)) 'none) (findInBoard 0 B 0 0)))) (newline))
    (else (println #F) (newline))))


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


(define (sortByMinF statesL index timesSorted W)
  (cond
    ((= index (length statesL)) statesL)
    ((= index (length statesL)) (sortByMinF statesL 0(add1 timesSorted) W))
    ((> (calcF (first statesL) W) (calcF (second statesL) W)) (sortByMinF (replace statesL index (add1 index)) (add1 index) timesSorted W))
    (else (sortByMinF statesL (add1 index) timesSorted W))))


(define (replace L index1 index2)
  (append (append (listUntill L index1 0) (list (list-ref L index2))) (cons (list-ref L index1) (listFrom L index2))))


(define (listUntill L index counter)
  (cond
    ((= index 0) '())
    ((= (sub1 (length L)) index) (rest L))
    ((= counter index) '())
    (else (cons (list-ref L counter) (listUntill L index (add1 counter))))))


(define (listFrom L index)
  (cond 
    ((= index 0) L)
    ((= (length L) index) '())
    (else (cons (list-ref L index) (listFrom L (add1 index))))))


(define (sort2 closed toSortL newMoves) ;WIP
  (cond
    ((empty? toSortL) (list newMoves closed))
    ((not (first toSortL)) (sort2 closed (rest toSortL) newMoves))
    ((not (findInList (first toSortL) closed)) (sort2 (cons (first toSortL) closed) (rest toSortL) (cons (first toSortL) newMoves)))
    (else (sort2 closed (rest toSortL) newMoves))))


(define (A* open closed W) ;WIP
  (print 'open:) (newline)
  (printStates open)
  (newline)
  (print 'closed:) (newline)
  (printStates closed)
  (newline)
  (cond
    ((empty? open) (list '(0 0 0) '(0 0 0) '(0 0 0)))
    ((= (F-h (state-F (first open))) 0) (state-board (first open)))
    (else (A* (sortByMinF (first (cons (first (sort2 closed (list (moveUp (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveDown (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveLeft (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveRigth (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0))) '())) open)) 0 0 W) (second (sort2 closed (list (moveUp (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveDown (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveLeft (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0)) (moveRigth (state-board (first open)) (first open) (findInBoard 0 (state-board (first open)) 0 0))) '())) W))))


(define (calcF state W)
  (+ (* W (F-h (state-F state))) (* (- 1 W) (F-g (state-F state)))))

;(general function: (f=w*h+(1-w)*g))
;(greedy: w=1)
;(A*:w=0.5)
;(lowestCost: w=0)

(define (start)
  (printBoard (A* (list (make-state B1 (make-F 0 (totalDistance B1 0 0)) 'none)) '() 0.5)))
