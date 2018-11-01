(define-struct state (board h g parent))
(define B (list '(1 2 3)  '(4 0 6) '(7 8 5)))
(define B1 (list '(3 6 1)  '(7 4 2) '(0 8 5)))

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

(define (outOfBounds? X Y)
  (cond
    ((or (> 0 X) (> 0 Y) (< 2 X) (< 2 Y)) #F)
    (else #T)))

(define  (tryAllMoves B)
  (cond
    ((moveLeft B (findInBoard 0 B 0 0)) (printBoard (moveLeft B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  
  (cond
    ((moveRigth B (findInBoard 0 B 0 0)) (printBoard (moveRigth B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  
  (cond
    ((moveUp B (findInBoard 0 B 0 0)) (printBoard (moveUp B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline)))
  
  (cond
    ((moveDown B (findInBoard 0 B 0 0)) (printBoard (moveDown B (findInBoard 0 B 0 0))) (newline))
    (else (println #F) (newline))))


(define (moveLeft B emptyTilePos)
  (cond
    ((not (outOfBounds? (add1 (getX emptyTilePos)) (getY emptyTilePos))) #F)
    (else (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (add1 (getX emptyTilePos)) (getY emptyTilePos)))) (add1 (getX emptyTilePos)) (getY emptyTilePos) '0))))

(define (moveRigth B emptyTilePos)
  (cond
    ((not (outOfBounds? (sub1 (getX emptyTilePos)) (getY emptyTilePos))) #F)
    (else (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (sub1 (getX emptyTilePos)) (getY emptyTilePos)))) (sub1 (getX emptyTilePos)) (getY emptyTilePos) '0))))

(define (moveUp B emptyTilePos)
  (cond
    ((not (outOfBounds? (getX emptyTilePos) (sub1 (getY emptyTilePos)))) #F)
    (else (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (getX emptyTilePos) (sub1 (getY emptyTilePos))))) (getX emptyTilePos) (sub1 (getY emptyTilePos)) '0))))

(define (moveDown B emptyTilePos)
  (cond
    ((not (outOfBounds? (getX emptyTilePos) (add1 (getY emptyTilePos)))) #F)
    (else (updateBoard (updateBoard B (getX emptyTilePos) (getY emptyTilePos) (getTileAt B (list (getX emptyTilePos) (add1 (getY emptyTilePos))))) (getX emptyTilePos) (add1 (getY emptyTilePos)) '0))))


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


(define (letPlayeredit L)
  (printBoard (setUpGame L))
  (newline)
  (println '(press w/a/s/d to move tiles and q when done))
  (letPlayeredit2 L (read)))

(define (letPlayeredit2 L input)
  (cond
    ((equal? input 'q) L)
    ((equal? input 'w) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 L (read)))
    ((equal? input 'a) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 L (read)))
    ((equal? input 's) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 L (read)))
    ((equal? input 'd) (println '(press w/a/s/d to move tiles and q when done)) (letPlayeredit2 L (read)))
    (else (println 'wrong...) (letPlayeredit2 L (read)))))

(define  (insertSorted open toInsert)
  (cond
    ((empty? open) (cons toInsert open))
    ()))

(define (A* open closed)
  (cond
    ((empty? open) 'FUCK)
    (else '(not yet))))








