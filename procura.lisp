;;; Ficheiro procura.lisp
;; Implementação dos algoritmos de procura BFS, DFS, A*

; Escrever depois num ficheiro a solução encontrada, número de nós gerados 
; número de nós expandidos, penetrância, fator de ramificação média e tempo de execução.

(defun abertos-bfs (lista-abertos lista-sucessores)
    (cond 
        ((null lista-sucessores) lista-abertos)
        ;((no-existep (car lista-sucessores) lista-abertos) (abertos-bfs lista-abertos (cdr lista-sucessores)))
        (t (abertos-bfs (append lista-abertos (cons (car lista-sucessores) nil)) (cdr lista-sucessores)))
    )
)

(defun abertos-dfs (lista-abertos lista-sucessores)
    (cons (car lista-sucessores) (append (cdr lista-sucessores) lista-abertos))
)

(defun no-existep (no lista)
    (cond 
        ((null lista) nil)
        ((equal (no-operadores no) (no-operadores (car lista))) T)
        (t (no-existep no (cdr lista)))
    )
)

;;; Funcoes auxiliares da procura
(defun no-solucaop (no)
    (cond 
        ((>= (no-pontuacao no) (no-solucao no)) T)
        (t nil)
    )
)

(defun tempo-execucao-bfs (algoritmo no funcao-solucao funcao-sucessores lista-operadores)
(format t "tempo de execucao~%")
(escrever-no no)
    (let ((inicio (get-internal-real-time)))
        (list (funcall algoritmo no funcao-solucao funcao-sucessores lista-operadores) (- (get-internal-real-time) inicio))
    )
)

(defun tempo-execucao-dfs (algoritmo no funcao-solucao funcao-sucessores lista-operadores profundidade)
    (let ((inicio (get-internal-real-time)))
        (list (funcall algoritmo no funcao-solucao funcao-sucessores lista-operadores profundidade) (- (get-internal-real-time) inicio))
    )
)

;;; Algoritmos
;; procura na largura (bfs)
(defun bfs (no funcao-solucao funcao-sucessores lista-operadores &optional (abertos '()) (fechados '()))
    (cond 
        ((and (not (null (no-tabuleiro no))) (funcall funcao-solucao no)) (progn (format t "No solução~%") (list no abertos fechados)))
        ((and (not (null (no-tabuleiro no))))
            (let ((novos-abertos (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs ))))
                (bfs (car novos-abertos) funcao-solucao funcao-sucessores lista-operadores (cdr novos-abertos) (append fechados (cons no nil)))
            )
        )
        (t (cond 
            ((null abertos) (progn (format t "Não tem solução~%") nil))
            (t (bfs (car abertos) funcao-solucao funcao-sucessores lista-operadores (cdr abertos) fechados))
        ))
    )
)

;; procura na profundidade (dfs)
(defun dfs (no funcao-solucao funcao-sucessores lista-operadores profundidade &optional (abertos '()) (fechados '()))
    (cond 
        ((and (not (null (no-tabuleiro no))) (<= (no-profundidade no) profundidade) (funcall funcao-solucao no)) (progn (format t "No solução~%") (list no abertos fechados)))
        ((and (not (null (no-tabuleiro no))) (<= (no-profundidade no) profundidade))
            (let ((novos-abertos (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs ))))
                (dfs (car novos-abertos) funcao-solucao funcao-sucessores lista-operadores profundidade (cdr novos-abertos) (append fechados (cons no nil)))
            )
        )
        (t (cond 
            ((null abertos) (progn (format t "Não tem solução~%") nil))
            (t (dfs (car abertos) funcao-solucao funcao-sucessores lista-operadores profundidade (cdr abertos) fechados))
        ))
    )
)
