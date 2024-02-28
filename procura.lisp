;;; Ficheiro procura.lisp do Projeto 1
;;; Autor: Jo�o Quintiliano, 201900287
;;; Curso: Licenciatura em Engenharia Inform�tica
;;; UC: Intelig�ncia Artificial
;; Implementa��o dos algoritmos de procura BFS, DFS, A*

(defun abertos-bfs (lista-abertos lista-sucessores)
"Fun��o que concatena os n�s sucessores no fim da lista de abertos"
    (cond 
        ((null lista-sucessores) lista-abertos)
        ;((no-existep (car lista-sucessores) lista-abertos) (abertos-bfs lista-abertos (cdr lista-sucessores)))
        (t (abertos-bfs (append lista-abertos (cons (car lista-sucessores) nil)) (cdr lista-sucessores)))
    )
)

(defun abertos-dfs (lista-abertos lista-sucessores)
"Fun��o que concatena os n�s sucessores no inicio da lista de abertos"
    (cons (car lista-sucessores) (append (cdr lista-sucessores) lista-abertos))
)

(defun no-existep (no lista)
"Verifica se um n� j� existe numa determinada lista de n�s."
    (cond 
        ((null lista) nil)
        ((equal (no-operadores no) (no-operadores (car lista))) T)
        (t (no-existep no (cdr lista)))
    )
)

;;; Funcoes auxiliares da procura
(defun no-solucaop (no)
"Verifica se um n� � solu��o do problema."
    (cond 
        ((>= (no-pontuacao no) (no-solucao no)) T)
        (t nil)
    )
)

(defun tempo-execucao-bfs (algoritmo no funcao-solucao funcao-sucessores lista-operadores)
"Devolve o tempo que o algoritmo bfs leva para encontrar uma solu��o."
    (let ((inicio (get-internal-real-time)))
        (list (funcall algoritmo no funcao-solucao funcao-sucessores lista-operadores) (* (- (get-internal-real-time) inicio) 1000))
    )
)

(defun tempo-execucao-dfs (algoritmo no funcao-solucao funcao-sucessores lista-operadores profundidade)
"Devolve o tempo que o algoritmo dfs leva para encontrar uma solu��o."
    (let ((inicio (get-internal-real-time)))
        (list (funcall algoritmo no funcao-solucao funcao-sucessores lista-operadores profundidade) (* (- (get-internal-real-time) inicio) 1000))
    )
)

;;; Algoritmos
;; procura na largura (bfs)
(defun bfs (no funcao-solucao funcao-sucessores lista-operadores &optional (abertos '()) (fechados '()))
"Algoritmo de procura bfs. Devolve o melhor caminho da �rvore."
    (cond 
        ((and (not (null (no-tabuleiro no))) (funcall funcao-solucao no)) (progn (format t "No solu��o~%") (list no abertos fechados)))
        ((and (not (null (no-tabuleiro no))))
            (let ((novos-abertos (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs ))))
                (bfs (car novos-abertos) funcao-solucao funcao-sucessores lista-operadores (cdr novos-abertos) (append fechados (cons no nil)))
            )
        )
        (t (cond 
            ((null abertos) (progn (format t "N�o tem solu��o~%") (list nil abertos fechados)))
            (t (bfs (car abertos) funcao-solucao funcao-sucessores lista-operadores (cdr abertos) fechados))
        ))
    )
)

;; procura na profundidade (dfs)
(defun dfs (no funcao-solucao funcao-sucessores lista-operadores profundidade &optional (abertos '()) (fechados '()))
"Algoritmo de procura dfs. Devolve o melhor caminho da �rvore."
    (cond 
        ((and (not (null (no-tabuleiro no))) (<= (no-profundidade no) profundidade) (funcall funcao-solucao no)) (progn (format t "No solu��o~%") (list no abertos fechados)))
        ((and (not (null (no-tabuleiro no))) (<= (no-profundidade no) profundidade))
            (let ((novos-abertos (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs ))))
                (dfs (car novos-abertos) funcao-solucao funcao-sucessores lista-operadores profundidade (cdr novos-abertos) (append fechados (cons no nil)))
            )
        )
        (t (cond 
            ((null abertos) (progn (format t "N�o tem solu��o~%") (list nil abertos fechados)))
            (t (dfs (car abertos) funcao-solucao funcao-sucessores lista-operadores profundidade (cdr abertos) fechados))
        ))
    )
)
