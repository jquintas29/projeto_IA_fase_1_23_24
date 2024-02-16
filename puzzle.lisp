;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heurísticas específicos do domínio da aplicação

;;alterar
(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

;;alterar
(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posição: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)


;;; Operadores
(defun linha (indice tabuleiro)
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t (error "Indice inválido"))
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t (error "Indice inválido"))
    )
)

(defun alisa (lista)
    (cond ((null lista) nil)
          ((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
          (t (append (alisa (car lista)) (alisa (cdr lista))))
    )
)

(defun lista-numeros (&optional (n 100))
    (cond ((= n 0) nil)
          (t (cons (- n 1) (lista-numeros (- n 1)))))
)

(defun remover-se(pred lista)
    (cond ((null lista) NIL) 
          ((funcall pred (car lista)) (remover-se pred (cdr lista)))
          (T (cons (car lista) (remover-se pred (cdr lista))))
    )
)

(defun baralhar (lista)
    (cond ((null lista) nil)
        (t (let* ((index (random (length lista)))
                    (numberRandom (nth index lista))
                    (nova-lista (remover-se #'(lambda (x) (= x numberRandom)) lista)))
                 (cons numberRandom (baralhar nova-lista))
            )
        )
    )
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor nil))
    (cond ((and (integerp indice) (>= indice 0) (< indice (length lista))) 
                (let ((nova-lista lista))
                        (setf (nth indice nova-lista) valor)
                    nova-lista))
          (t (error "Indice inválido"))
    )
)


(defun substituir (indice-linha indice-coluna tabuleiro &optional (valor 'nil))
    (let ((novo-tabuleiro tabuleiro) (numero (nth indice-coluna (nth indice-linha tabuleiro))))
        (substituir-posicao indice-coluna (linha indice-linha novo-tabuleiro) valor)
        (cond ((not (eql valor 'nil)) (substituir-simetrico numero tabuleiro))
        (novo-tabuleiro)
    )    
)
)

(defun substituir-simetrico (numero tabuleiro &optional (valor-linha 0))
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)) (simetrico (obter-simetrico numero)))
            (cond 
                ((find simetrico lista-linha) (substituir valor-linha (position simetrico lista-linha) tabuleiro))
                (t (substituir-simetrico numero tabuleiro (+ valor-linha 1)))
            )
        ))
    )
)

(defun obter-simetrico (numero)
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun posicao-cavalo (tabuleiro &optional (valor-linha 0))
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)))
            (cond 
                ((find T lista-linha) (cons valor-linha (cons (position T lista-linha) nil)))
                (t (posicao-cavalo tabuleiro (+ valor-linha 1)))
            )
        ))
    )
)

