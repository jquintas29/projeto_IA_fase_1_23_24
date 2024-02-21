;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heur�sticas espec�ficos do dom�nio da aplica��o

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
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


(defun tabuleiro-a ()
"Tabuleiro para o problema A"
  '(
    (02 20 44 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 30 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 22 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-b ()
"Tabuleiro para o problema B"
  '(
    (02 nil 04 nil 06 nil 08 nil 10 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 nil 05 nil 07 nil 09 nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-c ()
"Tabuleiro para o problema c"
  '(
    (01 12 03 23 nil 88 nil nil nil nil) 
    (21 45 43 nil nil nil nil nil nil nil) 
    (nil 56 nil 78 nil nil nil nil nil nil) 
    (89 nil 99 54 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-d ()
"Tabuleiro para o problema d"
  '(
    (98 97 96 95 94 93 92 91 90 89) 
    (01 02 03 04 05 55 06 07 08 09) 
    (nil 66 nil nil nil nil nil nil nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil 22 nil nil nil nil nil 33 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 88 nil nil nil 44 nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil 77 nil nil nil nil nil) 
    (nil nil nil nil nil nil 99 nil nil nil)
    )
)

(defun tabuleiro-e ()
"Tabuleiro para o problema e"
  '(
    (nil 05 nil nil nil 15 nil nil nil 25) 
    (nil nil nil 06 nil nil nil 16 nil nil) 
    (nil 04 nil nil nil 14 nil nil nil 24) 
    (nil nil nil 07 nil nil nil 17 nil nil) 
    (nil 03 nil nil nil 13 nil nil nil 23) 
    (nil nil nil 08 nil nil nil 18 nil nil) 
    (nil 02 nil nil nil 12 nil nil nil 22) 
    (nil nil nil 09 nil nil nil 19 nil nil) 
    (nil 01 nil nil nil 11 nil nil nil 21) 
    (nil nil nil 10 nil nil nil 20 nil nil)
    )
)

;;; M�todos seletores
(defun linha (indice tabuleiro)
"Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa essa linha do
tabuleiro."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t nil)
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
"Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do
tabuleiro."
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t nil)
    )
)

; n�o parece estar aqui a fazer nada
(defun alisa (lista)
"Devolve todos os elementos de uma lista que poder� conter sub-listas, 
com todos os elementos agregados numa �nica lista principal."
    (cond ((null lista) nil)
          ((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
          (t (append (alisa (car lista)) (alisa (cdr lista))))
    )
)

(defun lista-numeros (&optional (n 100))
"Fun��o que recebe um n�mero positivo n e cria uma lista com todos os n�meros
entre 0 (inclusiv�) e o n�mero passado como argumento (exclusiv�). Por default o n � 100"
    (cond ((= n 0) nil)
          (t (cons (- n 1) (lista-numeros (- n 1)))))
)

(defun remover-se(pred lista)
"Devolve a resconstru��o da lista passada como argumento sem os elementos que verificam a
condi��o/predicado passado pelo argumento pred 
exemplo: (remover-se #'(lambda (x) (= x 0)) lista) -> devolve a lista sem os �tomos 0"
    (cond ((null lista) NIL) 
          ((funcall pred (car lista)) (remover-se pred (cdr lista)))
          (T (cons (car lista) (remover-se pred (cdr lista))))
    )
)

(defun baralhar (lista)
"Recebe uma lista e ir� mudar aleatoriamente os n�meros de forma a criar uma lista baralhada."
    (cond ((null lista) nil)
        (t (let ((numberRandom (nth (random (length lista)) lista)))
                (cons numberRandom (baralhar (remover-se #'(lambda (x) (= x numberRandom)) lista)))
            )
        )
    )
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
"Recebe uma lista (por default ser� chamada a fun��o baralhar com argumento a fun��o lista-numeros)
e vai devolver essa lista dividida em sublistas de n elementos recebido como par�metro pelo argumento n
(por default ter� o valor 10)"
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir (indice-linha indice-coluna tabuleiro &optional (novo-numero 'nil))
  "Substitui o n�mero em uma posi��o espec�fica do tabuleiro."
    (cond 
        ((not (equal novo-numero 'nil))
            (cond 
                ((isduplo (celula indice-linha indice-coluna tabuleiro))
                    (cond 
                        ((maior-duplo (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero))) 
                            (substituir-duplo (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero))))
                        (t (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero)))
                    )
                )
              (t (substituir-simetrico (celula indice-linha indice-coluna tabuleiro)  
                    (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero))))
            )
        )
        (t (substituir-resto tabuleiro indice-linha (substituir-linha (nth indice-linha tabuleiro) indice-coluna novo-numero)))
    )
)

(defun substituir-linha (linha indice-coluna novo-numero)
"Substitui o n�mero em uma posi��o espec�fica da linha."
    (cond
        ((null linha) nil)
        ((= indice-coluna 0) (cons novo-numero (cdr linha)))
        (t (cons (car linha) (substituir-linha (cdr linha) (1- indice-coluna) novo-numero)))
    )
)

(defun substituir-resto (tabuleiro indice-linha nova-linha)
"Retorna o resto do tabuleiro sem modificar a linha especificada."
    (cond
        ((null tabuleiro) nil)
        ((= indice-linha 0) (cons nova-linha (cdr tabuleiro)))
        (t (cons (car tabuleiro) (substituir-resto (cdr tabuleiro) (1- indice-linha) nova-linha)))
    )
)


(defun substituir-simetrico (numero tabuleiro)
"Fun��o que recebe um n�mero, um tabuleiro. A fun��o vai retornar o tabuleiro 
com o sim�trico do n�mero da vari�vel numero substituido por NIL"
    (cond 
        ((null (procurar-posicao tabuleiro (obter-simetrico numero))) tabuleiro)
        (t (substituir (nth 0 (procurar-posicao tabuleiro (obter-simetrico numero))) (nth 1 (procurar-posicao tabuleiro (obter-simetrico numero))) tabuleiro))
    )
)

(defun obter-simetrico (numero)
"Fun��o que recebe um n�mero e devolve o sim�trico desse n�mero (exemplo: 56->65)"
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun substituir-duplo (tabuleiro)
    (cond 
        ((null (maior-duplo tabuleiro)) nil)
        (t (substituir (nth 0 (procurar-posicao tabuleiro (maior-duplo tabuleiro))) (nth 1 (procurar-posicao tabuleiro (maior-duplo tabuleiro))) tabuleiro))
    )
)

(defun maior-duplo (tabuleiro)
"Fun��o que recebe um tabuleiro e retorna o duplo maior desse tabuleiro"
    (cond 
        ((null (remove-if-not #'isduplo (alisa tabuleiro))) nil) 
        (t (apply #'max (remove-if-not #'isduplo (alisa tabuleiro))))
    )
)

(defun isduplo (numero)
"Recebe um n�mero e retorna verdadeiro (T) se o n�mero for um duplo, retorna nil caso contr�rio"
    (cond 
        ((null numero) nil)
        ((not (integerp numero)) nil)
        ((= (mod numero 11) 0) T)
        (t nil)
    )
)

(defun procurar-posicao (tabuleiro valor &optional (valor-linha 0))
"Fun��o que recebe o tabuleiro e devolve a posi��o (i j) em que se encontra o
valor passado por argumento. Pode ser usado para procurar o cavalo.
Caso o valor n�o se encontre no tabuleiro retorna NIL."
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)))
            (cond 
                ((find valor lista-linha) (cons valor-linha (cons (position valor lista-linha) nil)))
                (t (procurar-posicao tabuleiro valor (+ valor-linha 1)))
            )
        ))
    )
)

(defun print-tabuleiro (tabuleiro)
"Recebe um tabuleiro e imprime o tabuleiro no ecr� de forma formatada"
    (cond 
        ((null tabuleiro) nil)
        (t (progn 
            (format t "~d~%" (car tabuleiro))
            (print-tabuleiro (cdr tabuleiro))
           )
        )
    )
)

(defun operadores ()
 "Cria uma lista com todos os operadores do problema."
 (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))


;;; Operadores do problema (ver lab7)
;; operador-1
;usar fun��o substituir() para substituir pelo valor, e depois testar se � sim�trico 
;ou duplo e aplicar a regra
(defun operador (tabuleiro nova-pos-linha nova-pos-coluna)
"Fun��o que recebe um tabuleiro. Realiza um movimento do cavalo 2 casas para baixo e 1 para a esquerda.
Devolve o tabuleiro com a nova posi��o do cavalo."
    (cond ((or (null nova-pos-linha) (null nova-pos-coluna)) nil)
        (t (cond 
            ((or (>= nova-pos-linha (length (car tabuleiro))) (>= nova-pos-coluna (length tabuleiro)) (< nova-pos-linha 0) (< nova-pos-coluna 0)) 
                nil)
            ((equal (celula nova-pos-linha nova-pos-coluna tabuleiro) 'nil) nil)
            (t (cond
                    ((null (substituir (nth 0 (procurar-posicao tabuleiro 'T)) (nth 1 (procurar-posicao tabuleiro 'T)) (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T))) nil)
                    (t (substituir (nth 0 (procurar-posicao tabuleiro 'T)) (nth 1 (procurar-posicao tabuleiro 'T)) (substituir nova-pos-linha nova-pos-coluna tabuleiro 'T)))
                )
            )
        ))
    )  
)

(defun operador-1 (tabuleiro)
;(print-tabuleiro tabuleiro)
;(format t "operador-1: ~d~%"  (procurar-posicao tabuleiro 'T))

    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (+ (nth 0 (procurar-posicao tabuleiro 'T)) 2) (- (nth 1  (procurar-posicao tabuleiro 'T)) 1)))
    )
)

(defun operador-2 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (+ (nth 0 (procurar-posicao tabuleiro 'T)) 2) (+ (nth 1  (procurar-posicao tabuleiro 'T)) 1)))
    )
)

(defun operador-3 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (+ (nth 0 (procurar-posicao tabuleiro 'T)) 1) (+ (nth 1  (procurar-posicao tabuleiro 'T)) 2)))
    )
)

(defun operador-4 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (- (nth 0 (procurar-posicao tabuleiro 'T)) 1) (+ (nth 1  (procurar-posicao tabuleiro 'T)) 2)))
    )
)

(defun operador-5 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (- (nth 0 (procurar-posicao tabuleiro 'T)) 2) (+ (nth 1  (procurar-posicao tabuleiro 'T)) 1)))
    )
)

(defun operador-6 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (- (nth 0 (procurar-posicao tabuleiro 'T)) 2) (- (nth 1  (procurar-posicao tabuleiro 'T)) 1)))
    )
)

(defun operador-7 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (- (nth 0 (procurar-posicao tabuleiro 'T)) 1) (- (nth 1  (procurar-posicao tabuleiro 'T)) 2)))
    )
)

(defun operador-8 (tabuleiro)
    (cond 
        ((null (procurar-posicao tabuleiro 'T)) nil)
        (t (operador tabuleiro (+ (nth 0 (procurar-posicao tabuleiro 'T)) 1) (- (nth 1  (procurar-posicao tabuleiro 'T)) 2)))
    )
)

; no do problema
; - tabuleiro
; - profundidade do n� na arvore
; - estado do problema, n�mero de pontos j� feitos (?) 
; - operadores
 
;;; Construtor
(defun cria-no (tabuleiro profundidade pontuacao lista-operadores solucao)
  (list tabuleiro profundidade pontuacao lista-operadores solucao)
)


(defun escrever-no (no)
 "Permite escrever um no, por defeito no ecra."
    (format t "Tabuleiro ~%")
    (print-tabuleiro (no-tabuleiro no))
    (format t " | Profundidade: ~a~% | Pontos: ~a~% | Operadores: ~a~% | Objetivo: ~a~%-----------------------" 
            (no-profundidade no) (no-pontuacao no) (no-operadores no) (no-solucao no))
    (format t "------------------------------~%")
)

(defun escrever-nos (lista)
"Permite escrever no ecra um no do problema."
    (cond 
        ((null lista) nil)
        (t (progn
            (format t "Tabuleiro ~%")
            (print-tabuleiro (no-tabuleiro (car lista)))
            (format t "| Profundidade: ~a~% | Pontos: ~a~% | Operadores: ~a~% | Objetivo: ~a~%-----------------------" 
                    (no-profundidade (car lista)) (no-pontuacao (car lista)) (no-operadores (car lista)) (no-solucao (car lista)))
            (format t "------------------------------~%")
            (escrever-nos (cdr lista))
            )
        )
    )
)


;;;; Metodos seletores
(defun no-tabuleiro (no)
   (nth 0 no) 
)

(defun no-profundidade (no)
    (nth 1 no)
)

(defun no-pontuacao (no)
    (nth 2 no)
)

(defun no-operadores (no)
    (nth 3 no)    
)

(defun no-solucao (no)
    (nth 4 no)
)


(defun novo-sucessor (no operador)
    (let* ((tabuleiro (funcall operador (no-tabuleiro no)))
          (valor-celula (celula (nth 0 (procurar-posicao tabuleiro 'T)) (nth 1 (procurar-posicao tabuleiro 'T)) (no-tabuleiro no))))
;(format t "novo-sucessor tabuleiro, operador: ~d~%" operador)
;(print-tabuleiro tabuleiro)
;(format t "------------------------~%")
        (cond 
            ((null valor-celula) (cria-no tabuleiro (+ (no-profundidade no) 1) (no-pontuacao no) (alisa (list (no-operadores no) operador)) (no-solucao no)))
            (t (cria-no tabuleiro (+ (no-profundidade no) 1) (+ (no-pontuacao no) valor-celula) (alisa (list (no-operadores no) operador)) (no-solucao no)))
        )   
    )
)

(defun sucessores (no lista-operadores algoritmo &optional (profundidade 9999))
;(format t "sucessores operadores: ~d~%" lista-operadores)
;(escrever-no no)
    (cond 
        ((and (equal algoritmo 'dfs) (= profundidade (no-profundidade no))) no)
        ((null lista-operadores) nil)
        (t (cons (novo-sucessor no (car lista-operadores)) (sucessores no (cdr lista-operadores) algoritmo)))
    )
)
