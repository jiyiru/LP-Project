% Ines Ye Ji, ist199238

:-[codigo_comum].

% ------------------------------------------------------------------------------
% -----------------------Estrutura ilha-----------------------------------------
% Construtor:
faz_ilha(N, (Linha, Coluna), ilha(N, (Linha, Coluna))).

% Seletores:
linha_de(ilha(_,(Linha,_)), Linha).
coluna_de(ilha(_,(_,Coluna)), Coluna).
n_de(ilha(N, (_,_)), N).

% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas) 
% Ilhas eh a lista ordenada (ilhas da esquerda para a direita) cujos os 
% elementos sao as ilhas da linha Linha
% ------------------------------------------------------------------------------
% entra no predicado auxiliar extrai_ilhas_linha/5
extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    extrai_ilhas_linha(N_L, Linha, [], 1, Ilhas).

% extrai_ilha_linha/5
% condicao de paragem: chegar ao final da lista Linha
extrai_ilhas_linha(_, [], Ilhas, _, Ilhas).

extrai_ilhas_linha(N_L, [0|Linha], Ilhas_aux, N_C, Ilhas) :-
    !, NovoN_C is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Ilhas_aux, NovoN_C, Ilhas).

extrai_ilhas_linha(N_L, [E|Linha], Ilhas_aux, N_C, Ilhas) :-
    E > 0,
    faz_ilha(E, (N_L, N_C), Ilha),
    append(Ilhas_aux, [Ilha], Novas_Ilhas),
    NovoN_C is N_C+1,
    extrai_ilhas_linha(N_L, Linha, Novas_Ilhas, NovoN_C, Ilhas).

% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% Ilhas eh a lista ordenada (ilhas da esquerda para a direita e de cima para 
% baixo) cujos elementos sao as ilhas de Puz
% ------------------------------------------------------------------------------
% entra no predicado auxiliar ilhas/4
ilhas(Puz, Ilhas) :-
    ilhas(Puz, 1, [], Ilhas).

% ilhas/4
% condicao de paragem: chegar ao final da lista Puz
ilhas([], _, Ilhas, Ilhas).

ilhas([L|Puz], N_L, Ilhas_aux, Ilhas) :-
    extrai_ilhas_linha(N_L, L, Ilhas_Linha),
    append(Ilhas_aux, Ilhas_Linha, Novas_Ilhas),
    NovoN_L is N_L+1,
    ilhas(Puz, NovoN_L, Novas_Ilhas, Ilhas).

% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% Vizinhas eh a lista ordenada (ilhas de cima para baixo e da esquerda 
% para a direita) cujos elementos sao as ilhas vizinhas de Ilha
%
% vizinhas_aux(Index_C, Index_L, Lista_C, Lista_L, Vizinhas)
% Auxiliar que procura as ilhas vizinhas a partir das listas cujos os elementos 
% sao ilhas que pertencem a colunas/linhas iguais a Ilha
% ------------------------------------------------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas) :-
    linha_de(Ilha, Linha),
    coluna_de(Ilha, Coluna),
    findall(X, (member(X, Ilhas), linha_de(X, Linha)), Linha_igual),
    findall(X, (member(X, Ilhas), coluna_de(X, Coluna)), Coluna_igual),
    nth1(Index_C, Linha_igual, Ilha),
    nth1(Index_L, Coluna_igual, Ilha),
    vizinhas_aux(Index_C, Index_L, Coluna_igual, Linha_igual, Vizinhas).

vizinhas_aux(Index_C, Index_L, Lista_C, Lista_L, Vizinhas) :-
    Index_C1 is Index_C-1,
    Index_C2 is Index_C+1,
    Index_L1 is Index_L-1,
    Index_L2 is Index_L+1,
    findall(X, (member(X, Lista_C), nth1(Index_L1, Lista_C, X)), Result1),
    findall(X, (member(X, Lista_L), nth1(Index_C1, Lista_L, X)), Result2),
    findall(X, (member(X, Lista_L), nth1(Index_C2, Lista_L, X)), Result3),
    findall(X, (member(X, Lista_C), nth1(Index_L2, Lista_C, X)), Result4),
    append(Result1, Result2, Result_aux1),
    append(Result3, Result4, Result_aux2),
    append(Result_aux1, Result_aux2, Vizinhas).

% ------------------------------------------------------------------------------
% estado(Ilhas, Estado)
% Estado eh a lista ordenada cujos elementos sao as entradas referentes a cada 
% uma das ilhas de Ilhas.
%
% Auxiliar: cria_entrada(Ilhas, Ilha, Entrada)
% cria entradas no estado inicial, sendo Entrada uma lista em que o primeiro
% elemento eh uma Ilha, o segundo uma lista de Vizinhas dessa Ilha,
% e o terceiro, uma lista de pontes da Ilha que eh vazia no estado inicial
% ------------------------------------------------------------------------------
estado(Ilhas, Estado) :-
    maplist(criar_entrada(Ilhas), Ilhas, Estado).

criar_entrada(Ilhas, Ilha, Entrada) :-
    vizinhas(Ilhas, Ilha, Vizinhas),
    append([Ilha], [Vizinhas], Aux),
    append(Aux, [[]], Entrada).

% ------------------------------------------------------------------------------
% Auxiliar: menor_pos(Pos1, Pos2)
% verifica se as posicoes pertencem ou a mesma linha ou a mesma coluna
% e retorna true se o numero da linha/coluna da Pos1 for menor que a da Pos2
% ------------------------------------------------------------------------------
menor_pos((X1, Y1), (X2, Y2)) :-
    X1 =:= X2,
    Y1 < Y2.
menor_pos((X1, Y1), (X2, Y2)) :-
    Y1 =:= Y2,
    X1 < X2.

% ------------------------------------------------------------------------------
% Auxiliar: linha_pos_igual(Pos1, Pos2)
% verifica se as posicoes Pos1 e Pos2 pertencem a mesma linha mas a diferente 
% colunas
% ------------------------------------------------------------------------------
linha_pos_igual((X1, Y1), (X2, Y2)) :-
    X1 =:= X2,
    Y1 =\= Y2.

% ------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e
% Pos2), recorrendo ao posicoes_entre_aux. Se Pos1 e Pos2 nao pertencerem a 
% mesma linha ou a mesma coluna, o resultado eh false.
%
% posicoes_entre_aux(Pos1, Pos2, Posicoes)
% recorre a um processo iterativo atraves do posicoes_entre_aux/7 que apos 
% averiguar a linha ou a coluna igual entre as posicoes, vai adicionando 
% ao valor da linha/coluna diferente ate chegar ao seu maior valor
% ------------------------------------------------------------------------------
posicoes_entre(Pos1, Pos2, Posicoes) :-
    menor_pos(Pos1, Pos2),
    !, posicoes_entre_aux(Pos1, Pos2, Posicoes).

posicoes_entre(Pos1, Pos2, Posicoes) :-
    posicoes_entre_aux(Pos2, Pos1, Posicoes).

% posicoes_entre_aux/3
posicoes_entre_aux(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1,Y1),
    Pos2 = (X2,Y2),
    X1 =:= X2,
    Y1 =:= Y2, 
    posicoes_entre_aux(Pos1, Pos2, X1, Y1, Y2, [], Posicoes).

posicoes_entre_aux(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1,Y1),
    Pos2 = (X2,Y2),
    X1 =:= X2,
    Cont is Y1 + 1,
    posicoes_entre_aux(Pos1, Pos2, X1, Cont, Y2, [], Posicoes).

posicoes_entre_aux(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1,Y1),
    Pos2 = (X2,Y2),
    Y1 =:= Y2,
    Cont is X1 + 1,
    posicoes_entre_aux(Pos1, Pos2, Y1, Cont, X2, [], Posicoes).

% posicoes_entre_aux/7
% condicao de paragem: quando o contador eh igual ao maior valor da 
% linha/coluna diferente
posicoes_entre_aux(_, _, _, Cont, Cont, Pos, Pos) :- !.

% quando ambas as posicoes pertencem a mesma linha
posicoes_entre_aux(Pos1, Pos2, El, Cont, Maior_valor, Pos_aux, Posicoes) :-
    linha_pos_igual(Pos1, Pos2), !,
    Pos = [(El,Cont)],
    Novo_C is Cont + 1,
    append(Pos_aux, Pos, Nova_Pos),
    posicoes_entre_aux(Pos1, Pos2, El, Novo_C, Maior_valor, Nova_Pos, Posicoes).

% quando ambas as posicoes pertence a mesma coluna
posicoes_entre_aux(Pos1, Pos2, El, Cont, Maior_valor, Pos_aux, Posicoes) :-
    Pos = [(Cont,El)],
    Novo_C is Cont + 1,
    append(Pos_aux, Pos, Nova_Pos),
    posicoes_entre_aux(Pos1, Pos2, El, Novo_C, Maior_valor, Nova_Pos, Posicoes).

% ------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% Ponte eh uma ponte entre a Pos1 e a Pos2
% ------------------------------------------------------------------------------
cria_ponte(Pos1, Pos2, Ponte) :-
    menor_pos(Pos1, Pos2), !,
    Ponte = ponte(Pos1, Pos2).

cria_ponte(Pos1, Pos2, Ponte) :-
    Ponte = ponte(Pos2, Pos1).

% ------------------------------------------------------------------------------
% Auxiliar: posicao_igual(Pos1, Pos2, P1, P2)
% compara posicoes, devolvendo true se forem iguais
% ------------------------------------------------------------------------------
posicao_igual(Pos1, Pos2, P1, P2) :-
    Pos1 == P1,
    Pos2 == P2,
    !.
posicao_igual(Pos1, Pos2, P1, P2) :-
    Pos1 == P2,
    Pos2 == P1.

% ------------------------------------------------------------------------------
% Auxiliar: posicao_ilha(Ilha, Pos)
% Pos eh a posicao em que a Ilha se encontra
% ------------------------------------------------------------------------------
posicao_ilha(Ilha, Pos) :-
    linha_de(Ilha, L),
    coluna_de(Ilha, C),
    Pos = (L,C).

% ------------------------------------------------------------------------------
% Auxiliar: membro_igual(Elem, Lista)
% semelhante ao predicado member mas nao unifica, devolve true se Elem eh um 
% membro da Lista por comparacao ==
% ------------------------------------------------------------------------------
membro_igual(X, [P|_]) :-
    X == P.
membro_igual(X, [_|R]) :-
    membro_igual(X, R).

% ------------------------------------------------------------------------------
% Auxiliar: elementos_comuns(Lista1, Lista2)
% compara os elementos entre as duas listas e devolve true se existir pelo 
% menos um elemento comum entre as duas
% ------------------------------------------------------------------------------
elementos_comuns([P|_], L) :-
    membro_igual(P, L).
elementos_comuns([_|R], L) :-
    elementos_comuns(R, L).

% ------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% devolve true se a adicao da ponte(Pos1, Pos2) nao faz com que I e Vz deixem
% de ser vizinhas
% ------------------------------------------------------------------------------
% caso as duas posicoes sejam das ilhas vizinhas
caminho_livre(Pos1, Pos2, _, I, Vz) :-
    posicao_ilha(I, PosI),
    posicao_ilha(Vz, PosVz),
    posicao_igual(Pos1, Pos2, PosI, PosVz), !.

caminho_livre(_, _, Posicoes, I, Vz) :-
    posicao_ilha(I, PosI),
    posicao_ilha(Vz, PosVz),
    posicoes_entre(PosI, PosVz, Posicoes2),
    \+elementos_comuns(Posicoes, Posicoes2), !.

% ------------------------------------------------------------------------------
% atualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% Nova_Entrada eh igual a Entrada, excepto no que diz respeito a lista de ilhas 
% vizinhas; esta deve ser actualizada, removendo as ilhas que deixaram de ser 
% vizinhas, apos a adicao da ponte
% ------------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) :-
    Entrada = [Ilha, Vizinhas, Pontes],
    findall(X, (member(X, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha,
    X)), Lista),
    length(Vizinhas, Length),
    \+length(Lista, Length), !,
    Nova_Entrada = [Ilha, Lista, Pontes].

actualiza_vizinhas_entrada(_, _, _, Entrada, Nova_Entrada) :-
    Nova_Entrada = Entrada.

% ------------------------------------------------------------------------------
% atualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% Novo_estado eh o estado que se obtem de Estado apos a actualizacao das ilhas 
% vizinhas de cada uma das suas entradas
% ------------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).
    
% ------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% Ilhas_term eh a lista de ilhas que ja tem todas as pontes associadas,
% designadas por ilhas terminadas. Se a entrada referente a uma ilha for 
% [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes 
% for diferente de 'X' e o comprimento da lista Pontes for N_pontes
% ------------------------------------------------------------------------------
% entra no auxiliar ilhas_terminadas/3
ilhas_terminadas(Estado, Ilhas_term) :-
    ilhas_terminadas(Estado, [], Ilhas_term).

% ilhas_terminadas/3
% condicao de paragem: quando a lista Estado estiver vazia
ilhas_terminadas([], Ilhas_term, Ilhas_term).

% eh uma ilha terminada
ilhas_terminadas([P|Estado], Aux, Ilhas_term) :-
    P = [Ilha, _, Pontes],
    n_de(Ilha, N),
    N \== 'X',
    length(Pontes, L),
    N =:= L,
    append(Aux, [Ilha], Novo_aux), !, 
    ilhas_terminadas(Estado, Novo_aux, Ilhas_term).

% se a ilha tiver como numero de pontes X, nao eh terminada
ilhas_terminadas([P|Estado], Aux, Ilhas_term) :-
    P = [Ilha, _, _],
    n_de(Ilha, N),
    N == 'X', 
    ilhas_terminadas(Estado, Aux, Ilhas_term).

% se a ilha tiver o numero de pontes diferente do numero de elementos na lista 
% Pontes, nao eh terminada
ilhas_terminadas([P|Estado], Aux, Ilhas_term) :-
    P = [Ilha, _, Pontes],
    n_de(Ilha, N),
    length(Pontes, L),
    N =\= L, 
    ilhas_terminadas(Estado, Aux, Ilhas_term).

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Nova_entrada eh a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada
% ------------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) :-
    Entrada = [Ilha, Vizinhas, Pontes],
    findall(X, (member(X,Vizinhas), \+member(X,Ilhas_term)), List),
    Nova_entrada = [Ilha, List, Pontes].

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Novo_estado eh o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado
% ------------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Nova_entrada eh a entrada obtida de Entrada da seguinte forma: se a ilha de 
% Entrada pertencer a Ilhas_term, o numero de pontes desta eh substituido 
% por 'X'; caso contrario Nova_entrada eh igual a Entrada
% ------------------------------------------------------------------------------
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) :-
    Entrada = [Ilha, Vizinhas, Pontes],
    membro_igual(Ilha, Ilhas_term), !, 
    Ilha = ilha(_,(L,C)),
    Nova_Ilha = ilha('X', (L,C)),
    Nova_entrada = [Nova_Ilha, Vizinhas, Pontes].

marca_ilhas_terminadas_entrada(_, Entrada, Nova_entrada) :-
    Nova_entrada = Entrada.

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Novo_estado eh o estado resultante de aplicar o predicado
% marca_ilhas_terminadas_entrada a cada uma das entradas de Estado
% ------------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

% ------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% Novo_estado eh o estado resultante de aplicar os predicados 
% tira_ilhas_terminadas e marca_ilhas_terminadas ao Estado
% ------------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, N_estado),
    marca_ilhas_terminadas(N_estado, Ilhas_term, Novo_estado).

% ------------------------------------------------------------------------------
% Auxiliar: ilhas_iguais(Ilha, Ilha1, Ilha2)
% verifica se alguma das ilhas Ilha1 ou Ilha2 eh igual a ilha Ilha
% ------------------------------------------------------------------------------
ilhas_iguais(Ilha, Ilha1, _) :-
    Ilha == Ilha1.
ilhas_iguais(Ilha, _, Ilha2) :-
    Ilha == Ilha2.

% ------------------------------------------------------------------------------
% Auxiliar: adiciona_num_pontes(Entrada, Pos1, Pos2, N, Num_pontes, 
% Nova_entrada)
% Nova_entrada eh a entrada resultante de adicionar Num_pontes pontes entre
% Pos1 e Pos2 a Entrada
% ------------------------------------------------------------------------------
% condicao de paragem: quando o contador for igual ao Num_Pontes
adiciona_num_pontes(Entrada, _, _, N, N, Entrada) :- !.

adiciona_num_pontes(Entrada, Pos1, Pos2, N, Num_pontes, Nova_entrada) :-
    cria_ponte(Pos1, Pos2, Ponte),
    Entrada = [Ilha, Vizinhas, Pontes],
    append(Pontes, [Ponte], Novas_pontes),
    N_entrada = [Ilha, Vizinhas, Novas_pontes],
    N_n is N + 1,
    adiciona_num_pontes(N_entrada, Pos1, Pos2, N_n, Num_pontes, Nova_entrada).

% ------------------------------------------------------------------------------
% Auxiliar: adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, Novo_estado)
% Novo_estado eh o estado resultante de aplicar adiciona_num_pontes as
% entradas do Estado que pertencem a Ilha1 e a Ilha2
% ------------------------------------------------------------------------------
% entra no predicado auxiliar adiciona_pontes/6
adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, Novo_estado) :-
    adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, [], Novo_estado).

% adiciona_pontes/6
% condicao de paragem: quando a lista Estado esta vazia
adiciona_pontes([], _, _, _, Novo_estado, Novo_estado) :- !.

% se a entrada pertencer a uma das ilhas
adiciona_pontes([P|Estado], Ilha1, Ilha2, Num_pontes, Aux_estado, Novo_estado) :-
    P = [Ilha_aux, _, _],
    ilhas_iguais(Ilha_aux, Ilha1, Ilha2), !,
    posicao_ilha(Ilha1, Pos1),
    posicao_ilha(Ilha2, Pos2),
    adiciona_num_pontes(P, Pos1, Pos2, 0, Num_pontes, Nova_entrada),
    append(Aux_estado, [Nova_entrada], Novo_aux_estado),
    adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, Novo_aux_estado, Novo_estado).

% se a entrada nao pertencer a uma das ilhas
adiciona_pontes([P|Estado], Ilha1, Ilha2, Num_pontes, Aux_estado, Novo_estado) :-
    append(Aux_estado, [P], Novo_aux_estado),
    adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, Novo_aux_estado, Novo_estado).

% ------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% Novo_estado eh o estado que se obtem de Estado por adicao de Num_pontes 
% pontes entre Ilha1 e Ilha2
% ------------------------------------------------------------------------------
junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
    adiciona_pontes(Estado, Ilha1, Ilha2, Num_pontes, N_estado),
    posicao_ilha(Ilha1, Pos1),
    posicao_ilha(Ilha2, Pos2),
    actualiza_vizinhas_apos_pontes(N_estado, Pos1, Pos2, Estado_aux),
    trata_ilhas_terminadas(Estado_aux, Novo_estado).
