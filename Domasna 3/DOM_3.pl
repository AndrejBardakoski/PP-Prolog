%ZAD 1
%Task1

%lice(Ime, Hrana, Hobi, Maica)

%1. Тео седи најлево и јаде сендвич.
fakt1(L):-L=[lice(teo,sendvic,_,_)|_].

%2. Мира сака да решава крстозбори и ужива во јадењето пита.
fakt2(L):-clen(lice(mira,pita,krstozbor,_), L).

%3. Девојката има бела маица.
fakt3(L):-devojka(X), clen((lice(X,_,_,bela)), L).

%4. Бруно има жолта маица.
fakt4(L):-clen(lice(bruno,_,_,zolta), L).

%5. Оној што сака да пишува јаде хамбургер.
fakt5(L):-clen(lice(_,hamburger,pisuva,_), L).

%6. Личноста која јаде пита седи покрај Тео.
fakt6(L):-sediDo(lice(_,pita,_,_), lice(teo,_,_,_), L).

%7. Бруно седи покрај оној што јаде пица.
fakt7(L):-sediDo(lice(bruno,_,_,_), lice(_,pica,_,_), L).

%8. Личноста која седи покрај онаа во бела маица сака пица.
fakt8(L):-sediDo(lice(_,pica,_,_), lice(_,_,_,bela), L).

%9. Игор сака да чита.
fakt9(L):-clen(lice(igor,_,cita,_), L).

%10. Сина маица има личноста која седи десно од девојката.
fakt10(L):-devojka(X), clenAB(lice(X,_,_,_),lice(_,_,_,sina), L).

devojka(mira).

%clen(X,L) X e element na L
clen(X,[X|_]).
clen(X,[Y|L]):-X\==Y, clen(X,L).

%sediDo(A,B,L) True ako A i B se sosedni elementi vo L
sediDo(A,B,L):-clenAB(A,B,L).
sediDo(A,B,L):-clenAB(B,A,L).

%clenAB True ako A i B se sosedni elementi vo L i pritoa B e posle (desno od) A
clenAB(A,B,[A,B|_]).
clenAB(A,B,[_|L]):-clenAB(A,B,L).



%Task 2
reshenie(L) :-
% Inicijalizacija na L  na 4 elementi od tipot lice(Ime, Hrana, Hobi, Maica) 
% kade Ime, Hrana, Hobi, Maica se nepoznati na pocetok
    L=[lice(_,_,_,_),
       lice(_,_,_,_),
       lice(_,_,_,_),
       lice(_,_,_,_)],
    
    %gi proveruvame site fakti
    fakt1(L),
    fakt2(L),
    fakt3(L),
    fakt4(L),    
    fakt5(L),    
    fakt6(L),
    fakt7(L),
    fakt8(L),
    fakt9(L),
    fakt10(L),
    
    % dopolnitelno nekoj od licata nosi crvena maica, a nekoj ima hobi da fotografira 
	% (ne se spomnati vo faktite no se del od resenieto)
   	clen(lice(_,_,_,crvena), L),
	clen(lice(_,_,fotografira,_), L).