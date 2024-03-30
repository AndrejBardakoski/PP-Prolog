# Домашна 3  - Интеграм 

## Задача 1 

### А) Репрезентација на факти 

`%lice(Ime, Hrana, Hobi, Maica)`

Имаме 4 лица (Студенти) каде секое лице има име јаде некоја храна има хоби и носи маица со одредена боја, едно лице ќе го  репрезентираме со предикатот **lice(Ime, Hrana, Hobi, Maica).** 

```prolog
%1. Тео седи најлево и јаде сендвич.
fakt1(L):-L=[lice(teo,sendvic,_,_)|_].
```

Во листата со лица на прва позиција го имаме Тео кој јаде сендвич, неговото хоби и боја на маица се непознати 

```prolog
%2. Мира сака да решава крстозбори и ужива во јадењето пита. 
fakt2(L):-clen(lice(mira,pita,krstozbor,_), L). 
```

Слично како и за Тео во листата со лица ја имаме Мира која јаде пита и решава крстозбор позицијата каде седи Мира ни е непозната па затоа го користиме предикатот **clen** што само проверува дали Мира е член во листата позицијата не е битна.  

```prolog
%3. Девојката има бела маица. 
fakt3(L):-devojka(X), clen((lice(X,_,_,bela)), L).
```

Кажуваме дека во листата со лица има некоја девојка со име Х и таа носи бела маица. 

```prolog
%4. Бруно има жолта маица. 
fakt4(L):-clen(lice(bruno,_,_,zolta), L).

%5. Оној што сака да пишува јаде хамбургер. 
fakt5(L):-clen(lice(_,hamburger,pisuva,_), L).
```
```prolog
%6. Личноста која јаде пита седи покрај Тео. 
fakt6(L):-sediDo(lice(_,pita,_,_), lice(teo,_,_,_), L).
```

Кажуваме дека во листата од лица има лице кое јаде и пита и лице кое се вика Тео и тие седат еден до друг. За ова се користи предикатот **sediDo.** 

```prolog
%7. Бруно седи покрај оној што јаде пица. 
fakt7(L):-sediDo(lice(bruno,_,_,_), lice(_,pica,_,_), L).

%8. Личноста која седи покрај онаа во бела маица сака пица. 
fakt8(L):-sediDo(lice(_,pica,_,_), lice(_,_,_,bela), L). 

%9. Игор сака да чита. 
fakt9(L):-clen(lice(igor,_,cita,_), L).
```

```prolog
%10. Сина маица има личноста која седи десно од девојката. 
fakt10(L):-devojka(X), clenAB(lice(X,_,_,_),lice(_,_,_,sina), L).
```


Кажуваме дека во листата од личности имаме лице кое носи сина маица и тоа седи десно од девојката. За да го постигне ме ова го користиме предикатот **clenAB.** 

```prolog
devojka(mira).
```

Имаме четири лица: Тео,Мира,Игор и Бруно од нив само Мира е девојка. 

```prolog
%clen(X,L) X e element na L 
clen(X,[X|_]).
clen(X,[Y|L]):-X==Y, clen(X,L). 
```

```prolog
%sediDo(A,B,L) True ako A i B se sosedni elementi vo L 
sediDo(A,B,L):-clenAB(A,B,L).
sediDo(A,B,L):-clenAB(B,A,L). 
```
**sediDo(A,B,L)  –** Провери дали  А и Б се соседни елементи во Л така што А е пред Б или Б е пред А. 

```prolog
%clenAB True ako A i B se sosedni elementi vo L i pritoa B e posle (desno od) A
clenAB(A,B,[A,B|_]). 
clenAB(A,B,[_|L]):-clenAB(A,B,L).
```
**clenAB –** изминувај ја листата елемент по елемент доколку во некој момент од итерирањето првите два елементи на листата се бараните А и Б соодветно тогаш врати **True**.  

### Б) reshenie(L) 

```prolog
%L e reshenie na integramot* 
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
```

Најпрво ја иницијализираме резултатната листа на листа која содржи 4 елементи (долку лица имаме) од типот 
`lice(Ime, Hrana, Hobi, Maica)`  каде информациите за име, храна, хоби и маица не ни се познати (поставуваме мемориски 
локации `_` ), потоа ги проверуваме предходно дефинираните факти еден по еден, на крај кажуваме дека некој од лицата има
хоби да фотографира и некој носи црвена маица, ови информации не се дел од фактите но се дел од задачата (доколку се 
изозстават резултатната листа ќе содржи мемориски локации на нивно место пример: `lice(teo,sendvic,_1702,_1704)` 
