%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

%------------------------------
% Ejemplo:  
%------------------------------

%Cargar la base en una lista, imprimir la lista en consola y guardar todo en un nuevo archivo.
%No olvides poner las rutas correctas para localizar el archivo kb.txt en tu computadora!!!

ejemplo:-
	open_kb('kb.txt',KB),
	write('KB: '),
	write(KB),
	save_kb('new_kb.txt',KB).

%------------------------------
% Operators
%------------------------------
:- op(800,xfx,'=>').
:- op(801,xfx,'=>>').

%------------------------------
% List Utilities
%------------------------------

% Open and save KB
openk(KB):-
	open_kb('kb.txt', KB).
savek(KB):-
	save_kb('kb.txt', KB).

% member_of(X, L)
% Checks if X is a member of L
member_of(X, [X|_]).
member_of(X, [_|T]) :-
	member_of(X, T).

% get_class_of(X, KB, C)
% Gets the class of X inside the KB, and binds it to C.
get_class_of(_, [], unknown).
get_class_of(Id, [class(C,_,_,_,O)|_], C) :-
	is_member_of_object_list(Id, O).
get_class_of(Id, [_|T], C) :-
	get_class_of(Id, T, C).

% Tells if an id exist in an object list
is_member_of_object_list(Id, [[Ids,_,_]|_]) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T]) :-
	is_member_of_object_list(Id, T).

% Tells if an id exist in an object list and binds the list to which it belongs to Ids
is_member_of_object_list(Id, [[Ids,_,_]|_], Ids) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T], Ids) :-
	is_member_of_object_list(Id, T, Ids).

% substitute_element(X, Y, L, NL)
% Turns every element X in a list L into Y, and binds the new list to NL.
substitute_element(_, _, [], []).
substitute_element(X, Y, [X|T], [Y|Z]) :-
	substitute_element(X, Y, T, Z).
substitute_element(X, Y, [H|T], [H|Z]) :-
	substitute_element(X, Y, T, Z).

% delete_element(X, L, NL)
% Eliminates element X from a list L, and binds the new list to NL.
delete_element(_, [], []).
delete_element(X, [X|T], Y) :-
  delete_element(X, T, Y).
delete_element(X, [H|T], [H|Y]) :-
  delete_element(X, T, Y).

%------------------------------
% Create
%------------------------------

new_class(Name, Mother, OldKB, NewKB) :-
	append(OldKB, [class(Name, Mother, [], [], [])], NewKB).

new_class_property(Property, Weight, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, OldProps, Rels, Inst), class(Class, Mother, NewProps, Rels, Inst), OldKB, NewKB),
	append(OldProps, [[Property, Weight]], NewProps).


new_class_relation(Relation, Weight, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, OldRels, Inst), class(Class, Mother, Props, NewRels, Inst), OldKB, NewKB),
	append(OldRels, [[Relation, Weight]], NewRels).

%-------------------------------

% Adds a new object of Object id of the form [name_1, name_2...] to a Class in OldKB, and binds the new list to NewKB
new_object(ObjectId, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	append(OldInst, [[ObjectId, [], []]], NewInst).

new_object_property(Property, Weight, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_object_list(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObOldProps, ObRels], [ObIds, ObNewProps, ObRels], OldInst, NewInst),
	append(ObOldProps, [[Property, Weight]], ObNewProps).

new_object_relation(Relation, Weight, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_object_list(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, Rels, OldInst), class(Class, Mother, Props, Rels, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObOldRels], [ObIds, ObProps, ObNewRels], OldInst, NewInst),
	append(ObOldRels, [[Relation, Weight]], ObNewRels).

%------------------------------
% Delete
%------------------------------

% Deletes class C from the list OldKB and binds the new list to NewKB
delete_class(C, OldKB, NewKB) :-
	delete_all_relations_with(C, OldKB, X),
	get_mother_class(C, OldKB, M),
	copy_objects_to_another_class(C, M, X, Y),
	change_children_mother_class(C, M, Y, Z),
	delete_element(class(C,_,_,_,_), Z, NewKB).

% get_mother_class(C, KB, M)
% Gets the mother class from the class C in KB, and binds it to M
get_mother_class(_, [], none).
get_mother_class(C, [class(C,M,_,_,_)|_], M).
get_mother_class(C, [_|T], M):-
	get_mother_class(C, T, M).

% copy_objects_to_another_class
% Copies all the objects from class C in the list OldKB to a class X, and binds the new list to NewKB
copy_objects_to_another_class(C, X, OldKB, NewKB) :-
	get_object_list(C, OldKB, O),
	substitute_element(class(X, Mother, Props, Rels, OldInst), class(X, Mother, Props, Rels, NewInst), OldKB, NewKB),
	append(OldInst, O, NewInst).

% get_objects_list(C, KB, NL)
% Gets the object list from the class C in KB, and binds it to NL	
get_object_list(_, [], []).
get_object_list(C, [class(C,_,_,_,O)|_], O).
get_object_list(C, [_|T], O):-
	get_object_list(C, T, O).

%change_children_mother_class
% Changes the mother class of C's children to X
change_children_mother_class(C, X, OldKB, NewKB) :-
	get_class_children(C, OldKB, Children),
	change_classes_mother_class(Children, X, OldKB, NewKB).

% get_class_children
get_class_children(_, [], []).
get_class_children(C, [class(N,C,_,_,_)|T], L) :-
	get_class_children(C, T, L1),
	append([N],L1,L).
get_class_children(C, [_|T], L):-
	get_class_children(C, T, L).

%change_classes_mother_class(LC, X, OldKB, NewKB)
% Changes the mother class of a list of classes to X
change_classes_mother_class([],_,L,L).
change_classes_mother_class([C|T], X, OldKB, NewKB) :-
	change_class_mother_class(C, X, OldKB, NL),
	change_classes_mother_class(T, X, NL, NewKB).
		
% Changes the mother class of C to X
change_class_mother_class(C, X, OldKB, NewKB) :-
	substitute_element(class(C, _, Props, Rels, Inst), class(C, X, Props, Rels, Inst), OldKB, NewKB).

%-----------------------------------

%delete_object
delete_object(ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, C),
	get_object_list(C, OldKB, OL),
	is_member_of_object_list(ObjectId, OL, IdList),
	delete_all_relations_with_list(IdList, OldKB, X),
	substitute_element(class(C, Mother, Props, Rels, OldInst), class(C, Mother, Props, Rels, NewInst), X, NewKB),
	delete_element([IdList,_,_],OldInst,NewInst).

%delete_all_relations_with_list
delete_all_relations_with_list([], L, L).
delete_all_relations_with_list([Id|T], OldKB, NewKB) :-
	delete_all_relations_with(Id, OldKB, X),
	delete_all_relations_with_list(T, X, NewKB).

%delete_all_relations_with
delete_all_relations_with(_,[],[]).
delete_all_relations_with(Id, [class(C,M,P,OldR,OldI)|T], [class(C,M,P,NewR,NewI)|L]) :-
	delete_relations_with(Id, OldR, NewR),	
	delete_class_relations_with(Id, OldI, NewI),
	delete_all_relations_with(Id, T, L).

% deletes_list_relation_with
% Deletes relation with a certain Id (class or object) on a class
delete_class_relations_with(_, [], []).
delete_class_relations_with(Id, [[Ids,P,OldR]|T], [[Ids,P,NewR]|L]) :-
	delete_relations_with(Id, OldR, NewR),
	delete_class_relations_with(Id, T, L).

% Deletes id from relations list
delete_relations_with(Id, OldR, NewR) :-
	delete_element([_=>Id,_], OldR, X),
	delete_element([not(_=>Id),_], X, NewR).

%------------------------------------

%get_properties_from_class
get_properties_from_class(_, [], []).
get_properties_from_class(C, [class(C,_,P,_,_)|_], P).
get_properties_from_class(C, [_|T], P) :-
	get_properties_from_class(C, T, P).

%get_properties_from_object
get_properties_from_object(_, [], []).
get_properties_from_object(O, [[Ids,P,_]|_], P) :-
	member_of(O, Ids).
get_properties_from_object(O, [_|T], P) :-
	get_properties_from_object(O, T, P).

%get_relations_from_class
get_relations_from_class(_, [], []).
get_relations_from_class(C, [class(C,_,_,R,_)|_], R).
get_relations_from_class(C, [_|T], R) :-
	get_relations_from_class(C, T, R).

%get_relations_from_object(O, OL, R)
get_relations_from_object(_, [], []).
get_relations_from_object(O, [[Ids,_,R]|_], R):-
	member_of(O, Ids).
get_relations_from_object(O, [_|T], R) :-
	get_relations_from_object(O, T, R).

%------------------------------

%delete_property_of_class(P, C, OldKB, NewKB)
% Deletes a specific property of a class
delete_property_of_class(Property, C, OldKB, NewKB) :-
	get_properties_from_class(C, OldKB, OldP),
	delete_element([Property,_], OldP, NewP),
	substitute_element(class(C, M, _, R, O), class(C, M, NewP, R, O), OldKB, NewKB).

%delete_relation_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation of a class
delete_relation_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_from_class(C, OldKB, OldR),
	delete_element([Relation,_], OldR, NewR),
	substitute_element(class(C, M, P, _, O), class(C, M, P, NewR, O), OldKB, NewKB).

%delete_property_of_object(P, O, OldKB, NewKB)
% Deletes a specific property of an object
delete_property_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, R, OldO), class(C, M, P, R, NewO), OldKB, NewKB),
	get_properties_from_object(O, OldO, OldP),
	delete_element([Property,_], OldP, NewP),
	substitute_element([Ids,OldP,OR], [Ids,NewP,OR], OldO, NewO).

%delete_relation_of_object(R, C, OldKB, NewKB)
delete_relation_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, R, OldO), class(C, M, P, R, NewO), OldKB, NewKB),
	get_relations_from_object(O, OldO, OldR),
	delete_element([Relation,_], OldR, NewR),
	substitute_element([Ids,OP,OldR], [Ids,OP,NewR], OldO, NewO).

%------------------------------
% Useless classes
%------------------------------

% get_objects_from_class(C, KB, L)
% Gets all the objects from a class C in the KB, and puts them on a list L
%get_objects_from_class(_,[],[]).
%get_objects_from_class(C, [class(C,_,_,_,O)|_], L) :-
%	get_objects_ids(O, L).
%get_objects_from_class(C, [_|T], L) :-
%	get_objects_from_class(C, T, L).

% get_object_ids(O, OL)

% Gets a list of the object id lists from a list of objects
%get_objects_id_lists([], []).
%get_objects_id_lists([[Ids,_,_]|T], [Ids|L]) :-
%	get_objects_id_lists(T, L).

% flatten_list(L, NL)
flatten_list([], []).
flatten_list([X|T], NL) :-
	flatten_list(X, L1),
	flatten_list(T, L2),
	append(L1, L2, NL).
flatten_list(L, [L]).

%------------------------------------

% X is in the list L
is_in_list(_,[],unknown).
is_in_list(X,[not(X)|_],no).
is_in_list(X,[X|_],yes).
is_in_list(X,[_|T],A):-
        is_in_list(X,T,A).

% Intersection of two lists
intersect([],_,[]).
intersect(_,[],[]).
intersect([X|T],List2,[X|Rest]):-
        is_in_list(X,List2,yes),
        intersect(T,List2,Rest).

%flatten_preferences(P, FP)
% Flatten preferences
%flatten_preferences([], []).
%flatten_preferences([P|T], [P|L]) :-
%	is_flat(P, yes),
%	flatten_preferences(T, L).
%flatten_preferences([P|T], FP) :-
%	is_flat(P, no),
%	flatten_preference(P, L1),
%	flatten_preferences(T, L2),
%	append(L1, L2, FP).
	
% Flatten preference
flatten_preference([], []).
flatten_preference(X=>>Z, [X=>>Z|T]) :-
	flatten_preference(X, T).
flatten_preference(X=>>Y, [X=>>Y|_]). 

is_flat([_] =>> [_], yes).
is_flat(_ =>> _, no).

%resolve_rules(R, Pref, Prem, L)
resolve_rules([], _, _, []).
resolve_rules([X =>> Y|T], Pref, Prem, [Y|L]) :-
	resolve_rule(X =>> Y, Pref, Prem, yes),
	resolve_rules(T, Pref, Prem, L).
resolve_rules([X =>> Y|T], Pref, Prem, L) :-
	resolve_rule(X =>> Y, Pref, Prem, no),
	resolve_rules(T, Pref, Prem, L).

%resolve_rule(R, Pref, Prem, yes)
% Say if the rule is complied according to the premises and preferences
resolve_rule(_, _, [], no).
resolve_rule(X =>> _, _, Prem, yes) :-
	is_in_list(X, Prem, yes).
resolve_rule(X =>> _, Pref, Prem, R) :-
	is_consequent_of_pref(X, Pref, P, yes),
	resolve_rule(P, Pref, Prem, R).
resolve_rule(_, _, _, no).

is_consequent_of_pref(_, [], _, no).
is_consequent_of_pref(X, [Ant =>> X|_], Ant =>> X, yes).
is_consequent_of_pref(X, [_|T], P, R) :-
	is_consequent_of_pref(X, T, P, R).
	
	
	
	






















