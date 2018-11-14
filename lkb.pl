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

% X is in the list L
is_in_list(_,[],unknown).
is_in_list(X,[not(X)|_],no).
is_in_list(X,[X|_],yes).
is_in_list(X,[_|T],A):-
        is_in_list(X,T,A).

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

% Delete repetitions in a list
delete_repetitions([],[]).
delete_repetitions([H|T],L):-
        delete_element(H,T,L1),
        delete_repetitions(L1,L2),
        append([H],L2,L).

% list_difference(L1,L2,L).
% L is the result of deleting all elements of L2 from L1
list_difference(L1,[],L1).
list_difference(L1,[H|T],L):-
    delete_element(H,L1,L2),
    list_difference(L2,T,L).

% pick_first_for_each(List,Choices)
% Picks first element of each element of List
pick_first_for_each([],[]).
pick_first_for_each([[H|_]|S], [H|Rest]):-
        pick_first_for_each(S,Rest).

% Intersection of two lists
intersect([],_,[]).
intersect(_,[],[]).
intersect([X|T],List2,[X|Rest]):-
        is_in_list(X,List2,yes),
        intersect(T,List2,Rest).

%----------------------------------
% General routines for working with objects  
%----------------------------------

% get_class_objects(Class, KB, Objs)
% Gets the object list of Class in KB, no inheritance
get_class_objects(_, [], []).
get_class_objects(Class, [class(Class,_,_,_,_,_,Objs)|_], Objs).
get_class_objects(Class, [_|T], Objs):-
    get_class_objects(Class, T, Objs).

% get_classes_objects(Classes,KB,Objs)
% Gets the list of objects of a list of classes 
get_classes_objects([],_,[]).
get_classes_objects(_,[],[]).
get_classes_objects([H|T],B,Objs):-
     get_class_objects(H,B,HObjs),
     get_classes_objects(T,B,OthersObjs),
     append(HObjs,OthersObjs,Objs).

% get_class_of(X, KB, C)
% Gets the class of X inside the KB, and binds it to C.
get_class_of(_, [], unknown).
get_class_of(Id, [class(C,_,_,_,_,_,O)|_], C) :-
	is_member_of_object_list(Id, O).
get_class_of(Id, [_|T], C) :-
	get_class_of(Id, T, C).

% Tells if an id exist in an object list
is_member_of_object_list(Id, [[Ids,_,_,_,_]|_]) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T]) :-
	is_member_of_object_list(Id, T).

% Tells if an id exist in an object list and binds the list to which it belongs to Ids
is_member_of_object_list(Id, [[Ids,_,_,_,_]|_], Ids) :-
	member_of(Id, Ids).
is_member_of_object_list(Id, [_|T], Ids) :-
	is_member_of_object_list(Id, T, Ids).

find_object_by_id(_,[],unknown).   
find_object_by_id(Id,[class(_,_,_,_,_,_,Objs)|_],Obj):-
        is_in_list(Obj,Objs,yes),
        is_member_of_object_list(Id,[Obj]).
find_object_by_id(Id,[_|T],Obj):-
        find_object_by_id(Id,T,Obj).

%----------------------------------
% Relatives of a class 
%----------------------------------

% get_class_parent(Class,KB,Parent).
% Gets the parent class of Class in KB
get_parent_class(_,[],unknown).
get_parent_class(Class,[class(Class,Parent,_,_,_,_,_)|_],Parent).
get_parent_class(Class,[_|T],Parent):- 
        get_parent_class(Class,T,Parent).

% get_class_ascendants(Class,KB,Ascendants)
% gets full list of ascendant (antecesor) classes of a class
get_class_ascendants(_,[],[]).
get_class_ascendants(Class,[class(Class,none,_,_,_,_,_)|_],[]).
get_class_ascendants(Class,B,Ascendants):-
      get_parent_class(Class,B,Parent),
      get_class_ascendants(Parent,B,ParentAscendants),
      append([Parent],ParentAscendants,Ascendants).
      
% get_classes_ascendants(Classes,KB,Ascendants)
% gets all ascendant classes of a list of classes
get_classes_ascendants([],_,[]).
get_classes_ascendants([H|T],B,Ascendants):-
        get_class_ascendants(H,B,HAscendants),
        get_classes_ascendants(T,B,OthersAscendants),
        append(HAscendants,OthersAscendants,Ascendants).

% get_class_children(Class,KB,Children)
% Gets the list of children of Class in KB
get_class_children(_,[],[]).
get_class_children(Class,[class(Child,Class,_,_,_,_,_)|T],Children):-
        get_class_children(Class,T,Siblings),
        append([Child],Siblings,Children).
get_class_children(Class,[_|T],Children):-
        get_class_children(Class,T,Children).

% get_classes_children(Classes,KB,Children)
% Gets all the child classes of a list of classes
get_classes_children([],_,[]).
get_classes_children([H|T],B,Children):-
        get_class_children(H,B,HChildren),
        get_classes_children(T,B,OthersChildren),
        append(HChildren,OthersChildren,Children).

% get_classes_descendants(Classes,KB,Descendants)
% Gets all descendant classes of a list of classes, including themselves
get_classes_descendants([],_,[]).
get_classes_descendants(Classes,B,Descendants):-
        get_classes_children(Classes,B,FirstLevel),
        get_classes_descendants(FirstLevel,B,NextLevels),
        append(Classes,NextLevels,Descendants).

% get_class_descendants(Class,KB,Descendants).
% Full list of descendant classes of a class
get_class_descendants(_,[],[]).
get_class_descendants(Class,B,Descendants):-
        get_class_children(Class,B,Children),
        get_classes_descendants(Children,B,Descendants).

%------------------------------------
% Properties, Relations and Preferences of Classes and Objects
%------------------------------------

%get_properties_from_class
get_properties_from_class(_, [], []).
get_properties_from_class(C, [class(C,_,P,_,_,_,_)|_], P).
get_properties_from_class(C, [_|T], P) :-
	get_properties_from_class(C, T, P).

%get_properties_preferences_from_class
get_properties_preferences_from_class(_, [], []).
get_properties_preferences_from_class(C, [class(C,_,_,PP,_,_,_)|_], PP).
get_properties_preferences_from_class(C, [_|T], PP) :-
	get_properties_preferences_from_class(C, T, PP).

%get_properties_from_object
get_properties_from_object(_, [], []).
get_properties_from_object(O, [[Ids,P,_,_,_]|_], P) :-
	member_of(O, Ids).
get_properties_from_object(O, [_|T], P) :-
	get_properties_from_object(O, T, P).

%get_properties_preferences_from_object
get_properties_preferences_from_object(_, [], []).
get_properties_preferences_from_object(O, [[Ids,_,PP,_,_]|_], PP) :-
	member_of(O, Ids).
get_properties_preferences_from_object(O, [_|T], PP) :-
	get_properties_preferences_from_object(O, T, PP).

%get_relations_from_class
get_relations_from_class(_, [], []).
get_relations_from_class(C, [class(C,_,_,_,R,_,_)|_], R).
get_relations_from_class(C, [_|T], R) :-
	get_relations_from_class(C, T, R).

%get_relations_preferences_from_class
get_relations_preferences_from_class(_, [], []).
get_relations_preferences_from_class(C, [class(C,_,_,_,_,PR,_)|_], PR).
get_relations_preferences_from_class(C, [_|T], PR) :-
	get_relations_preferences_from_class(C, T, PR).

%get_relations_from_object(O, OL, R)
get_relations_from_object(_, [], []).
get_relations_from_object(O, [[Ids,_,_,R,_]|_], R):-
	member_of(O, Ids).
get_relations_from_object(O, [_|T], R) :-
	get_relations_from_object(O, T, R).

%get_relations_preferences_from_object(O, OL, R)
get_relations_preferences_from_object(_, [], []).
get_relations_preferences_from_object(O, [[Ids,_,_,_,PR]|_], PR):-
	member_of(O, Ids).
get_relations_preferences_from_object(O, [_|T], PR) :-
	get_relations_preferences_from_object(O, T, PR).

%---------------------------------------------------
% Order pairs [property,weight] by weight in an
% ascending manner
%---------------------------------------------------
order_by_weights([],[]).
order_by_weights([[X,N],[Y,M]],[[X,N],[Y,M]]):-
        (N < M; N = M).
order_by_weights([[X,N],[Y,M]],[[Y,M],[X,N]]):-
        M < N.
order_by_weights([[X,N]|T],Ordered):-
        order_by_weights(T,S),
        insert_pair([X,N],S,Ordered).

insert_pair([X,N],[],[[X,N]]).
insert_pair([X,N],[[H,M]|T],Ordered):-
        (N < M; N = M),
        append([[X,N]],[[H,M]|T],Ordered).
insert_pair([X,N],[[H,M]|T],[[H,M]|TOrdered]):-
        insert_pair([X,N],T,TOrdered).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Creating------------------------------*
%---------------------------------------------------------------*
%****************************************************************

new_class(Name, Mother, OldKB, NewKB) :-
	append(OldKB, [class(Name, Mother, [], [], [], [], [])], NewKB).

new_class_property(Property, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, OldProps, PrefP, Rels, PrefR, Inst), class(Class, Mother, NewProps, PrefP, Rels, PrefR, Inst), OldKB, NewKB),
	append(OldProps, [Property], NewProps).

new_class_property_preference(Property, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, OldPrefP, Rels, PrefR, Inst), class(Class, Mother, Props, NewPrefP, Rels, PrefR, Inst), OldKB, NewKB),
	append(OldPrefP, [Property], NewPrefP).

new_class_relation(Relation, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, OldRels, PrefR, Inst), class(Class, Mother, Props, PrefP, NewRels, PrefR, Inst), OldKB, NewKB),
	append(OldRels, [Relation], NewRels).

new_class_relation_preference(Relation, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, Rels, OldPrefR, Inst), class(Class, Mother, Props, PrefP, Rels, NewPrefR, Inst), OldKB, NewKB),
	append(OldPrefR, [Relation], NewPrefR).

%-------------------------------

% Adds a new object of Object id of the form [name_1, name_2...] to a Class in OldKB, and binds the new list to NewKB
new_object(ObjectId, Class, OldKB, NewKB) :-
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	append(OldInst, [[ObjectId, [], [], [], []]], NewInst).

new_object_property(Property, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObOldProps, ObPrefP, ObRels, ObPrefR], [ObIds, ObNewProps, ObPrefP, ObRels, ObPrefR], OldInst, NewInst),
	append(ObOldProps, [Property], ObNewProps).

new_object_property_preference(Property, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObOldPrefP, ObRels, ObPrefR], [ObIds, ObProps, ObNewPrefP, ObRels, ObPrefR], OldInst, NewInst),
	append(ObOldPrefP, [Property], ObNewPrefP).

new_object_relation(Relation, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObPrefP, ObOldRels, ObPrefR], [ObIds, ObProps, ObPrefP, ObNewRels, ObPrefR], OldInst, NewInst),
	append(ObOldRels, [Relation], ObNewRels).

new_object_property_relation(Relation, ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, Class),
	get_class_objects(Class, OldKB, OldInst),
	is_member_of_object_list(ObjectId, OldInst, ObIds),
	substitute_element(class(Class, Mother, Props, PrefP, Rels, PrefR, OldInst), class(Class, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	substitute_element([ObIds, ObProps, ObPrefP, ObRels, ObOldPrefR], [ObIds, ObProps, ObPrefP, ObRels, ObNewPrefR], OldInst, NewInst),
	append(ObOldPrefR, [Relation], ObNewPrefR).

%------------------------------
% Functions for deleting the appereances of an object/class in every relation inside the KB
%------------------------------

%delete_all_relations_with_list
delete_all_relations_with_list([], L, L).
delete_all_relations_with_list([Id|T], OldKB, NewKB) :-
	delete_all_relations_with(Id, OldKB, X),
	delete_all_relations_with_list(T, X, NewKB).

%delete_all_relations_with
delete_all_relations_with(_,[],[]).
delete_all_relations_with(Id, [class(C,M,P,PP,OldR,PR,OldI)|T], [class(C,M,P,PP,NewR,PR,NewI)|L]) :-
	delete_relations_with(Id, OldR, NewR),
%	delete_relations_preferences_with(Id, OldPR, NewPR),	
	delete_class_relations_with(Id, OldI, NewI),
	delete_all_relations_with(Id, T, L).

% deletes_list_relation_with
% Deletes relation with a certain Id (class or object) on a class
delete_class_relations_with(_, [], []).
delete_class_relations_with(Id, [[Ids,P,PP,OldR,PR]|T], [[Ids,P,PP,NewR,PR]|L]) :-
	delete_relations_with(Id, OldR, NewR),
%	delete_relations_preferences_with(Id, OldPR, NewPR),
	delete_class_relations_with(Id, T, L).

% Deletes id from a relations list
delete_relations_with(Id, OldR, NewR) :-
	delete_element(_=>Id, OldR, X),
	delete_element(not(_=>Id), X, NewR).
delete_relations_preferences_with(Id, OldR, NewR) :-
	delete_element([_]=>>[_=>Id,_], OldR, X),
	delete_element([_]=>>[not(_=>Id),_], X, NewR).

%****************************************************************
%---------------------------------------------------------------*
%-------------------------Deleting------------------------------*
%---------------------------------------------------------------*
%****************************************************************

% Deletes class C from the list OldKB and binds the new list to NewKB
delete_class(C, OldKB, NewKB) :-
	delete_all_relations_with(C, OldKB, X),
	get_parent_class(C, OldKB, M),
	copy_objects_to_another_class(C, M, X, Y),
	change_children_mother_class(C, M, Y, Z),
	delete_element(class(C,_,_,_,_,_,_), Z, NewKB).

% copy_objects_to_another_class
% Copies all the objects from class C in the list OldKB to a class X, and binds the new list to NewKB
copy_objects_to_another_class(C, X, OldKB, NewKB) :-
	get_class_objects(C, OldKB, O),
	substitute_element(class(X, Mother, Props, PrefP, Rels, PrefR, OldInst), class(X, Mother, Props, PrefP, Rels, PrefR, NewInst), OldKB, NewKB),
	append(OldInst, O, NewInst).

%change_children_mother_class
% Changes the mother class of C's children to X
change_children_mother_class(C, X, OldKB, NewKB) :-
	get_class_children(C, OldKB, Children),
	change_classes_mother_class(Children, X, OldKB, NewKB).

%change_classes_mother_class(LC, X, OldKB, NewKB)
% Changes the mother class of a list of classes to X
change_classes_mother_class([],_,L,L).
change_classes_mother_class([C|T], X, OldKB, NewKB) :-
	change_class_mother_class(C, X, OldKB, NL),
	change_classes_mother_class(T, X, NL, NewKB).
		
% Changes the mother class of C to X
change_class_mother_class(C, X, OldKB, NewKB) :-
	substitute_element(class(C, _, Props, PrefP, Rels, PrefR, Inst), class(C, X, Props, PrefP, Rels, PrefR, Inst), OldKB, NewKB).

%-----------------------------------

%delete_object
delete_object(ObjectId, OldKB, NewKB) :-
	get_class_of(ObjectId, OldKB, C),
	get_class_objects(C, OldKB, OL),
	is_member_of_object_list(ObjectId, OL, IdList),
	delete_all_relations_with_list(IdList, OldKB, X),
	substitute_element(class(C, Mother, Props, PrefP, Rels, PrefR, OldInst), class(C, Mother, Props, PrefP, Rels, PrefR, NewInst), X, NewKB),
	delete_element([IdList,_,_,_,_],OldInst,NewInst).

%-----------------------------------

%delete_property_of_class(P, C, OldKB, NewKB)
% Deletes a specific property of a class
delete_property_of_class(Property, C, OldKB, NewKB) :-
	get_properties_from_class(C, OldKB, OldP),
	delete_element(Property, OldP, NewP),
	substitute_element(class(C, M, _, PP, R, PR, O), class(C, M, NewP, PP, R, PR, O), OldKB, NewKB).

%delete_property_preference_of_class(P, C, OldKB, NewKB)
% Deletes a specific property preference of a class
delete_property_preference_of_class(Property, C, OldKB, NewKB) :-
	get_properties_preferences_from_class(C, OldKB, OldPP),
	delete_element(Property, OldPP, NewPP),
	substitute_element(class(C, M, P, _, R, PR, O), class(C, M, P, NewPP, R, PR, O), OldKB, NewKB).

%delete_relation_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation of a class
delete_relation_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_from_class(C, OldKB, OldR),
	delete_element(Relation, OldR, NewR),
	substitute_element(class(C, M, P, PP, _, PR, O), class(C, M, P, PP, NewR, PR, O), OldKB, NewKB).

%delete_relation_preference_of_class(R, C, OldKB, NewKB)
% Deletes a specific relation preference of a class
delete_relation_preference_of_class(Relation, C, OldKB, NewKB) :-
	get_relations_preferences_from_class(C, OldKB, OldPR),
	delete_element(Relation, OldPR, NewPR),
	substitute_element(class(C, M, P, PP, R, _, O), class(C, M, P, PP, R, NewPR, O), OldKB, NewKB).

%-------------------------------------

%delete_property_of_object(P, O, OldKB, NewKB)
% Deletes a specific property of an object
delete_property_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_from_object(O, OldO, OldP),
	delete_element(Property, OldP, NewP),
	substitute_element([Ids,OldP,OPP,OR,OPR], [Ids,NewP,OPP,OR,OPR], OldO, NewO).

%delete_property_preference_of_object(P, O, OldKB, NewKB)
% Deletes a specific property preference of an object
delete_property_preference_of_object(Property, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),		
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_properties_preferences_from_object(O, OldO, OldOPP),
	delete_element(Property, OldOPP, NewOPP),
	substitute_element([Ids,OP,OldOPP,OR,OPR], [Ids,OP,NewOPP,OR,OPR], OldO, NewO).

%delete_relation_of_object(R, C, OldKB, NewKB)
% Deletes a specific relation of an object
delete_relation_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_from_object(O, OldO, OldR),
	delete_element(Relation, OldR, NewR),
	substitute_element([Ids,OP,OPP,OldR,OPR], [Ids,OP,OPP,NewR,OPR], OldO, NewO).

%delete_relation_preference_of_object(R, C, OldKB, NewKB)
% Deletes a specific relation preference of an object
delete_relation_preference_of_object(Relation, O, OldKB, NewKB) :-
	get_class_of(O, OldKB, C),
	substitute_element(class(C, M, P, PP, R, PR, OldO), class(C, M, P, PP, R, PR, NewO), OldKB, NewKB),
	get_relations_preferences_from_object(O, OldO, OldOPR),
	delete_element(Relation, OldOPR, NewOPR),
	substitute_element([Ids,OP,OPP,OR,OldOPR], [Ids,OP,OPP,OR,NewOPR], OldO, NewO).


%****************************************************************
%---------------------------------------------------------------*
%-------------------------Consulting----------------------------*
%---------------------------------------------------------------*
%****************************************************************

%------------------------------------
% Functions for inferring properties/relations from preferences
%------------------------------------

%inferences(Rules, Pref, Prem, L)
inferences([], _, _, []).
inferences([X =>> Y|T], Pref, Prem, [Y|L]) :-
	infere(X =>> Y, Pref, Prem, yes),
	inferences(T, Pref, Prem, L).
inferences([X =>> Y|T], Pref, Prem, L) :-
	infere(X =>> Y, Pref, Prem, no),
	inferences(T, Pref, Prem, L). 

%infere(Rule, Pref, Prem, Ans)
% Say if a rule can be inferred from a set of Preferences and Premises, and bind the answer to Ans
infere(X =>> _, Pref, Prem, Ans) :-
	validate_antecedents(X, Pref, Prem, R),
	all_yes(R, Ans).

% validate_antecedents(Ant, Pref, Prem, R)
% Check if every element of Ant is either explicitly stated in the Premises, or if they can be inferred from the list of Preferences
validate_antecedents([], _, _, []).
validate_antecedents([X|T], Pref, Prem, [R|L]) :-
	validate_antecedent(X, Pref, Prem, R),
	validate_antecedents(T, Pref, Prem, L).

% validate_antecedent
% Check if a certain property/relation X is either explicitly stated in the Premises, or if it can be inferred from the list of preferences
validate_antecedent(_,_,[],no).
validate_antecedent(X, _, Prem, yes) :-
	is_in_list(X, Prem, yes).
validate_antecedent(X, Pref, Prem, yes) :-
	is_consequent_of_pref([X], Pref, P, yes),
	extract_antecedents(P, Ant),
	validate_antecedents(Ant, Pref, Prem, L),
	all_yes(L, yes).
validate_antecedent(_,_,_,no).

% Extract the antecedent from a Rule of the form Ant =>> Consequence
extract_antecedents(Ant =>> _, Ant).

%all_yes(L, R)
% Check if all the elements in a certain list L are equal to "yes", and bind the answer to R.
all_yes([], yes).
all_yes([yes|T], R) :-
	all_yes(T, R).
all_yes([no|_], no).

%is_consequent_of_pref(X, Pref, P, R)
% Say if a certain X is the consequence of a certain rule R in a list of Preferences, and bind the answer to R
is_consequent_of_pref(_, [], _, no).
is_consequent_of_pref([X], [Ant =>> [X,W]|_], Ant =>> [X,W], yes).
is_consequent_of_pref(X, [_|T], P, R) :-
	is_consequent_of_pref(X, T, P, R).

%--------------------------------------
% Routines for working with properties 
%--------------------------------------
% Gets the properties of a list of classes
get_classes_properties(_,[],[]).
get_classes_properties([],_,[]).
get_classes_properties([H|T],B,Properties):-
        get_class_properties(H,B,HProperties),
        get_classes_properties(T,B,OthersProperties),
        append(HProperties,OthersProperties,Properties).

% get_class_properties_preferences(Class,KB,PrefProps).
% Gets the list of Properties Preferences of the Class
get_class_properties_preferences(_,[],[]).
get_class_properties_preferences(Class,[class(Class,_,_,PrefProps,_,_,_)|_],PrefProps).
get_class_properties_preferences(Class,[_|T],PrefProps):-
        get_class_properties_preferences(Class,T,PrefProps).

% get_classes_properties_preferences(Classes,KB,PropPrefs).
% Gets the property preferences  of a list of classes
get_classes_properties_preferences([],_,[]).
get_classes_properties_preferences([H|T],B,PropPrefs):-
        get_class_properties_preferences(H,B,HPropPrefs),
        get_classes_properties_preferences(T,B,OthersPropPrefs),
        append(HPropPrefs,OthersPropPrefs,PropPrefs).

% Gets the explicit properties of an object 
get_local_object_properties([_,Properties,_,_,_],Properties).

% Gets the explicit properties preferences of an object
get_object_properties_preferences([_,_,ObjPropPref,_,_],ObjPropPref).

% Full properties of a class, no specificity
all_class_properties(_,[],[]).
all_class_properties(Class,B,Properties):-
    get_local_class_properties(Class,B,LocalProperties),
    get_class_ascendants(Class,B,Ascendants),
    get_local_classes_properties(Ascendants,B,AscendantsProperties),
    append(LocalProperties,AscendantsProperties,Properties).

% Properties in present class, no inheritance
get_local_class_properties(_,[],[]).
get_local_class_properties(Class,[class(Class,_,Properties,_,_,_,_)|_],Properties).
get_local_class_properties(Class,[_|T],Properties):-
        get_local_class_properties(Class,T,Properties).

% Properties of a list of classes
get_local_classes_properties([],_,[]).
get_local_classes_properties(_,[],[]).
get_local_classes_properties([H|T],B,Properties):-
     get_local_class_properties(H,B,HProperties),
     get_local_classes_properties(T,B,OthersProperties),
     append(HProperties,OthersProperties,Properties).

% pick_objs_with_property(Property,KB,IdsList,IdswProperty)
% Picks all objects in a list for which Property holds
pick_objs_with_property(_,_,[],[]).
pick_objs_with_property(_,[],_,[]).
pick_objs_with_property(Property,B,[Id|T],[R|Rest]):-
        get_object_properties(Id,B,Properties),
        is_in_list(Property,Properties,R),
        pick_objs_with_property(Property,B,T,Rest).
pick_objs_with_property(Property,B,[_|T],Rest):-
        pick_objs_with_property(Property,B,T,Rest).

resolve_objects([], [], []).
resolve_objects([Id|T], [yes|RL], [Id|L]) :-
	resolve_objects(T, RL, L).
resolve_objects([_|T], [_|RL], L) :-
	resolve_objects(T, RL, L).

%-------------------------------
% Resolve the properties of a class
%-------------------------------

% Removes repetitions, inconsistencies, applies inferences,
% thus gets full properties 
infer_and_clean(PrefProps,First,Properties):-
        properties_cleanup(First,CleanL),
        inferences(PrefProps,PrefProps,CleanL,Inferred),
        order_by_weights(Inferred,OrdInferred),
        resolve_by_weights(OrdInferred,Resolved),
        append(CleanL,Resolved,Aux),
        properties_cleanup(Aux,Properties).

% resolve_by_weights([[prop1,w1],...,[propK,wK]],L)
% Picks properties with lower weight
resolve_by_weights([],[]).
resolve_by_weights([[not(X),_]|T],[not(X)|Rest]):-
        delete_element([X,_],T,Aux),
        resolve_by_weights(Aux,Rest).
resolve_by_weights([[X,_]|T],[X|Rest]):-
        delete_element([not(X),_],T,Aux),
        resolve_by_weights(Aux,Rest).
resolve_by_weights([_|T],L):-
        resolve_by_weights(T,L).
	
% Infer new properties using properties and preferences, 
% Returns list [[prop1,w1],...,[propK,wK]] 
properties_cleanup([],[]).
properties_cleanup(Properties,Solved):-
        delete_repetitions(Properties,Aux1),
        overwrite_attribute(Aux1,Aux2),
        delete_inconsistencies(Aux2,Solved).

overwrite_attribute([],[]).
overwrite_attribute([X => Y|T],[X => Y|Rest]):-
        delete_element(X => _,T,Aux),
        overwrite_attribute(Aux,Rest).
overwrite_attribute([H|T],[H|L]):-
        overwrite_attribute(T,L).

delete_inconsistencies([],[]).
delete_inconsistencies([not(X)|T],[not(X)|Rest]):-
        delete_element(X,T,Aux),
        delete_inconsistencies(Aux,Rest).
delete_inconsistencies([X|T],[X|Rest]):-
        delete_element(not(X),T,Aux),
        delete_inconsistencies(Aux,Rest).
delete_inconsistencies([_|T],Solved):-
        delete_inconsistencies(T,Solved).

        
%-----------------------------------
% MAIN Consulting routines
%-----------------------------------
% class_extension(Class,KB,Objs)
% The full list of the objects of Class in KB, observing inheritance
get_class_extension(_,[],[]).
get_class_extension(Class,B,Objs):-
    	get_class_objects(Class,B,ClassOnlyObjs),
    	get_class_descendants(Class,B,Descendants),
    	get_classes_objects(Descendants,B,DescendantsObjs),
    	append(ClassOnlyObjs,DescendantsObjs,Aux1),
    	pick_first_for_each(Aux1,Aux2),
    	pick_first_for_each(Aux2,Objs).

% get_object_membership(Id,KB,Classes). 
% The classes an object belongs to, observing inheritance
get_object_membership(_,[],[]).
get_object_membership(Id,B,Classes):-
    	get_class_of(Id,B,Class),
    	get_class_ascendants(Class,B,Ascendants),
    	append([Class],Ascendants,Classes).

% get_class_properties(Class, KB, Properties)
% Gets the full list of properties of Class in KB, observing preferences
get_class_properties(Class,B,Properties):-
        all_class_properties(Class,B,ExplicitProperties),
        get_class_properties_preferences(Class,B,PrefProps),
        infer_and_clean(PrefProps,ExplicitProperties,Properties).

% get_object_properties(Obj,KB,Properties).        
% Gets the full list of properties of Class in KB, observing preferences
get_object_properties(_,[],[]). 
get_object_properties(Id,B,Properties):-
        find_object_by_id(Id,B,Obj),
        get_local_object_properties(Obj,LocalProperties),
        get_object_membership(Id,B,Classes),
        get_classes_properties(Classes,B,InheritedProps),
        append(LocalProperties,InheritedProps,AllProps),
        get_object_properties_preferences(Obj,ObjPropPref),
        get_classes_properties_preferences(Classes,B, PropPref),
        append(ObjPropPref,PropPref,AllPropPref),
        infer_and_clean(AllPropPref,AllProps,Properties).

%%THE FOLLOWING IS NOT WORKING: my routine pick_objs_with_property cycles indefinetely
% get_property_extension(Property, KB, Objs).
% Gets the list of objects for which Property holds
get_property_extension(_,[],[]).
get_property_extension(Property,B,Objs):-
        get_class_extension(top,B,AllObjs),
        pick_objs_with_property(Property,B,AllObjs,RObjs),
	resolve_objects(AllObjs,RObjs,Objs).






















