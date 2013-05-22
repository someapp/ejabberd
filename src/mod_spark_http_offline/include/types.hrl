ifndef(__TYPES_HRL).
-define(__TYPES_HRL, true).

%% Return values
-type reason()	  :: atom().
-type ok(A)       :: {ok, A}.
-type error(A)    :: {error, A}.
-type warn(A)	  :: {warn, A}.
-type maybe(A, B) :: {ok, A} | {error, B}.
-type whynot()    :: ok | {error, _}.

%% Misc
-type alist(A, B) :: [{A, B}].
-type file()      :: string().

-endif. %include guard
