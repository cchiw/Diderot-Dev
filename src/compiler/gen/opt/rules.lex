(* rules.lex
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

%name RulesLex;

%arg (lexErr);

%let digit = [0-9];
%let int = {digit}+;
%let alpha=[A-Za-z];
%let id={alpha}({alpha} |{digit}|"-")*;
%let ws=[ \t |\n];
%let eol="\n";  (*Just added for comments section*)

%states INITIAL STRING COM1 COM2;

%defs(
  structure T = RulesTokens
  type lex_result = T.token
  fun mkOP s = T.OPER(Atom.atom s);
  fun eof() = T.EOF
);

{id}        => (Keywords.idToken yytext);
{ws}+       => (skip());
"("         => (T.LP);
")"         => (T.RP);
"<"         => (T.LANGLE);
">"         => (T.RANGLE);
","         => (T.COMMA);
";"         => (T.SEMI);
"=>"        => (T.ARROW);


(*
Not sure if I need this section 

<STRING>{esc}		=> (addStr(valOf(String.fromString yytext)); continue());
<STRING>{sgood}+	=> (addStr yytext; continue());
<STRING> "\""		=> (YYBEGIN INITIAL; mkString());

<STRING> .		=> (lexErr(yypos, [
				"bad character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());
*)

(*Comments*)

<INITIAL> "//"  => (YYBEGIN COM1; skip());
<COM1> {eol}    => (YYBEGIN INITIAL; skip());
<COM1> .        =>( skip());

<INITIAL> "/*"  => (YYBEGIN COM2; skip());
<COM2>    "*/"  => (YYBEGIN INITIAL; skip());
<COM2>     .    => (skip());
