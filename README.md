pt-estruturado
==============

An interpreter to "Português Estruturado" written in Haskell


Why?
====

Português estruturado is an commun pseudo language used in universets at Brasil to introduse Computter Engenner.
To herp studants undentunds better what happen with yours algorthimons this interpreter was write.

Portugues estruturado syntax
============================

The syntax used in this interpreter was based in syntax used at school, and is like below.


<code>
algoritimo Teste
/* mult
   line
   coment */
   
  var a logico; // line comment 
  var b real;
  var c int;
inicio
  b = 10 + -15.1 * -(2/3); 
  a = verdadeiro;
  a = a ou falso;
  //escreva(a);
  c = 3;
  escreva(falso ou falso);
  escreva(a);
  escreva(b+b);
  escreva(c%2);
fimalgoritimo
</code>

Usage
=====

cabal configure
cabal build
cabal install 

PTEstruturado code-exemple.pe

Or
--

runghc Main.hs code-expemple.pe
