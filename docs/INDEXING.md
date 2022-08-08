Introduction
============

In Prolog systems there are two types of indexing used: 1) program-like
and 2) data-like. They are of a radically difference nature. Trealla
makes this distinction.


Program-like indexing
=====================

This is the familiar lookup of rules that constitute a predicate. It
has to handle backtracking and allow efficient and effective early
choice-point elimination. Things that affect this capability are
dynamic and multifile predicates. Program-like predicates are usually
of limited size, typically at most a few hundred to a few thousand
clauses.

Points to go over...

 Uniqueness over the full term

 Uniqueness over arg1 & arg2

 Ground queries (in full or part)


Data-like indexing
==================

This is more akin to a traditional database based on a key-value type
search, possibly with duplicates. Backtracking is usually not an
issue, and predicates may be dynamic. The number of such rules may be
in the tens of thousands plus.

Points to go over...

 Logical-update semantics... the transactional view

 When are delete clauses (garbage) collected



