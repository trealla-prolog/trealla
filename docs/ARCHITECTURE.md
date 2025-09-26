Introduction
============

For convenience a fixed size (24-byte) cell has been implemented. Cells
are layed out in a flattened-arena form rather than a traditional AST.
The following diagrams illustrate the cell layout on 64-bit systems.


Interned
========

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        |               - UNUSED -                |
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                 val_off                 |
        +----------+---------+----------+---------+
   20   |               - UNUSED -                |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_INTERNED.
Where *arity* is always 0.
Where *num_cells* is always 1.
Where *val_off* is a byte-offset into the symbol table.

Two interned cells will unify if their *val_off* is the same.
An interned cell is always used for functor names.


Var
===

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        |               - UNUSED -                |
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                 val_off                 |
        +----------+---------+----------+---------+
   20   |       var_nbr      |      - UNUSED -    |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_VAR.
Where *arity* is always 0.
Where *flags* is 0.
Where *num_cells* is always 1.
Where *val_off* is a byte_offset into the symbol table.
Where *var_nbr* is the index into the current context
and refers to a *slot*.

A variable will usually only be found in asserted database rules.
Such a variable, when matched, will have an implied context. For
example when matching against a term (in a rule) all variables
in the term will be relative (the *var_nbr*) to the context.


Ref
===

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        |               - UNUSED -                |
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                 val_ctx                 |
        +----------+---------+----------+---------+
   20   |       var_nbr      |      - UNUSED -    |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_VAR.
Where *arity* is always 0.
Where *flags* is FLAG_VAR_REF
Where *num_cells* is always 1.
Where *val_ctx* is the context (actually a *frame* number)
Where *var_nbr* is the index into the specified context

A reference is a variable with an explicit context. They are
generated when copying terms or making instructions dynamically.


Integer
=======

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +                 val_int                 +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                                         |
        |               - UNUSED -                |
   20   |                                         |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_INT.
Where *arity* is always 0.
Where *flags* is 0.
Where *num_cells* is always 1.
Where *val_int* is a signed 64-bit integer.


Bigint
======

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +               val_bigint                +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                                         |
        |               - UNUSED -                |
   20   |                                         |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_INT.
Where *arity* is always 0.
Where *flags* is FLAG_INT_BIG.
Where *num_cells* is always 1.
Where *val_bigint* is a pointer.


Double
======

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +                val_float                +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                                         |
        +               - UNUSED -                +
   20   |                                         |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_DOUBLE.
Where *arity* is always 0
Where *flags* is 0.
Where *num_cells* is always 1.
Where *val_real* is a floating point *double*.


Cstring
=======

A small string < 16 bytes.

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |  chr_len |                              |
        +----------+                              +
   12   |                                         |
        +                 val_chr[16]             +
   16   |                                         |
        +                                         +
   20   |                                         |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_CSTRING.
Where *arity* is always 0.
Where *flags* is 0.
Where *num_cells* is always 1.
Where *chr_len* is the number of bytes (0-14) in *val_chr*.
Where *val_chr* is up to 14 bytes of UTF-8 chars, NULL-terminated.

A Cstring may be used for atoms that are not functors and need quoting.


Static BLOB
===========

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +                 val_str                 +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                                         |
        +                 len_str                 +
   20   |                                         |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB | FLAG2_STATIC.
Where *num_cells* is always 1.
Where *val_str* is a pointer to a slice of UTF-8 chars.
Where *len_str* is the length of the slice in bytes.

A static BLOB is length delimited, not NULL-terminated. The
primary use of a static BLOB is to point to memory that is never
freed, such as in a memory-mapped file.


Non-static BLOB
===============

A ref-counted string buffer.

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +               val_strbuf                +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |               strbuf_off                |
        +----------+---------+----------+---------+
   20   |               strbuf_len                |
        +----------+---------+----------+---------+
```

Where *tag* is TAG_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB.
Where *num_cells* is always 1.
Where *val_strbuf* is a pointer to a strbuf object.
Where *strbuf_off* is the byte offset into a slice of a strbuf.
Where *strbuf_len* is the length of the slice.

A *strbuf* contains a reference count for tracking lifetimes, a length
field and a data payload. The *strbuf_off* field can be used to
define the tail of a BLOB. Or use a *strbuf_len* to get a slice.


String
======

A string is an optimized form of a chars-list and can be a BLOB of
either type. The *arity* is 2 but has no functor and the *flag*
has FLAG_STRING set.


Compound
========

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +               - UNUSED -                +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                 val_off                 |
        +----------+---------+----------+---------+
   20   |               - UNUSED -                |
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 2
```

Where *tag* is TAG_LITERAL.
Where *arity* is > 0.
Where *flags* is 0.
Where *num_cells* is > 1 and includes the args.
Where *val_off* is a byte-offset into the symbol table of the functor name.
Where args are the following cells (see *num_cells*).

The first cell is the functor, the args follow based on arity. Compounds
can be nested to arbirary depth.


List
====

A list is just a special instance of a compound.

```
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 1
        +----------+---------+----------+---------+
    4   |                 num_cells               |
        +----------+---------+----------+---------+
    8   |                                         |
        +               - UNUSED -                +
   12   |                                         |
        +----------+---------+----------+---------+
   16   |                 val_off                 |
        +----------+---------+----------+---------+
   20   |               - UNUSED -                |
        +----------+---------+----------+---------+
    0   |   tag    |  arity  |       flags        |    CELL 2
```

Where *tag* is TAG_LITERAL.
Where *arity* is always 2.
Where *flags* is 0.
Where *num_cells* is > 1 and includes head & tail args.
Where *val_off* is a byte-offset into the symbol table to the functor name '.'.
Where args are the following cells (see *num_cells*).
Where the tail arg is usually a list.
Where the final tail arg is usually the atom *[]*.


Frames
======

A frame is an element of the frame stack. Each frame is of fixed size
and contains an index into the slot table of the base slot for the
frame. It also contains a count of the number of variables that make up
the frame. If a frame expands (creates new variables) the slots *may*
become discontiguous if it's not the top frame.

The frame plus it's slots constitute a working context (or *environment*)
for a set of goals. Choices can back-track to a given context.

Since only index numbers are used to refer to frames (a *ctx* number)
the frame space can be easily resized.

One of the most important properties of a frame is the choice generation.
This number signifies the choice point used to create the frame. A cut
needs to drop choice points back to this point.


Slots
=====

A slot is an element in the slot stack. Each slot holds a cell plus
it's context (for vars and compounds) plus attribute info.

Slots are cleared (and attributes reset) on backtracking via the
trailed record.

Since only index numbers are used to refer to slots (a *slot* number)
the slot space can be easily resized.

During execution of a builtin predicate (a C function) active slot
pointers (if any) may need to be refreshed after creating new variables
(eg. in length/2, copy_term/2 etc) as possibly the slot space was
reallocated.

A collection of slots constitute an *environment* and belong to a frame.


Choices
=======

A choice is an element in the choice stack. There a number of choice
types (really should be called a control point) and it contains the
index of the highest heap, slot & trail used at this point. On
backtracking excess space can be freed. It also contains the index
number of the frame which created it and a record of the frame state
(nbr of vars etc) at the time the choice was created. On backtracking
vars (slots space) can be trimmed back (if possible) and the frame
state is restored. It also contains flags related to managing cuts &
call cleanup etc.

Apart from basic choice points other types of contol points are
barriers and catchers.

One of the most important properties of a choice point is the choice
generation signifiying when it was created. A cut will drop choice
points with a generation more recent than the frame it originated in.

A barrier temporarily creates a choice to block cuts, this is used by
call/n and friends. When a barrier is dropped the choice is dropped.


Trail
=====

... TODO ...


Heap
====

A space for dynamically created terms (compounds). Heap space is
allocated in variable-sized pages as a linked list.

A term allocated on the heap must be fully contained within one page,
to this end terms are first built in a temporary space and copied
into a suitably sized page.

Excess heap pages may be freed on backtracking or with cuts.

Non-compiled control structures are built on the heap and called. This
space can be freed at the call return if no choice points are active.
