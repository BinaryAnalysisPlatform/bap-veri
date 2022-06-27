# bil-verification

A central goal of this tool is to find errors in our BIL code. So this tool
compares results of instructions execution in trace with execution of BIL code,
that describes this instructions. Verification is based on comparison of two
set of events. First one is a real events set, that came from trace. And second
one is a set, that was filled during execution of `code_exec` event, by
emitting artificial events.

##Build and install
```
oasis setup
make
make install
```
##Policy

There is an ideal case when all events from trace and all events from
BIL are equal to each other. But in practice both trace and bil could
contain some special cases. For example, trace could be obtained from
source, that does not provide some cpu flags, or bil does not support
some instructions.  And we possible may want to shadow this cases and
continue verification process. From other point of view, we do not want
to miss errors. So for this reasons tool supports policy, that is a set
of rules with the following grammar.

Each rule consists of 4 fields: `ACTION INSN L_EVENT R_EVENT`

1. `ACTION`  field could be either `SKIP`, either `DENY`. If we have processed
   trace without matching with any `DENY`, then everything is ok.
2. `INSN`    field could contain an instruction name like `MOV64rr` or regular
   expression, like `MOV.*`
3. `L_EVENT` field, left hand-side event, corresponds to textual representation
   of tracer events, and could contain any string and regualar expression.
4. `R_EVENT` field, right hand-side event, corresponds to textual representation
   of lifter events, and could contain any string and regualar expression.

Matching is performed textually, based on event syntax. Regexp syntax supports
backreferences in event fields. Only that events, that don't have an equal
pair in other set goes to this matching.

Rules could be written in text file, that will be passed as argument through
command line. Syntax is a pretty simple. Each row either contains a rule,
either commented with `#` symbol, either empty. Rule must have exactly
4 fields. An empty field must be written as `''` or `""`. Fields with spaces
must be written in quotes: `"RAX => .*"`, single quotes also supported:
`'RAX => .*'`.

###Examples

For example, let's imagine that a tracer doesn't support read from zero
flag, so all read events from zero flag in bil code will be unmatched.
So we are able to create next rule :
`SKIP .* '' 'ZF -> .*'`
This could be read as: for any instruction skip unmatched zero flag
reading in bil code.

Next two rules means that that no one should be left without a pair.
```
DENY .* .* ''
DENY .* '' .*
```
That does mean that for any instruction unmatched left/right event is an error.
This pair is also a default behavior in case when there wasn't any policy file
given through a command line.

Also we can use this policy to check incomplete lifter, for example we can say:
```
DENY MOV.* .* ''
DENY MOV.* '' .*
```
And check only move instructions.

Backreferenses example: `DENY .* '(.F) <= .*' '\1 <= .*'`, that could be read
as: for any instruction complain if a value written in some flag in tracer is
differrent from value written in lifter for the same flag. And values are
really different, since only not equal events goes to matching.

##Usage
Program works only with files with `.frames` extension.
```
bap veri --show-errors --show-stat --rules "path to rules file" PATH

`PATH` is either directory with files from a tracer, either a file.
`show-errors` option allows to see a detailed information about BIL errors
`show-stat` option allows to see a summary over a trace verification.

```
