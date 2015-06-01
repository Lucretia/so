# Oberon-0 grammar

~~~ebnf
ident = letter {letter | digit}.

integer = digit {digit}.

selector = {"." ident | "[" expression "]"}.

factor = ident selector | integer | "(" expression ")" | "~" factor.

term = factor {("*" | "DIV" | "MOD" | "&") factor}.

SimpleExpression = ["+"|"-"] term {("+"|"-" | "OR") term}.

expression = SimpleExpression
[("=" | "#" | "<" | "<=" | ">" | ">=") SimpleExpression].

assignment = ident selector ":=" expression.

ActualParameters = "(" [expression {"," expression}] ")" .

ProcedureCall = ident [ActualParameters].

IfStatement = "IF" expression "THEN" StatementSequence
{"ELSIF" expression "THEN" StatementSequence}
["ELSE" StatementSequence] "END".

WhileStatement = "WHILE" expression "DO" StatementSequence "END".

statement = [assignment | ProcedureCall | IfStatement | WhileStatement].

StatementSequence = statement {";" statement}.

IdentList = ident {"," ident}.

ArrayType = "ARRAY" expression "OF" type.

FieldList = [IdentList ":" type].

RecordType = "RECORD" FieldList {";" FieldList} "END".

type = ident | ArrayType | RecordType.

FPSection = ["VAR"] IdentList ":" type.

FormalParameters = "(" [FPSection {";" FPSection}] ")".

ProcedureHeading = "PROCEDURE" ident [FormalParameters].

ProcedureBody = declarations ["BEGIN" StatementSequence] "END".

ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.

declarations = ["CONST" {ident "=" expression ";"}]
["TYPE" {ident "=" type ";"}]
["VAR" {IdentList ":" type ";"}]
{ProcedureDeclaration ";"}.

module = "MODULE" ident ";" declarations
["BEGIN" StatementSequence] "END" ident "." .
~~~

# Symbol tables

~~~ebnf
SymFile = BEGIN key {name key} imported modules
[ CONST {type name value} ] [ VAR {type name} ] constants and variables
[ PROC {type name {[VAR] type name} END} ] procedures, parameters
[ ALIAS {type name} ] [ NEWTYP {type} ] END . renamed procedures

type = basicType | [Module] OldType | NewType.

basicType = BOOL | CHAR | INTEGER | REAL | ...

NewType = ARRAY type name intval | DYNARRAY type name | POINTER type name
| RECORD type name {type name} END record types and fields
| PROCTYP type name {[VAR] type name END . procedure types and parameters
~~~
