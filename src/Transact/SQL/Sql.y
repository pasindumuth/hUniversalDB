{
module Transact.SQL.Sql where

import Transact.Model.SqlAST
import Transact.SQL.Parse
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      SELECT             { T'Select }
      FROM               { T'From }
      WHERE              { T'Where }
      AS                 { T'As }
      ','                { T'Comma }
      '.'                { T'Dot }
      ';'                { T'SemiColon }
      '('                { T'LeftParen }
      ')'                { T'RightParen }
      'AND'              { T'AndOp }
      'OR'               { T'OrOp }
      compare_op         { T'CompareOp $$ }
      identifier         { T'Identifier $$ }
      quoted_string      { T'QuotedString $$ }
      bool               { T'Bool $$ }
      int                { T'Int $$ }
      float              { T'Float $$ }
%%

select_statement
    : SELECT select_body FROM from_body WHERE where_body ';'   { SelectStatement $2 $4 $6 }

select_body
    : column_names              { SelectBody (reverse $1) }

column_names
    : column_name_alias                     { [$1] }
    | column_names ',' column_name_alias    { ($3:$1) }

column_name_alias
    : column_name               { ColumnNameAlias $1 Nothing }
    | column_name AS alias      { ColumnNameAlias $1 (Just $3) }

from_body
    : table_name_alias          { FromBody $1 }

table_name_alias
    : table_name                { TableNameAlias $1 Nothing }
    | table_name AS alias       { TableNameAlias $1 (Just $3) }

where_body
    : bool_disjunction_expr                 { WhereBody $1 }

bool_disjunction_expr
    : bool_disjunction_expr_list            { BoolDisjunction (reverse $1) }

bool_disjunction_expr_list
    : bool_conjunction_expr                                    { [$1] }
    | bool_disjunction_expr_list 'OR' bool_conjunction_expr    { ($3:$1) }

bool_conjunction_expr
    : bool_conjunction_expr_list            { BoolConjunction (reverse $1) }

bool_conjunction_expr_list
    : bool_expr_atom                                           { [$1] }
    | bool_conjunction_expr_list 'AND' bool_expr_atom          { ($3:$1) }

bool_expr_atom
    : '(' bool_disjunction_expr ')'         { SubBoolDisjunction $2 }
    | value_atom compare_op value_atom      { CompareExpr $1 $2 $3 }
    | bool                                  { SingleBoolValue $1 }

value_atom
    : quoted_string             { StringValue $1 }
    | bool                      { BoolValue $1 }
    | int                       { IntValue $1 }
    | float                     { FloatValue $1 }
    | column_name               { ColumnName $1 }
    | alias '.' column_name     { QualifiedColumnName $1 $3 }

column_name : identifier        { $1 }
table_name : identifier         { $1 }
alias : identifier              { $1 }
