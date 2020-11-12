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
      ';'                { T'StatementEnd }
      identifier         { T'Identifier $$ }
      bool               { T'Bool $$ }
%%

select_statement
    : SELECT select_body FROM from_body WHERE where_body ';'   { SelectStatement $2 $4 $6}

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
    | table_name AS alias       { TableNameAlias $1 (Just $3)  }

where_body
    : bool                      { WhereBody $1 }

column_name : identifier        { $1 }
table_name : identifier         { $1 }
alias : identifier              { $1 }
