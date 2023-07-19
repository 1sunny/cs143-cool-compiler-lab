/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the *** last *** INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    
    /* You will want to change the following line. */
    %type <features> feature_list
    %type <formals> formal_list
    %type <formals> trivial_formal_list

    %type <expression> expr

    %type <expressions> actual_params
    %type <expressions> trivial_params

    %type <expressions> expr_list
    %type <expressions> expr_list_or_empty

    %type <expression> trivial_let

    %type <cases> case_branch_list
    %type <case_> case_branch

    /* Precedence declarations go here. */
    /* MY NOTE:
     * The directive %nonassoc creates run-time error: using the operator in a associative way is a syntax error.
     * The directive %precedence creates compile-time errors: an operator can be involved in an associativity-related conflict, contrary to what expected the grammar author.
     * Not all rules and not all tokens have precedence. If either the rule or the lookahead token has no precedence, then the default is to shift.
    */
    %right ASSIGN
    %nonassoc NOT

    %nonassoc LOWER_THAN_OP

    %nonassoc LE '<' '='
    %left '+' '-'
    %left '*' '/'
    %nonassoc ISVOID
    %nonassoc '~'
    %nonassoc '@'
    %nonassoc '.'

    %nonassoc THEN
    %nonassoc ELSE
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list :
      class {
        if (omerrs == 0) {
          $$ = single_Classes($1);
          parse_results = $$;
        }
      }
    | class_list class {
        if (omerrs == 0) {
          $$ = append_Classes($1,single_Classes($2));
          parse_results = $$;
        }
      }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	:
      CLASS TYPEID '{' feature_list '}' ';' {
        if (omerrs == 0) {
          $$ = class_($2,idtable.add_string("Object"),$4,stringtable.add_string(curr_filename));
        }
      }
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';' {
        if (omerrs == 0) {
          $$ = class_($2,$4,$6,stringtable.add_string(curr_filename));
        }
      }
    | CLASS error INHERITS TYPEID '{' feature_list '}' ';' {}
    | CLASS TYPEID error TYPEID '{' feature_list '}' ';' {}
    | CLASS TYPEID INHERITS error '{' feature_list '}' ';' {}
    | CLASS TYPEID INHERITS TYPEID '{' error '}' ';' {}
    ;

    feature_list :
      {
        if (omerrs == 0) {
          $$ = nil_Features();
        }
      }
    | OBJECTID '(' formal_list ')' ':' TYPEID '{' expr '}' ';' feature_list {
        if (omerrs == 0) {
          if ($11->len() == 0)
            $$ = single_Features(method($1, $3, $6, $8));
          else
            $$ = append_Features(single_Features(method($1, $3, $6, $8)), $11);
        }
      }
    | error '(' formal_list ')' ':' TYPEID '{' expr '}' ';' feature_list {}
    | OBJECTID '(' error ')' ':' TYPEID '{' expr '}' ';' feature_list {}
    | OBJECTID '(' formal_list ')' ':' error '{' expr '}' ';' feature_list {}
    | OBJECTID '(' formal_list ')' ':' TYPEID '{' error '}' ';' feature_list {}
    | OBJECTID ':' TYPEID ';' feature_list {
        if (omerrs == 0) {
          if ($5->len() == 0)
            $$ = single_Features(attr($1, $3, no_expr()));
          else
            $$ = append_Features(single_Features(attr($1, $3, no_expr())), $5);
        }
      }
    | error ':' TYPEID ';' feature_list {}
    | OBJECTID ':' error ';' feature_list {}
    | OBJECTID ':' TYPEID ASSIGN expr ';' feature_list {
        if (omerrs == 0) {
          if ($7->len() == 0)
            $$ = single_Features(attr($1, $3, $5));
          else
            $$ = append_Features(single_Features(attr($1, $3, $5)), $7);
        }
      }
    | error ':' TYPEID ASSIGN expr ';' feature_list {}
    | OBJECTID ':' error ASSIGN expr ';' feature_list {}
    | OBJECTID ':' TYPEID error expr ';' feature_list {}
    | OBJECTID ':' TYPEID ASSIGN error ';' feature_list {}
    ;
    
    formal_list :
      {
        if (omerrs == 0) {
          $$ = nil_Formals();
        }
      }
    | OBJECTID ':' TYPEID trivial_formal_list {
        if (omerrs == 0) {
          if ($4->len() == 0)
            $$ = single_Formals(formal($1, $3));
          else
            $$ = append_Formals(single_Formals(formal($1, $3)), $4);
        }
      }
    ;

    trivial_formal_list :
      {
        if (omerrs == 0) {
          $$ = nil_Formals();
        }
      }
    | ',' OBJECTID ':' TYPEID trivial_formal_list {
        if (omerrs == 0) {
          if ($5->len() == 0)
            $$ = single_Formals(formal($2, $4));
          else
            $$ = append_Formals(single_Formals(formal($2, $4)), $5);
        }
      }
    ;

    /* MY NOTE:
     * Let 表达式冲突
     * expr '.' OBJECTID '(' . actual_params ')' {
         $$ = dispatch($1, $3, $5);
       }
     * 15 expr: %empty
     * 42 actual_params: %empty
     *  actual_params :
          {
            $$ = nil_Expressions();
          }
        | expr trivial_params {
            if ($2->len() == 0)
              $$ = single_Expressions($1);
            else
              $$ = append_Expressions(single_Expressions($1), $2);
          }
        ;
     * 存在冲突
     * ')'       reduce using rule 15 (expr)
     * ')'       [reduce using rule 42 (actual_params)]
     * 解决方法
     * expr不能为空
     */
    expr :
      /* ID <- expr */
      OBJECTID ASSIGN expr {
        if (omerrs == 0) {
          $$ = assign($1, $3);
        }
      }
    | expr '.' OBJECTID '(' actual_params ')' {
        if (omerrs == 0) {
          $$ = dispatch($1, $3, $5);
        }
      }
    | expr '.' error '(' actual_params ')' {}
    | expr '@' TYPEID '.' OBJECTID '(' actual_params ')' {
        if (omerrs == 0) {
          $$ = static_dispatch($1, $3, $5, $7);
        }
      }
    | expr '@' error '.' OBJECTID '(' actual_params ')' {}
    | expr '@' TYPEID '.' error '(' actual_params ')' {}
    | OBJECTID '(' actual_params ')' {
        if (omerrs == 0) {
          $$ = dispatch(object(idtable.add_string("self")), $1, $3);
        }
      }
    | IF expr THEN expr ELSE expr FI {
        if (omerrs == 0) {
          $$ = cond($2, $4, $6);
        }
      }
    | WHILE expr LOOP expr POOL {
        if (omerrs == 0) {
          $$ = loop($2, $4);
        }
      }
    | '{' expr_list '}' {
        if (omerrs == 0) {
          $$ = block($2);
        }
      }
    | LET OBJECTID ':' TYPEID trivial_let {
        if (omerrs == 0) {
          $$ = let($2, $4, no_expr(), $5);
        }
      }
    | LET error ':' TYPEID trivial_let {}
    | LET OBJECTID ':' error trivial_let {}
    | LET OBJECTID ':' TYPEID ASSIGN expr trivial_let {
        if (omerrs == 0) {
          $$ = let($2, $4, $6, $7);
        }
      }
    | LET error ':' TYPEID ASSIGN expr trivial_let {}
    | LET OBJECTID ':' error ASSIGN expr trivial_let {}
    | LET OBJECTID ':' TYPEID error expr trivial_let {}
    | CASE expr OF case_branch case_branch_list ESAC {
        if (omerrs == 0) {
          if ($5->len() == 0)
            $$ = typcase($2, single_Cases($4));
          else
            $$ = typcase($2, append_Cases(single_Cases($4), $5));
        }
      }
    | NEW TYPEID {
        if (omerrs == 0) {
          $$ = new_($2);
        }
      }
    | ISVOID expr {
        if (omerrs == 0) {
          $$ = isvoid($2);
        }
      }
    | expr '+' expr {
        if (omerrs == 0) {
          $$ = plus($1, $3);
        }
      }
    | expr '-' expr {
        if (omerrs == 0) {
          $$ = sub($1, $3);
        }
      }
    | expr '*' expr {
        if (omerrs == 0) {
          $$ = mul($1, $3);
        }
      }
    | expr '/' expr {
        if (omerrs == 0) {
          $$ = divide($1, $3);
        }
      }
    | '~' expr {
        if (omerrs == 0) {
          $$ = neg($2);
        }
      }
    | expr '<' expr {
        if (omerrs == 0) {
          $$ = lt($1, $3);
        }
      }
    | expr LE expr {
        if (omerrs == 0) {
          $$ = leq($1, $3);
        }
      }
    | expr '=' expr {
        if (omerrs == 0) {
          $$ = eq($1, $3);
        }
      }
    | NOT expr {
        if (omerrs == 0) {
          $$ = comp($2);
        }
      }
    | '(' expr ')' {
        if (omerrs == 0) {
          $$ = $2;
        }
      }
    | OBJECTID {
        if (omerrs == 0) {
          $$ = object($1);
        }
      }
    | INT_CONST {
        if (omerrs == 0) {
          $$ = int_const($1);
        }
     }
    | STR_CONST {
        if (omerrs == 0) {
          $$ = string_const($1);
        }
      }
    | BOOL_CONST {
        if (omerrs == 0) {
          $$ = bool_const($1);
        }
      }
    ;

    actual_params :
      {
        if (omerrs == 0) {
          $$ = nil_Expressions();
        }
      }
    | expr trivial_params {
        if (omerrs == 0) {
          if ($2->len() == 0)
            $$ = single_Expressions($1);
          else
            $$ = append_Expressions(single_Expressions($1), $2);
        }
      }
    ;

    trivial_params :
      {
        if (omerrs == 0) {
          $$ = nil_Expressions();
        }
      }
    | ',' expr trivial_params {
        if (omerrs == 0) {
          if ($3->len() == 0)
            $$ = single_Expressions($2);
          else
            $$ = append_Expressions(single_Expressions($2), $3);
        }
      }
    ;

    expr_list :
      expr ';' expr_list_or_empty {
        if (omerrs == 0) {
          if ($3->len() == 0)
            $$ = single_Expressions($1);
          else
            $$ = append_Expressions(single_Expressions($1), $3);
        }
      }
    | error ';' expr_list_or_empty {}
    ;

    expr_list_or_empty : {
        if (omerrs == 0) {
          $$ = nil_Expressions();
        }
      }
    | expr ';' expr_list_or_empty {
        if (omerrs == 0) {
          if ($3->len() == 0)
            $$ = single_Expressions($1);
          else
            $$ = append_Expressions(single_Expressions($1), $3);
        }
      }
    | error ';' expr_list_or_empty {}
    ; 

    trivial_let :
      IN expr %prec LOWER_THAN_OP {
        if (omerrs == 0) {
          $$ = $2;
        }
      }
    | ',' OBJECTID ':' TYPEID trivial_let {
        if (omerrs == 0) {
          $$ = let($2, $4, no_expr(), $5);
        }
      }
    | ',' OBJECTID ':' TYPEID ASSIGN expr trivial_let {
        if (omerrs == 0) {
          $$ = let($2, $4, $6, $7);
        }
      }
    ;

    case_branch_list :
      {
        if (omerrs == 0) {
          $$ = nil_Cases();
        }
      }
    | case_branch case_branch_list {
        if (omerrs == 0) {
          if ($2->len() == 0)
            $$ = single_Cases($1);
          else
            $$ = append_Cases(single_Cases($1), $2);
        }
      }
    ;

    case_branch :
      OBJECTID ':' TYPEID DARROW expr ';' {
        if (omerrs == 0) {
          $$ = branch($1, $3, $5);
        }
      }
    ;  

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;

      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    