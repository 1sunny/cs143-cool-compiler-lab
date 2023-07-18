/*
 *  The scanner definition for COOL.
 */

/*
* nodefault使flex不要添加默认规则,当输入无法被给定的规则完全匹配时,词法分析器可以报告一个错误
* 现在大多flex程序使用 "%option noyywrap" 来禁用yywrap,然后写出自己的主程序,所以它们不需要-lfl库
*/
%option nodefault noyywrap

%x StateNestedComment
%x StateLineComment
%x StateString
%x StateFindStringEnd
/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 * 出于最大灵活性考虑,你可以重新定义flex用于读取输入到当前缓冲区的宏
 * 每当词法分析器的输入缓冲区为空时,就会调用YY_INPUT
 * result是一个宏,所以使用result而不是result*
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
    if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
        YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;
int string_buf_index = 0;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int stringtable_index = 0;
int n_comment_open = 0;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>

ASSIGN          <-

LE              <=

%%

 /*
  *  One-line comments
  */
"--" { BEGIN(StateLineComment); }
<StateLineComment>.*
<StateLineComment>\n {
  curr_lineno++;
  BEGIN(INITIAL);
}

 /*
  *  Nested comments
  */
"(*" |
<StateNestedComment>"(*" {
  n_comment_open++;
  BEGIN(StateNestedComment);
}
<StateNestedComment>"*)" {
  n_comment_open--;
  if (n_comment_open == 0) {
    BEGIN(INITIAL);
  }
}
<StateNestedComment>.
<StateNestedComment>\n { curr_lineno++; }
<StateNestedComment><<EOF>> {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN(INITIAL);
  return (ERROR);
}

"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}

 /*
  *  The multiple-character operators.
  */
{DARROW}        { return (DARROW); }

{ASSIGN}        { return (ASSIGN); }

{LE}            { return (LE); }

 /*
  *  Special syntactic symbols
  */
";" { return (';'); }

"{" { return ('{'); }

"}" { return ('}'); }

"(" { return ('('); }

"," { return (','); }

")" { return (')'); }

":" { return (':'); }

"@" { return ('@'); }

"." { return ('.'); }

"+" { return ('+'); }

"-" { return ('-'); }

"*" { return ('*'); }

"/" { return ('/'); }

"~" { return ('~'); }

"<" { return ('<'); }

"=" { return ('='); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  * 关键字不区分大小写,除了 true 和 false 之外,它们必须以小写字母开头
  * To conform to the rules for other objects, the first letter of true and false must be lowercase; * the trailing letters may be upper or lower case.
  */
(?i:CLASS) { return (CLASS); }

(?i:ELSE) { return (ELSE); }

(?i:IF) { return (IF); }

(?i:FI) { return (FI); }

(?i:IN) { return (IN); }

(?i:INHERITS) { return (INHERITS); }

(?i:LET) { return (LET); }

(?i:LOOP) { return (LOOP); }

(?i:POOL) { return (POOL); }

(?i:THEN) { return (THEN); }

(?i:WHILE) { return (WHILE); }

(?i:CASE) { return (CASE); }

(?i:ESAC) { return (ESAC); }

(?i:OF) { return (OF); }

(?i:NEW) { return (NEW); }

(?i:ISVOID) { return (ISVOID); }

(?i:NOT) { return (NOT); }

 /*
  * Boolean
  * To conform to the rules for other objects, the first letter of true and false must be lowercase
  */
t(?i:rue) {
  cool_yylval.boolean = 1;
  return (BOOL_CONST);
}

f(?i:alse) {
  cool_yylval.boolean = 0;
  return (BOOL_CONST);
}


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
"\"" {
  string_buf_index = 0;
  BEGIN(StateString);
}
<StateString>"\n" {
  curr_lineno++;
  cool_yylval.error_msg = "Unterminated string constant";
  BEGIN(INITIAL);
  return (ERROR);
}
<StateString>"\0" {
  cool_yylval.error_msg = "String contains null character.";
  // resume after the end of the string
  BEGIN(StateFindStringEnd);
  return (ERROR);
}
<StateFindStringEnd>"\"" {
  BEGIN(INITIAL);
}
<StateFindStringEnd>"\n" {
  curr_lineno++;
  BEGIN(INITIAL);
}
<StateFindStringEnd>. 

<StateString>"\\"(.|\n) {
  if (string_buf_index > MAX_STR_CONST - 1 - 1) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN(StateFindStringEnd);
    return (ERROR);
  }
  char now = yytext[1];
  if (now == 'b') {
    string_buf[string_buf_index++] = '\b';
  } else if (now == 't') {
    string_buf[string_buf_index++] = '\t';
  } else if (now == 'n') {
    string_buf[string_buf_index++] = '\n';
  } else if (now == 'f') {
    string_buf[string_buf_index++] = '\f';
  } else if (now == '\0') {
    BEGIN(StateFindStringEnd);
    cool_yylval.error_msg = "String contains escaped null character.";
    return (ERROR);
  } else {
    if (now == '\n') {
      curr_lineno++;
    }
    string_buf[string_buf_index++] = now;
  }
}
<StateString>"\"" {
  string_buf[string_buf_index] = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  BEGIN(INITIAL);
  return (STR_CONST);
}
<StateString><<EOF>> {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  return (ERROR);
}
<StateString>. {
  if (string_buf_index > MAX_STR_CONST - 1 - 1) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN(StateFindStringEnd);
    return (ERROR);
  }
  string_buf[string_buf_index++] = yytext[0];
}

 /*
  * Integers, non-empty strings of digits 0-9
  */
[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}

 /*
  * Type Identifiers, begin with a capital letter
  */
[A-Z][_a-zA-z0-9]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}

 /*
  * Object Identifiers, begin with a lower case letter
  */
[a-z][_a-zA-z0-9]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}

 /*
  * White Space
  */
[ \f\r\t\v]+

[\n] {
  curr_lineno++;
}

 /*
  * Unknown
  */
.|\n {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}

<<EOF>> {
  return 0;
}
%%
