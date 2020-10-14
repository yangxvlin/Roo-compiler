/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_OZYY_POZ_H_INCLUDED
# define YY_OZYY_POZ_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ozyydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INSTR_PUSH_STACK_FRAME = 258,
    INSTR_POP_STACK_FRAME = 259,
    INSTR_LOAD = 260,
    INSTR_STORE = 261,
    INSTR_LOAD_ADDRESS = 262,
    INSTR_LOAD_INDIRECT = 263,
    INSTR_STORE_INDIRECT = 264,
    INSTR_INT_CONST = 265,
    INSTR_REAL_CONST = 266,
    INSTR_STRING_CONST = 267,
    INSTR_ADD_INT = 268,
    INSTR_ADD_REAL = 269,
    INSTR_ADD_OFFSET = 270,
    INSTR_SUB_INT = 271,
    INSTR_SUB_REAL = 272,
    INSTR_SUB_OFFSET = 273,
    INSTR_MUL_INT = 274,
    INSTR_MUL_REAL = 275,
    INSTR_DIV_INT = 276,
    INSTR_DIV_REAL = 277,
    INSTR_NEG_INT = 278,
    INSTR_NEG_REAL = 279,
    INSTR_CMP_EQ_INT = 280,
    INSTR_CMP_NE_INT = 281,
    INSTR_CMP_GT_INT = 282,
    INSTR_CMP_GE_INT = 283,
    INSTR_CMP_LT_INT = 284,
    INSTR_CMP_LE_INT = 285,
    INSTR_CMP_EQ_REAL = 286,
    INSTR_CMP_NE_REAL = 287,
    INSTR_CMP_GT_REAL = 288,
    INSTR_CMP_GE_REAL = 289,
    INSTR_CMP_LT_REAL = 290,
    INSTR_CMP_LE_REAL = 291,
    INSTR_CMP_EQ_STRING = 292,
    INSTR_CMP_NE_STRING = 293,
    INSTR_CMP_GT_STRING = 294,
    INSTR_CMP_GE_STRING = 295,
    INSTR_CMP_LT_STRING = 296,
    INSTR_CMP_LE_STRING = 297,
    INSTR_AND = 298,
    INSTR_OR = 299,
    INSTR_NOT = 300,
    INSTR_BRANCH_UNCOND = 301,
    INSTR_BRANCH_ON_TRUE = 302,
    INSTR_BRANCH_ON_FALSE = 303,
    INSTR_CALL = 304,
    INSTR_CALL_BUILTIN = 305,
    INSTR_RETURN = 306,
    INSTR_INT_TO_REAL = 307,
    INSTR_MOVE = 308,
    INSTR_DEBUG_REG = 309,
    INSTR_DEBUG_SLOT = 310,
    INSTR_DEBUG_STACK = 311,
    INSTR_HALT = 312,
    FUNC_READ_INT = 313,
    FUNC_READ_REAL = 314,
    FUNC_READ_BOOL = 315,
    FUNC_READ_STRING = 316,
    FUNC_PRINT_INT = 317,
    FUNC_PRINT_REAL = 318,
    FUNC_PRINT_BOOL = 319,
    FUNC_PRINT_STRING = 320,
    FUNC_PRINT_NEWLINE = 321,
    FUNC_STRING_CONCAT = 322,
    FUNC_STRING_LENGTH = 323,
    FUNC_SUBSTRING = 324,
    FUNC_SQRT = 325,
    FUNC_TRUNC = 326,
    FUNC_ROUND = 327,
    COMMA = 328,
    COLON = 329,
    TOKEN_REG = 330,
    TOKEN_ID = 331,
    NAT_CONST = 332,
    INT_CONST = 333,
    REAL_CONST = 334,
    STRING_CONST = 335,
    GARBAGE = 336
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 20 "poz.y" /* yacc.c:1909  */

	char		*Ustr;
	int		Uint;
	bool		Ubool;
	float		Ureal;
	Function	Ufunc;

#line 144 "poz.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE ozyylval;

int ozyyparse (void);

#endif /* !YY_OZYY_POZ_H_INCLUDED  */
