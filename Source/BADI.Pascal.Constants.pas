(**

  This module contains all the Object Pascal specific constants used by the parser and its
  sub-classes.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.Constants;

Interface

Uses
  BADI.Types;

Const
  (** A set of characters for alpha characaters **)
  strTokenChars : Set Of AnsiChar = ['#', '_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of AnsiChar = ['$', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '-', '.', '/', ':',
    ';', '<', '=', '>', '@', '[', ']', '^'];
  (** A set of characters for quotes **)
  strQuote : Set Of AnsiChar = [''''];
  (** A string representing the Array Of parameter type. **)
  strArrayOf : Array[False..True] Of String = ('', 'Array Of ');

  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[0..73] Of String = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else',
    'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for',
    'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    'nil', 'not', 'object', 'of', 'on', 'or', 'out', 'packed',
    'private', 'procedure', 'program', 'property', 'protected', 'public', 'published',
    'raise', 'record', 'reference', 'repeat',
    'resourcestring', 'sealed', 'set', 'shl', 'shr', 'strict', 'string',
    'then', 'threadvar', 'to', 'try', 'type', 'unit', 'unsafe', 'until', 'uses',
    'var', 'while', 'with', 'xor'
  );
  (** A list of reserved words which are allows to be used as identifiers. **)
  strIdentifierReservedWords : Array[0..4] Of String = (
    'on', 'out', { 'private', 'protected', 'public', 'published', } 'reference', 'sealed',
    { 'strict', } 'unsafe'
  );
  (** A sorted list of directives. Used for identifying tokens as
  directives. **)
  strDirectives : Array[0..44] Of String = (
    'absolute', 'abstract', 'assembler', 'automated', 'cdecl', 'contains',
    'default', 'deprecated', 'dispid', 'dynamic', 'export', 'external', 'far',
    'final', 'forward', 'helper', 'implements', 'index', 'inline', 'library', 'local',
    'message', 'name', 'near', 'nodefault', 'operator', 'overload', 'override', 'package',
    'pascal', 'platform', 'read',
    'readonly', 'register', 'reintroduce', 'requires', 'resident', 'safecall',
    'static', 'stdcall', 'stored', 'varargs', 'virtual', 'write', 'writeonly'
  );

  (** A list of string representing the types of modules. **)
  strModuleTypes : Array[mtProgram..mtUnit] Of String = ('Program', 'Package',
    'Library', 'Unit');
  (** A list of strings representing the stricted scope types. **)
  strStrictedScope : Array[scPrivate..scProtected] Of String = ('private',
    'protected');
  (** A list of strings representing the scope types. **)
  strScope : Array[scGlobal..scPublished] Of String = ('global', 'local',
    'private', 'protected', 'public', 'published');
  (** A sorted list of method directives. Used in identifying method
  directives. **)
  strMethodDirectives : Array[1..24] Of String = (
    'abstract', 'assembler', 'cdecl', 'dispid', 'dynamic', 'export',
    'external', 'far', 'final', 'forward',  'inline', 'local', 'message',
    'near', 'overload', 'override', 'pascal', 'register', 'reintroduce',
    'safecall', 'static', 'stdcall',  'varargs', 'virtual'
  );
  (** A list of real types. **)
  strRealTypes : Array[1..7] Of String = ('comp', 'currency',
    'double', 'extended', 'real', 'real48', 'single');
  (** A list of ordinal idents **)
  strOrdIdents : Array[1..13] Of String = ('boolean', 'byte', 'cardinal', 'char',
    'int64', 'integer', 'longint', 'longword', 'pchar', 'shortint', 'smallint',
    'widechar', 'word');
  (** A list of variants **)
  strVariants : Array[1..2] Of String = ('olevariant', 'variant');
  (** A list of string types. **)
  strStrings  : Array[1..4] Of String = ('ansistring', 'shortstring', 'string',
    'widestring');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..31] Of String = (';', 'const', 'contains',
    'do', 'else', 'end', 'except', 'exports', 'finalization', 'finally',
    'function', 'implementation',
    'implements', 'initialization', 'interface', 'label', 'library', 'object',
    'package', 'procedure', 'program', 'record', 'requires', 'resourcestring',
    'static', 'then', 'type', 'unit', 'until', 'uses', 'var'
  );

  (** This is a list of functions which can be used in a const expression. **)
  strConstExprDesignators : Array[1..17] Of String = ('abs', 'byte', 'chr', 'hi',
    'high', 'integer', 'length', 'lo', 'low', 'odd', 'ord', 'pred', 'round',
    'sizeof', 'succ', 'swap', 'trunc');
  (** A list of Rel operators for expressions. **)
  strRelOps : Array[1..9] Of String = ('<', '<=', '<>', '=', '>', '>=', 'as',
    'in', 'is');
  (** A list of Add operators for expressions. **)
  strAddOps : Array[1..4] Of String = ('+', '-', 'or', 'xor');
  (** A list of Multiplier operators for expressions. **)
  strMulOps : Array[1..7] Of String = ('*', '/', 'and', 'div', 'mod', 'shl', 'shr');
  (** A list of portability directives. **)
  strPortabilityDirective : Array[1..3] Of String = ('deprecated', 'library',
    'platform');
  (** A list of strings representing the types of methods. **)
  strMethodTypes : Array[Low(TMethodType)..High(TMethodType)] Of String = (
    'Constructor', 'Destructor', 'Procedure', 'Function', 'Operator');
  (** A list of strings representing the parameter modifiers for methods. **)
  strParamModifier : Array[pamNone..pamOut] Of String = ('', 'var ', 'const ',
    'out ');
  (** A list of the valid operator names. **)
  strOperatorList : Array[1..31] Of String = (
    'add',
    'bitwiseand',
    'bitwisenot',
    'bitwiseor',
    'bitwisexor',
    'dec',
    'divide',
    'equal',
    'explicit',
    'greaterthan',
    'greaterthanorequal',
    'implicit',
    'in',
    'inc',
    'intdivide',
    'leftshift',
    'lessthan',
    'lessthanorequal',
    'logicaland',
    'logicalnot',
    'logicalor',
    'logicalxor',
    'modulus',
    'multiply',
    'negative',
    'notequal',
    'positive',
    'rightshift',
    'round',
    'subtract',
    'trunc'
  );

Implementation

End.
