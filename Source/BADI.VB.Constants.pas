(**

  This module contains a set of constants for use with the VB parser.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit BADI.VB.Constants;

Interface

Uses
  BADI.Types,
  BADI.VB.Types;

Const
  (** A set of characters for alpha characters **)
  strTokenChars : Set Of AnsiChar = ['_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of AnsiChar = ['&', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', '%', '$', '&',
    ',', '-', '.', '/', ':', ';', '<', '=', '>', '@', '[', ']', '^', '{', '}'];
  (** A set of characters for quotes **)
  strQuote : Set Of AnsiChar = ['"'];
  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[1..138] Of String = (
    'addhandler', 'addressof', 'alias', 'and', 'andalso', 'ansi', 'as', 'assembly',
    'auto', 'base', 'boolean', 'byref', 'byte', 'byval', 'call', 'case', 'catch',
    'cbool', 'cbyte', 'cchar', 'cdate', 'cdbl', 'cdec', 'char', 'cint', 'class',
    'clng', 'cobj', 'compare', 'const', 'cshort', 'csng', 'cstr', 'ctype', 'date',
    'decimal', 'declare', {'default', }'delegate', 'dim', 'directcast', 'do', 'double',
    'each', 'else', 'elseif', 'end', 'enum', 'erase', 'error', 'event', 'exit',
    'explicit', {'false', }'finally', 'for', 'friend', 'function', 'get', 'gettype',
    'gosub', 'goto', 'handles', 'if', 'implements', 'imports', 'in', 'inherits',
    'integer', 'interface', 'is', 'let', 'lib', 'like', 'long', 'loop', 'me', 'mod',
    'module', 'mustinherit', 'mustoverride', 'mybase', 'myclass', 'namespace', 'new',
    'next', 'not', 'nothing', 'notinheritable', 'notoverridable', 'object', 'on',
    'option', 'optional', 'or', 'orelse', 'overloads', 'overridable', 'overrides',
    'paramarray', 'preserve', 'private', 'property', 'protected', 'public', 'raiseevent',
    'readonly', 'redim', 'rem', 'removehandler', 'resume', 'return', 'select', 'set',
    'shadows', 'shared', 'short', 'single', 'static', 'step', 'stop', 'string',
    'structure', 'sub', 'synclock', 'then', 'throw', 'to', {'true', }'try', 'type',
    'typeof', 'unicode', 'until', 'variant', 'when', 'while', 'with', 'withevents',
    'writeonly', 'xor'
  );

  (** A list of directives. **)
  strDirectives : Array[1..3] Of String = (
    'default', 'false', 'true'
  );

  (** A set of token to be searched for after an error. **)
  strSeekTokens : Array[1..6] Of String = (
    'const', 'dim', 'function', 'private', 'public', 'sub'
  );

  (** A constant array of method names. **)
  strMethodType : Array[Low(TMethodType)..High(TMethodType)] Of String = (
    '', '', 'Sub', 'Function', ''
  );
  (** A constant array to define the property types. **)
  strPropertyType : Array[Low(TVBPropertyType)..High(TVBPropertyType)] Of String = (
    'Unknown', 'Get', 'Let', 'Set');

Implementation

End.
