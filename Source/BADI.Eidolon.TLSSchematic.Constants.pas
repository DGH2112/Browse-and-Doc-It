(**

  This module contains constants specific to TLS Schematic diagrams.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Mar 2017

**)
Unit BADI.Eidolon.TLSSchematic.Constants;

Interface

Uses
  BADI.Eidolon.Types;

Const
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', '.', '/',
    ':', '=', '@', '[', ']', '^', '|', '%', '-'];
  (** A set of characters for single quotes **)
  strSingleQuotes : Set Of AnsiChar = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes : Set Of AnsiChar = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers :  Set Of AnsiChar = ['_', 'a'..'z', 'A'..'Z', '<', '>'];
  (** A set of number characters. **)
  strNumbers:  Set Of AnsiChar = ['#', '$', '0'..'9'];

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[0..16] Of String = ('centreline', 'debug',
    'diamond', 'ellipse', 'lines', 'margins', 'notext', 'object', 'objects',
    'road', 'roads', 'spacing', 'staticdiamond', 'staticellipse',
    'staticobject', 'text', 'textorientation');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..1] Of String = (';');

  (** A constant array of shape types. **)
  ObjectType : Array [Low(TSymbolType)..High(TSymbolType)] Of String = (
    'OBJECT', '', '', 'ELLIPSE', '', 'DIAMOND', '');

Implementation

End.
