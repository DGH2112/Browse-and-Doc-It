(**

  This module contains resource strings specific to TLS Schematic diagrams.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Mar 2017

**)
Unit BADI.Eidolon.TLSSchematic.ResourceStrings;

Interface

ResourceString
  (** A resource string for the Roads node in the tree. **)
  strRoads = 'Roads';
  (** A resource string for the Objects node in the tree. **)
  strObjects = 'Objects';
  (** A resource string for named width objects in the tree. **)
  strNamedWidths = 'Named Widths';
  (** A resource string for the node underwhich suppressed text objects are
      listed. **)
  strNoTexts = 'Suppressed Text Objects';
  (** A resource string for the settings node. **)
  strSettings = 'Settings';
  (** A resource string for an unexpected token instead of an integer. **)
  strIntegerExpected = 'An integer number is expected but ''%s'' found at line %d column %d.';
  (** A resource string for an invalid colour name. **)
  (** A resource string for an invalid colour. **)
  strInvalidColourName = 'Invalid colour name ''%s'' at line %d column %d.';
  (** A resource string for an unexpected end chainage. **)
  strExpectedAnEndChainage = 'Expected an end chainage but ''%s'' found at l' +
  'ine %d column %d.';
  (** A resource string for an unexpected start chainage. **)
  strExpectedAStartChainage = 'Expected a start chainage but ''%s'' found at' +
  ' line %d column %d.';
  (** A resource string for an unexpected end offset. **)
  strExpectedAnEndOffset = 'Expected an end offset but found ''%s'' at line ' +
  '%d column %d.';
  (** A resource string for an unexpected start offset. **)
  strExpectedAStartOffet = 'Expected a start offset but found ''%s'' at line' +
  ' %d column %d.';
  (** An error message for chainages the wrong way around. **)
  strChainageError = 'The end chainage is less than or equal to the start ch' +
  'ainage (''%s'') at line %d column %d.';
  (** A resource string for an invalid line style. **)
  strInvalidLineStyle = 'Invalid line style ''%s'' at line %d column %d.';
  (** A resource string for an invalid line weight. **)
  strInvalidLineWeight = 'Invalid line weight ''%s'' at line %d column %d.';
  (** A resource string message for an invalid text orientation. **)
  strExpectedHORIZONTALVERTICAL = 'Expected HORIZONTAL or VERTICAL but found' +
  ' ''%s'' at line %d column %d.';
  (** A resource string message for an invalid text position. **)
  strExpectedOUTSIDEINSIDE = 'Expected OUTSIDE or INSIDE but found ''%s'' at' +
  ' line %d column %d.';

Implementation

End.
