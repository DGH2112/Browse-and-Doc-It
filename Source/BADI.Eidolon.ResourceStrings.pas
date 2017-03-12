(**

  This module contains resource strings for use with the Eidolon module parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.ResourceStrings;

Interface

ResourceString
  (** A resource string for the Text Table definitions. **)
  strTextTableDefsLabel = 'Text Table Definitions';
  (** A resource string for the Database Table definitions. **)
  strDatabaseTableDefsLabel = 'Database Table Definitions';
  (** A resource string for the Time Location Table definitions. **)
  strTimeLocationTableDefsLabel = 'Time Location Table Definitions';
  (** A resource string for the Output Table definitions. **)
  strOutputTableDefsLabel = 'Output Table Definitions';
  (** A resource string for the Requirements Table definitions. **)
  strRequirementsTableDefsLabel = 'Requirements Table Definitions';
  (** A resource string for the Database reference. **)
  strDatabaseLabel = 'Database';
  (** A resource string for the Connection reference. **)
  strConnectionLabel = 'Connection';
  (** A resource string for the Table Name reference. **)
  strTableNameLabel = 'Tablename';
  (** A resource string for the Primary reference. **)
  strPrimaryLabel = 'Primary';
  (** A resource string for the Symbols reference. **)
  strSymbolsLabel = 'Symbols';

  (** A resource string for an error message where a line end token was expected. **)
  strExpectedLineEnd = 'Expected <LineEnd> but found ''%s'' at line %d colum' +
  'n %d.';
  (** A resource string for an error message where a name is null. **)
  strTheDefintionNull = 'The defintion name can not be null at line %d column' +
  ' %d.';
  (** A resource string for an invalid data type. **)
  strInvalidDataType = 'Invalid data type ''%s'' found at line %d column %d.';
  (** A resource string for an invalid data width. **)
  strInvalidDataWidth = 'Invalid data width ''%s'' at line %d colunm %d.';
  (** A resource string for an invalid colour name. **)
  strInvalidColourName = 'Invalid colour name ''%s'' at line %d column %d.';
  (** A resource string for an invalid line style. **)
  strInvalidLineStyle = 'Invalid line style ''%s'' at line %d column %d.';
  (** A resource string for an invalid line weight. **)
  strInvalidLineWeight = 'Invalid line weight ''%s'' at line %d column %d.';
  (** A resource string for an invalid pattern. **)
  strInvalidPattern = 'Invalid pattern ''%s'' at line %d column %d.';
  (** A resource string for an invalid number. **)
  strInvalidNumber = 'Invalid number ''%s'' at line %d column %d.';
  (** A resource string for an invalid line end type. **)
  strInvalidLineType = 'Invalid Line Type ''%s'' at line %d column %d.';
  (** A resource string for an invalid line end size. **)
  strInvalidLineSize = 'Invalid Line Size ''%s'' at line %d column %d.';
  (** A resource string for an interior colour with no pattern **)
  strYouHaveSpecifiedAColour = 'You have specified an interior colour ''%s''' +
  ' when the pattern is None';
  (** A resource string for an pattern and no colour. *)
  strYouHaveSpecifiedAPattern = 'You have specified a pattern ''%s'' but no ' +
  'interior colour.';
  (** A resource string for an invalid table type. *)
  strInvalidTableType = 'Invalid table type ''%s'' at line %d column %d.';
  (** A resource string for an null name message. **)
  strNullName = 'The name can not by null at line %d column %d.';
  (** A resource string for an invalid connection string. **)
  strIsNotAValidConnection = '''%s'' is not a valid connection string at lin' +
  'e %d column %d.';
  //(** A resource string for an error with LINE border colours. **)
  //strLineColourOfNoneError = 'You can not have a border colour of NONE for a' +
  //  ' LINE at line %d column %d. If you do not want to see the line, either ma' +
  //  'ke it the same colour as the interior or use a line weight of NONE. ';

Implementation

End.
