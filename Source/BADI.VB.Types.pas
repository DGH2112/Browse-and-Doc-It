(**

  This module contains a number of VB specific general types for use in the parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Mar 2017

**)
Unit BADI.VB.Types;

Interface

Type
  (** A type to define at upper and lower limits of an array. **)
  TArrayDimensions = Array[1..2] Of String;

  (** A type to define the type of properties supported by visual basic. **)
  TVBPropertyType = (ptUnknown, ptGet, ptLet, ptSet);

  (** An enumerate to represent the different module types in visual basic. **)
  TVBModuleType = (mtModule, mtForm, mtClass);

Implementation

End.
