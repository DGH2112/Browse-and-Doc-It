(**
  
  This module contains a function to determining the type of documentation
  that should be produced.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Documentation.Dispatcher;

Interface

Uses
  BADI.Base.Documentation;

Type
  (** This is an enumerate to define the types of documentation hat can be
      produced **)
  TDocType = (dtHTML {, dtRTF, dtWinHelp, dtHTMLHelp});

Const
  (** This is a constant array to define text representations of the document
      types available. **)
  strDocumentationTypes : Array[Low(TDocType)..High(TDocType)] Of String = (
    'HTML Documentation' {,
    'Rich Text Format Documentation',
    'Win32 Help Documentation',
    'HTML Help Documentation'}
  );

  Function DocumentDispatcher(Const strOutputDirectory, strTitle : String;
    ADocType : TDocType) : TBaseDocumentation;

Implementation

Uses
  SysUtils,
  BADI.HTMLDocumentation;

(**

  This function returns a documentation instance for the type of documentation
  requested.

  @precon  None.
  @postcon Returns a documentation instance for the type of documentation
           requested.

  @param   strOutputDirectory as a String as a Constant
  @param   strTitle           as a String as a Constant
  @param   ADocType           as a TDocType
  @return  a TBaseDocumentation

**)
Function DocumentDispatcher(Const strOutputDirectory, strTitle : String;
  ADocType : TDocType) : TBaseDocumentation;

ResourceString
  strUnsupportedDocumentationType = 'Unsupported Documentation Type!';

Begin
  Case ADocType Of
    dtHTML : Result := THTMLDocumentation.Create(strOutputDirectory, strTitle);
  Else
    Raise Exception.Create(strUnsupportedDocumentationType);
  End;
End;

End.