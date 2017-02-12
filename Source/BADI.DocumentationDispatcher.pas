(**
  
  This module contains a function to determining the type of documentation
  that should be produced.

  @Author  David Hoyle
  @Date    12 Feb 2017
  @Version 1.0

**)
Unit BADI.DocumentationDispatcher;

Interface

Uses
  BADI.BaseDocumentation;

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

  Function DocumentDispatcher(strOutputDirectory, strTitle : String;
    ADocType : TDocType) : TBaseDocumentation;

Implementation

Uses
  SysUtils, BADI.HTMLDocumentation;

(**

  This function returns a documentation instance for the type of documentation 
  requested. 

  @precon  None. 
  @postcon Returns a documentation instance for the type of documentation 
           requested. 

  @param   strOutputDirectory as a String
  @param   strTitle           as a String
  @param   ADocType           as a TDocType
  @return  a TBaseDocumentation

**)
Function DocumentDispatcher(strOutputDirectory, strTitle : String;
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