(**

  This module containers code to create a module parser from a passed stream
  and an enumerate for the type of code.

  @Author  David Hoyle
  @Date    02 Jul 2010
  @Version 1.0

**)
Unit ModuleDispatcher;

Interface

Uses
  SysUtils, Classes, BaseLanguageModule, CommonIDEFunctions, PascalModule,
  VBModule, BackusNaurModule, XMLModule, DFMModule, EidolonModule;

  Function Dispatcher(Source : String; strFileName : String;
    boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;
  Function CanParseDocument(strFileName : String) : Boolean;
  Function CanDocumentDocument(strFileName : String) : Boolean;
  Function GetCommentType(strFileName : String;
    CommentStyle : TCommentStyle) : TCommentType;


Type
  (** A class type to define classes in the record structure. **)
  TBaseLanguageModuleClass = Class Of TBaseLanguageModule;

  (** A record to describe the file extensions and parser modules. **)
  TDispatcherInfo = Record
    FExt       : String;
    FCls       : TBaseLanguageModuleClass;
    FCanDoc    : Boolean;
    FBlockCmt  : TCommentType;
    FLineCmt   : TCommentType;
    FInSituCmt : TCommentType;
  End;

Const
  (** A constant array of file extensions with the appropriate parser modules. **)
  Modules : Array[0..13] of TDispatcherInfo = (
    (FExt: '.bas';  FCls: TVBModule        ; FCanDoc: True;  FBlockCmt: ctVBLine;      FLineCmt: ctVBLine;      FInSituCmt: ctVBLine),
    (FExt: '.bnf';  FCls: TBackusNaurModule; FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPBlock;    FInSituCmt: ctCPPBlock),
    (FExt: '.cls';  FCls: TVBModule        ; FCanDoc: True;  FBlockCmt: ctVBLine;      FLineCmt: ctVBLine;      FInSituCmt: ctVBLine),
    (FExt: '.dfm';  FCls: TDFMModule       ; FCanDoc: False; FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FExt: '.dpk';  FCls: TPascalModule    ; FCanDoc: True;  FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FExt: '.dpr';  FCls: TPascalModule    ; FCanDoc: True;  FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FExt: '.dtd';  FCls: TXMLModule       ; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML),
    (FExt: '.frm';  FCls: TVBModule        ; FCanDoc: True;  FBlockCmt: ctVBLine;      FLineCmt: ctVBLine;      FInSituCmt: ctVBLine),
    (FExt: '.htm';  FCls: TXMLModule       ; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML),
    (FExt: '.html'; FCls: TXMLModule       ; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML),
    (FExt: '.map';  FCls: TEidolonModule   ; FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPLine;     FInSituCmt: ctCPPBlock),
    (FExt: '.pas';  FCls: TPascalModule    ; FCanDoc: True;  FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FExt: '.xml';  FCls: TXMLModule       ; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML),
    (FExt: '.xsd';  FCls: TXMLModule       ; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML)
  );

Implementation

Uses
  Windows;
(**

  This function returns the index of the parser information corresponding to the
  passed file extension. If there is no match 0 is returned.

  @precon  None.
  @postcon Returns the index of the parser information corresponding to the
           passed file extension. If there is no match 0 is returned.

  @param   strExt as a String
  @return  an Integer

**)
Function Find(strExt : String) : Integer;

Var
  iFirst, iMid, iLast : Integer;
  i: Integer;

Begin
  Result := -1;
  iFirst := Low(Modules);
  iLast := High(Modules);
  While iFirst <= iLast Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      i := CompareText(Modules[iMid].FExt, strExt);
      If i = 0 Then
        Begin
          Result := iMid;
          Exit;
        End
      Else If i < 0 Then
        iFirst := iMid + 1
      Else
        iLast := iMid - 1;
    End;
End;

(**

  This function returns an instance of a TBaseLanguageModule assigned a specific
  language parser depending on the extension of the file passed.

  @precon  Source must be a valid TStream of charcters to parse.
  @postcon Returns an instance of a TBaseLanguageModule assigned a specific
           language parser depending on the extension of the file passed.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   boolModified  as a Boolean
  @param   ModuleOptions as a TModuleOptions
  @return  a TBaseLanguageModule

**)
Function Dispatcher(Source : String; strFileName : String;
  boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;

Var
  iIndex: Integer;

Begin
  Result := Nil;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Result := Modules[iIndex].FCls.CreateParser(Source, strFileName,
      boolModified, ModuleOptions);
End;

(**

  This method determines if the document can be documented in HTML, RTF, etc,
  i.e. your wouldn`t document a code type that you only wish to browse, say
  XML or HTML.

  @precon  None.
  @postcon Determines if the document can be documented in HTML, RTF, etc.

  @param   strFileName as a String
  @return  a Boolean

**)
Function CanDocumentDocument(strFileName : String) : Boolean;

Var
  iIndex: Integer;

Begin
  Result := False;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex  > -1 Then
    Result := Modules[iIndex].FCanDoc;
End;

(**


  This method determines if the file can be documented by the system.

  @precon  None.
  @postcon Determines if the file can be documented by the system.


  @param   strFileName as a String
  @return  a Boolean

**)
Function CanParseDocument(strFileName : String) : Boolean;

Begin
  Result := Find(ExtractFileExt(strFileName)) > -1;
End;

(**

  This method returns the type of comment required for the file name given and
  for the comment style given.

  @precon  None.
  @postcon Returns the type of comment required for the file name given and
           for the comment style given.

  @param   strFileName  as a String
  @param   CommentStyle as a TCommentStyle
  @return  a TCommentType

**)
Function GetCommentType(strFileName : String; CommentStyle : TCommentStyle) : TCommentType;

Var
  iIndex : Integer;

Begin
  Result := ctNone;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Case CommentStyle Of
      csBlock:  Result := Modules[iIndex].FBlockCmt;
      csLine:   Result := Modules[iIndex].FLineCmt;
      csInSitu: Result := Modules[iIndex].FInSituCmt;
    End;
End;

End.
