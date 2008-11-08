(**

  This module containers code to create a module parser from a passed stream
  and an enumerate for the type of code.

  @Author  David Hoyle
  @Date    07 Nov 2008
  @Version 1.0

**)
Unit ModuleDispatcher;

Interface

Uses
  SysUtils, Classes, BaseLanguageModule;

  Function Dispatcher(Source : TStream; strFileName : String;
    boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;
  Function CanParseDocument(strFileName : String) : Boolean;

  Implementation

Uses
  Windows, PascalDocModule;

Type
  (** A class type to define classes in the record structure. **)
  TBaseLanguageModuleClass = Class Of TBaseLanguageModule;
  
  (** A record to describe the file extensions and parser modules. **)
  TDispatcherInfo = Record
    FExt : String;
    FCls : TBaseLanguageModuleClass;
  End;
  
Const
  (** A constant array of file extensions with the appropriate parser modules. **)
  Modules : Array[1..3] of TDispatcherInfo = (
    (FExt: '.dpk'; FCls: TPascalModule),
    (FExt: '.dpr'; FCls: TPascalModule),
    (FExt: '.pas'; FCls: TPascalModule)
  );

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
  Result := 0;
  iFirst := Low(Modules);
  iLast := High(Modules);
  While iFirst <= iLast Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      i := AnsiCompareText(Modules[iMid].FExt, strExt);
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

  @param   Source        as a TStream
  @param   strFileName   as a String
  @param   boolModified  as a Boolean
  @param   ModuleOptions as a TModuleOptions
  @return  a TBaseLanguageModule

**)
Function Dispatcher(Source : TStream; strFileName : String;
  boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;

Var
  iIndex: Integer;

Begin
  Result := Nil;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > 0 Then
    Result := Modules[iIndex].FCls.Create(Source, strFileName, boolModified,
      ModuleOptions);
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
  Result := Find(ExtractFileExt(strFileName)) > 0;
End;

End.
