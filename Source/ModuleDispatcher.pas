(**

  This module containers code to create a module parser from a passed stream
  and an enumerate for the type of code.

  @Author  David Hoyle
  @Date    10 Aug 2008
  @Version 1.0

**)
Unit ModuleDispatcher;

Interface

Uses
  SysUtils, Classes, BaseLanguageModule;

Function Dispatcher(Source : TStream; strFileName : String;
  boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;

  Implementation

Uses
  Windows, PascalDocModule;

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
  strExt : String;

Begin
  Result := Nil;
  strExt := ExtractFileExt(strFileName);
  If AnsiCompareText(strExt, '.dpk') = 0 Then
    Result := TPascalModule.Create(Source, strFileName, boolModified, ModuleOptions);
  If AnsiCompareText(strExt, '.dpr') = 0 Then
    Result := TPascalModule.Create(Source, strFileName, boolModified, ModuleOptions);
  If AnsiCompareText(strExt, '.pas') = 0 Then
    Result := TPascalModule.Create(Source, strFileName, boolModified, ModuleOptions);
End;

End.
