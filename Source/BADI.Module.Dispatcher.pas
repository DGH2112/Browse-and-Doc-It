(**

  This module contains a class which registers the module parsers against file extensions and
  can therefore true the required parser for a given file extension.

  @Author  David Hoyle
  @Version 1.287
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
Unit BADI.Module.Dispatcher;

Interface

Uses
  Classes,
  Contnrs,
  BADI.ModuleInfo,
  BADI.Base.Module,
  BADI.Types;

Type
  (** A class to handle all the registered modules in the system. **)
  TBADIDispatcher = Class
  Strict Private
    FModules: TObjectList;
  Strict Private
    Class Var
      (** This is a hidden class variable to hold the instance of the module dispatcher. **)
      FBADIModuleDispatcherInstance : TBADIDispatcher;
  Strict Protected
    Function Find(Const strExt: String): Integer;
    Function GetCount: Integer;
    Function GetModules(Const iIndex: Integer): TModuleInfo;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Function BADIDispatcher : TBADIDispatcher;
    Procedure Add(
      Const Cls: TBaseLanguageModuleClass;
      const strExtensions: String;
      Const boolCanDoc: Boolean;
      Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType;
      Const setCommentTypes : TCommentTypes
    );
    Function Dispatcher(Const Source, strFileName: String; Const boolModified: Boolean;
      Const ModuleOptions: TModuleOptions): TBaseLanguageModule;
    Function CanParseDocument(Const strFileName: String): Boolean;
    Function CanDocumentDocument(Const strFileName: String): Boolean;
    Function GetCommentType(Const strFileName: String; Const CommentStyle: TCommentStyle) : TCommentType;
    Function GetCommentTypes(Const strFileName: String) : TCommentTypes;
    (**
      This property returns a TModuleInfo reference for the indexed module.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon Returns a TModuleInfo reference for the indexed module.
      @param   iIndex as an Integer as a constant
      @return  a TModuleInfo
    **)
    Property Modules[Const iIndex: Integer]: TModuleInfo Read GetModules;
    (**
      This property returns the number of registered modules in the dispatcher.
      @precon  None.
      @postcon Returns the number of registered modules in the dispatcher.
      @return  an Integer
    **)
    Property Count: Integer Read GetCount;
  End;

Implementation

Uses
  SysUtils;

(**

  This method adds a set of registration information into the dispatcher.

  @precon  None.
  @postcon Adds a set of registration information into the dispatcher.

  @param   Cls             as a TBaseLanguageModuleClass as a constant
  @param   strExtensions   as a String as a constant
  @param   boolCanDoc      as a Boolean as a constant
  @param   iBlockCmt       as a TCommentType as a constant
  @param   iLineCmt        as a TCommentType as a constant
  @param   iInSituCmt      as a TCommentType as a constant
  @param   setCommentTypes as a TCommentTypes as a constant

**)
Procedure TBADIDispatcher.Add(
            Const Cls: TBaseLanguageModuleClass;
            Const strExtensions: String;
            Const boolCanDoc: Boolean;
            Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType;
            Const setCommentTypes : TCommentTypes
          );

Var
  iModule: Integer;

Begin
  For iModule := 0 To FModules.Count - 1 Do
    If Modules[iModule].Cls = Cls Then
      Raise Exception.Create(Format( 'You cannot register the same module more than once (%s)',
        [Cls.ClassName]));
  FModules.Add(
    TModuleInfo.Create(Cls, strExtensions, boolCanDoc, iBlockCmt, iLineCmt, iInSituCmt, setCommentTypes)
  );
End;

(**

  This class method returns the singleton instance of the BADI Module Dispatcher.

  @precon  None.
  @postcon An instance of the BAID Module Dispatcher is returned (and created if it has already
           been).

  @return  a TBADIDispatcher

**)
Class Function TBADIDispatcher.BADIDispatcher: TBADIDispatcher;

Begin
  If Not Assigned(FBADIModuleDispatcherInstance) Then
    FBADIModuleDispatcherInstance := TBADIDispatcher.Create;
  Result := FBADIModuleDispatcherInstance;
End;

(**

  This method determines if the document can be documented in HTML, RTF, etc,
  i.e. your wouldn`t document a code type that you only wish to browse, say
  XML or HTML.

  @precon  None.
  @postcon Determines if the document can be documented in HTML, RTF, etc.

  @param   strFileName as a String as a Constant
  @return  a Boolean

**)
Function TBADIDispatcher.CanDocumentDocument(Const strFileName: String): Boolean;

Var
  iIndex: Integer;

Begin
  Result := False;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Result := Modules[iIndex].CanDoc;
End;

(**


  This method determines if the file can be documented by the system.

  @precon  None.
  @postcon Determines if the file can be documented by the system.


  @param   strFileName as a String as a Constant
  @return  a Boolean

**)
Function TBADIDispatcher.CanParseDocument(Const strFileName: String): Boolean;

Begin
  Result := Find(ExtractFileExt(strFileName)) > -1;
End;

(**

  A constructor for the TBADIDispatcher class.

  @precon  None.
  @postcon Creates a object list to contain the module registration information.

**)
Constructor TBADIDispatcher.Create;

Begin
  FModules := TObjectList.Create(True);
End;

(**

  This function returns the index of the parser information corresponding to the
  passed file extension. If there is no match 0 is returned.

  @precon  None.
  @postcon Returns the index of the parser information corresponding to the
           passed file extension. If there is no match 0 is returned.

  @param   strExt as a String as a Constant
  @return  an Integer

**)
Function TBADIDispatcher.Find(Const strExt: String): Integer;

Var
  iModule: Integer;

Begin
  Result := -1;
  For iModule := 0 To FModules.Count - 1 Do
    If Modules[iModule].CanProcessExt(strExt) Then
      Begin
        Result := iModule;
        Break;
      End;
End;

(**

  A destructor for the TBADIDispatcher class.

  @precon  None.
  @postcon Frees the memory used by the module registrations.

**)
Destructor TBADIDispatcher.Destroy;

Begin
  FModules.Free;
  Inherited Destroy;
  FBADIModuleDispatcherInstance := Nil;
End;

(**

  This function returns an instance of a TBaseLanguageModule assigned a specific
  language parser depending on the extension of the file passed.

  @precon  Source must be a valid TStream of characters to parse.
  @postcon Returns an instance of a TBaseLanguageModule assigned a specific
           language parser depending on the extension of the file passed.

  @param   Source        as a String as a Constant
  @param   strFileName   as a String as a Constant
  @param   boolModified  as a Boolean as a Constant
  @param   ModuleOptions as a TModuleOptions as a Constant
  @return  a TBaseLanguageModule

**)
Function TBADIDispatcher.Dispatcher(Const Source, strFileName: String;
  Const boolModified: Boolean; Const ModuleOptions: TModuleOptions): TBaseLanguageModule;

Var
  iIndex: Integer;

Begin
  Result := Nil;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Result := (FModules[iIndex] As TModuleInfo).Cls.CreateParser(Source, strFileName, boolModified,
      ModuleOptions);
End;

(**

  This method returns the type of comment required for the file name given and
  for the comment style given.

  @precon  None.
  @postcon Returns the type of comment required for the file name given and
           for the comment style given.

  @param   strFileName  as a String as a Constant
  @param   CommentStyle as a TCommentStyle as a Constant
  @return  a TCommentType

**)
Function TBADIDispatcher.GetCommentType(Const strFileName: String;
  Const CommentStyle: TCommentStyle) : TCommentType;

Var
  iIndex: Integer;

Begin
  Result := ctNone;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Case CommentStyle Of
      csBlock:  Result := (FModules[iIndex] As TModuleInfo).BlockCmt;
      csLine:   Result := (FModules[iIndex] As TModuleInfo).LineCmt;
      csInSitu: Result := (FModules[iIndex] As TModuleInfo).InSituCmt;
    End;
End;

Function TBADIDispatcher.GetCommentTypes(Const strFileName: String) : TCommentTypes;

Var
  iIndex: Integer;

Begin
  Result := [];
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Result := (FModules[iIndex] As TModuleInfo).CommentTypes;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of registrations in the dispatcher.

  @return  an Integer

**)
Function TBADIDispatcher.GetCount: Integer;

Begin
  Result := FModules.Count;
End;

(**

  This is a getter method for the Modules property.

  @precon  iIndex must be between 0 and Count - 1.
  @postcon Returns a reference to the indexed module registration.

  @param   iIndex as an Integer as a Constant
  @return  a TModuleInfo

**)
Function TBADIDispatcher.GetModules(Const iIndex: Integer): TModuleInfo;

Begin
  Result := FModules[iIndex] As TModuleInfo;
End;

End.
