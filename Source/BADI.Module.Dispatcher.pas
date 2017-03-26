(**

  This module contains a class which registers the module parsers against file extensions and
  can therefore true the required parser for a given file extension.

  @Author  David Hoyle
  @Version 1.1
  @Date    26 Mar 2017

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
  TModuleDispatcher = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FModules: TObjectList;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function Find(Const strExt: String): Integer;
    Function GetCount: Integer;
    Function GetModules(Const iIndex: Integer): TModuleInfo;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Const Cls: TBaseLanguageModuleClass; const strExtensions: String;
      Const boolCanDoc: Boolean; Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType);
    Function Dispatcher(Const Source, strFileName: String; Const boolModified: Boolean;
      Const ModuleOptions: TModuleOptions): TBaseLanguageModule;
    Function CanParseDocument(Const strFileName: String): Boolean;
    Function CanDocumentDocument(Const strFileName: String): Boolean;
    Function GetCommentType(Const strFileName: String;
      Const CommentStyle: TCommentStyle) : TCommentType;
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

Var
  (** This is a global variable that is initialised by this module and available
      to all over modules so that they can register their information. **)
  ModuleDispatcher: TModuleDispatcher;

Implementation

Uses
  SysUtils;

(**

  This method adds a set of registration information into the dispatcher.

  @precon  None.
  @postcon Adds a set of registration information into the dispatcher.

  @param   Cls           as a TBaseLanguageModuleClass as a Constant
  @param   strExtensions as a String as a Constant
  @param   boolCanDoc    as a Boolean as a Constant
  @param   iBlockCmt     as a TCommentType as a Constant
  @param   iLineCmt      as a TCommentType as a Constant
  @param   iInSituCmt    as a TCommentType as a Constant

**)
Procedure TModuleDispatcher.Add(Const Cls: TBaseLanguageModuleClass; Const strExtensions: String;
  Const boolCanDoc: Boolean; Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType);

Var
  iModule: Integer;

Begin
  For iModule := 0 To FModules.Count - 1 Do
    If Modules[iModule].Cls = Cls Then
      Raise Exception.Create(Format( 'You cannot register the same module more than once (%s)',
        [Cls.ClassName]));
  FModules.Add(TModuleInfo.Create(Cls, strExtensions, boolCanDoc, iBlockCmt, iLineCmt, iInSituCmt));
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
Function TModuleDispatcher.CanDocumentDocument(Const strFileName: String): Boolean;

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
Function TModuleDispatcher.CanParseDocument(Const strFileName: String): Boolean;

Begin
  Result := Find(ExtractFileExt(strFileName)) > -1;
End;

(**

  A constructor for the TModuleDispatcher class.

  @precon  None.
  @postcon Creates a object list to contain the module registration information.

**)
Constructor TModuleDispatcher.Create;

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
Function TModuleDispatcher.Find(Const strExt: String): Integer;

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

  A destructor for the TModuleDispatcher class.

  @precon  None.
  @postcon Frees the memory used by the module registrations.

**)
Destructor TModuleDispatcher.Destroy;

Begin
  FModules.Free;
  Inherited Destroy;
End;

(**

  This function returns an instance of a TBaseLanguageModule assigned a specific
  language parser depending on the extension of the file passed.

  @precon  Source must be a valid TStream of charcters to parse.
  @postcon Returns an instance of a TBaseLanguageModule assigned a specific
           language parser depending on the extension of the file passed.

  @param   Source        as a String as a Constant
  @param   strFileName   as a String as a Constant
  @param   boolModified  as a Boolean as a Constant
  @param   ModuleOptions as a TModuleOptions as a Constant
  @return  a TBaseLanguageModule

**)
Function TModuleDispatcher.Dispatcher(Const Source, strFileName: String;
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
Function TModuleDispatcher.GetCommentType(Const strFileName: String;
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

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of registrations in the dispatcher.

  @return  an Integer

**)
Function TModuleDispatcher.GetCount: Integer;

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
Function TModuleDispatcher.GetModules(Const iIndex: Integer): TModuleInfo;

Begin
  Result := FModules[iIndex] As TModuleInfo;
End;

(** This initializations section ensures that there is a valid instance of the
    BrowseAndDocItOption class. **)
Initialization
  ModuleDispatcher := TModuleDispatcher.Create;
(** This finalization section ensures that the BrowseAndDocItOptions class are
    destroyed. **)
Finalization
  ModuleDispatcher.Free;
End.
