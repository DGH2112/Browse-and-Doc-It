(**

  This module contains a class which loads and saves all the application options to an INI file.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Apr 2017

**)
Unit BADI.Options;

Interface

Uses
  Classes,
  Graphics,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class to define a set of options for the application. **)
  TBADIOptions = Class
  Strict Private
    Class Var
      (** This is a clas varaiable to hide and hold the BADI Options instance reference. **)
      FBADIOptionsInstance : TBADIOptions;
  Strict Private
    FOptions : TDocOptions;
    FDefines : TStringList;
    FSpecialTags : TStringList;
    FExpandedNodes : TStringList;
    FINIFileName : String;
    FUpdateInterval: Cardinal;
    FScopesToRender : TScopes;
    FBrowsePosition : TBrowsePosition;
    FFontName : String;
    FFontSize : Integer;
    FTokenFontInfo : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo;
    FExcludeDocFiles : TStringList;
    FMethodDescriptions : TStringList;
    FScopesToDocument : TScopes;
    FModuleExplorerBGColour : TColor;
    FTokenLimit : Integer;
    FMaxDocOutputWidth: Integer;
    FManagedNodesLife : Integer;
    FTreeColour : TColor;
    FProfilingCode : TStringList;
    FIssueLimits : Array[Low(TLimitType)..High(TLimitType)] Of Integer;
    FBADIMenuShortCuts : Array[Low(TBADIMenu)..High(TBADIMenu)] Of String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetTokenFontInfo(Const ATokenType  : TBADITokenType) : TTokenFontInfo;
    Procedure SetTokenFontInfo(Const ATokenType  : TBADITokenType;
      Const ATokenFontInfo : TTokenFontInfo);
    Function  GetProfilingCode(Const strModuleName: String): String;
    Procedure SetProfilingCode(Const strModuleName: String; Const strValue: String);
    Function  GetIssueLimit(Const LimitType : TLimitType) : Integer;
    Procedure SetIssueLimit(Const LimitType : TLimitType; Const iValue : Integer);
    Function  GetMenuShortcut(Const iBADIMenu : TBADIMenu) : String;
    Procedure SetMenuShortcut(Const iBADIMenu : TBADIMenu; Const strShortcut : String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Function BADIOptions : TBADIOptions;
    Procedure LoadSettings;
    Procedure SaveSettings;
    (**
      This property contains the basic toggleable options for the application.
      @precon  None.
      @postcon Contains the basic toggleable options for the application.
      @return  a TDocOptions
    **)
    Property Options : TDocOptions Read FOptions Write FOptions;
    (**
      This property provides a list of Compiler Conditional Defines.
      @precon  None.
      @postcon Provides a list of Compiler Conditional Defines.
      @return  a TStringList
    **)
    Property Defines : TStringList Read FDefines;
    (**
      This property provide access to the special tags string list.
      @precon  None.
      @postcon Provide access to the special tags string list.
      @return  a TStringList
    **)
    Property SpecialTags : TStringList Read FSpecialTags;
    (**
      This property provide access to the expanded nodes string list.
      @precon  None.
      @postcon Provide access to the expanded nodes string list.
      @return  a TStringList
    **)
    Property ExpandedNodes : TStringList Read FExpandedNodes;
    (**
      This property determines the amount of time in milliseonds between the
      last editor update and the next refresh. Interval only, the application
      needs to implement the logic.
      @precon  None.
      @postcon Gets and sets the update interval.
      @return  a Cardinal
    **)
    Property UpdateInterval : Cardinal Read FUpdateInterval Write FUpdateInterval;
    (**
      This property determines the scopes to render in the module explorer.
      @precon  None.
      @postcon Gets and sets the scopes to render.
      @return  a TScopes
    **)
    Property ScopesToRender : TScopes Read FScopesToRender Write FScopesToRender;
    (**
      This property determines the behaviour of the positioning of the cursor
      in the editor window.
      @precon  None.
      @postcon Gets and sets the browse position.
      @return  a TBrowsePosition
    **)
    Property BrowsePosition : TBrowsePosition Read FBrowsePosition Write FBrowsePosition;
    (**
      This property determines the Font Name of the Module explorer.
      @precon  None.
      @postcon Gets or sets the module explorer font name.
      @return  a String
    **)
    Property FontName : String Read FFontName Write FFontName;
    (**
      This property determines the font size of the module explorer font.
      @precon  None.
      @postcon Gets or sets the module explorer font size.
      @return  an Integer
    **)
    Property FontSize : Integer Read FFontSize Write FFontSize;
    (**
      This property determines the colour and style attribute of a token in the
      module explorer
      @precon  None.
      @postcon Gets and sets the colour and style of the token.
      @param   ATokenType as a TBADITokenType as a constant
      @return  a TTokenFontInfo
    **)
    Property TokenFontInfo[Const ATokenType : TBADITokenType] : TTokenFontInfo Read
      GetTokenFontInfo Write SetTokenFontInfo;
    (**
      This properrty holds a list of files / partial or full which should not be
      documented.
      @precon  None.
      @postcon Gets and sets the list.
      @return  a TStringList
    **)
    Property ExcludeDocFiles : TStringList Read FExcludeDocFiles;
    (**
      This property stores a list of method descriptions related to pattern
      matches.
      @precon  None.
      @postcon Gets and sets the method list.
      @return  a TStringList
    **)
    Property MethodDescriptions : TStringList Read FMethodDescriptions;
    (**
      This property determines the scopes to document.
      @precon  None.
      @postcon Gets and sets the scopes to document.
      @return  a TScopes
    **)
    Property ScopesToDocument : TScopes Read FScopesToDocument Write FScopesToDocument;
    (**
      This gets and sets the background colour for the Module explorer.
      @precon  None.
      @postcon Gets and sets the background colour for the Module explorer.
      @return  a TColor
    **)
    Property BGColour : TColor Read FModuleExplorerBGColour Write FModuleExplorerBGColour;
    (**
      This property gets ans sets the token limit to the module explorer.
      @precon  None.
      @postcon Gets ans sets the token limit to the module explorer.
      @return  an Integer
    **)
    Property TokenLimit : Integer Read FTokenLimit Write FTokenLimit;
    (**
      This property gets and sets the maximum width of the code output in
      documentation.
      @precon  None.
      @postcon Gets and sets the maximum width of the code output in
               documentation.
      @return  an Integer
    **)
    Property MaxDocOutputWidth : Integer Read FMaxDocOutputWidth Write FMaxDocOutputWidth;
    (**
      This property gets and sets the period of time in days which managed nodes
      are alive for.
      @precon  None.
      @postcon Gets and sets the period of time in days which managed nodes
               are alive for.
      @return  an Integer
    **)
    Property ManagedNodesLife : Integer Read FManagedNodesLife Write FManagedNodesLife;
    (**
      This property gets and sets the colour of the explorer tree lines.
      @precon  None.
      @postcon Gets and sets the colour of the explorer tree lines.
      @return  a TColor
    **)
    Property TreeColour : TColor Read FTreeColour Write FTreeColour;
    (**
      This property returns the name of the inifile.
      @precon  None.
      @postcon Returns the name of the inifile.
      @return  a String
    **)
    Property INIFileName : String Read FINIFileName;
    (**
      This property gets and sets the profiling code for the given filenames.
      @precon  None.
      @postcon Gets and sets the profiling code for the given filenames.
      @param   strModuleName as a String as a constant
      @return  a String
    **)
    Property ProfilingCode[Const strModuleName : String] : String Read GetProfilingCode
      Write SetProfilingCode;
    (**
      This property gets and sets the numerical limits for the output of errors, warnings,
      hints and conflicts.
      @precon  None.
      @postcon Gets and sets the numerical limits for the output of errors, warnings,
               hints and conflicts.
      @param   LimitType as a TLimitType as a constant
      @return  an Integer
    **)
    Property IssueLimits[Const LimitType : TLimitType] : Integer Read GetIssueLimit
      Write SetIssueLimit;
    (**
      This property provide access to the string representation of the menu shortcuts.
      @precon  None.
      @postcon Gets and sets the string representation of the menu shortcuts.
      @param   BADIMenu as a TBADIMenu as a constant
      @return  a String
    **)
    Property MenuShortcut[Const BADIMenu : TBADIMenu] : String Read GetMenuShortcut
      Write SetMenuShortcut;
  End;

Implementation

Uses
  SysUtils,
  BADI.Constants,
  IniFiles,
  BADI.Module.Dispatcher,
  BADI.Functions;

(**

  This class method returns the singleton instance of the BADI Options class.

  @precon  None.
  @postcon returns the instance of the BADI Options (and creates it if it hasnt alrady been done).

  @return  a TBADIOptions

**)
Class Function TBADIOptions.BADIOptions: TBADIOptions;

Begin
  If Not Assigned(FBADIOptionsInstance) Then
    FBADIOptionsInstance := TBADIOptions.Create;
  Result := FBADIOptionsInstance;
End;

(**

  This is the constructor method for the TBrowseAndDocItOptions class.

  @precon  None.
  @postcon Does nothing at the moment.

**)
Constructor TBADIOptions.Create;

Var
  setTPDefOps : TBADITagProperties;

Begin
  Inherited Create;
  FDefines := TStringList.Create;
  FSpecialTags := TStringList.Create;
  // Create a default set of Special Tags.
  setTPDefOps := [tpShowInTree, tpAutoExpand, tpShowInDoc];
  FSpecialTags.AddObject('todo=Things To Do', TObject(Byte(setTPDefOps)));
  FSpecialTags.AddObject('precon=Pre-Conditions', Nil);
  FSpecialTags.AddObject('postcon=Post-Conditions', Nil);
  FSpecialTags.AddObject('param=Parameters', Nil);
  FSpecialTags.AddObject('return=Returns', Nil);
  FSpecialTags.AddObject('note=Notes', Nil);
  FSpecialTags.AddObject('see=Also See', Nil);
  FSpecialTags.AddObject('exception=Exception Raised', Nil);
  FSpecialTags.AddObject('bug=Known Bugs', TObject(Byte(setTPDefOps)));
  FSpecialTags.AddObject('debug=Debugging Code', TObject(Byte(setTPDefOps)));
  FSpecialTags.AddObject('date=Date Code Last Updated', Nil);
  FSpecialTags.AddObject('author=Code Author', Nil);
  FSpecialTags.AddObject('version=Code Version', Nil);
  FSpecialTags.AddObject('refactor=Refactorings', TObject(Byte(setTPDefOps)));
  setTPDefOps := [tpFixed];
  FSpecialTags.AddObject('code=Code Example', TObject(Byte(setTPDefOps)));
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  FINIFileName := BuildRootKey;
  FScopesToRender := [scPrivate, scProtected, scPublic, scPublished];
  FExcludeDocFiles := TStringList.Create;
  FMethodDescriptions := TStringList.Create;
  FScopesToDocument := [scPublished, scPublic, scProtected, scPrivate];
  FProfilingCode := TStringList.Create;
End;

(**

  This is the destructor method for the TBrowseAndDocItOptions class.

  @precon  none.
  @postcon Does onthing at the moment except call the inherited destroy method.

**)
Destructor TBADIOptions.Destroy;

Begin
  SaveSettings;
  FProfilingCode.Free;
  FMethodDescriptions.Free;
  FExcludeDocFiles.Free;
  FExpandedNodes.Free;
  FSpecialTags.Free;
  FDefines.Free;
  Inherited Destroy;
  FBADIOptionsInstance := Nil;
End;

(**

  This is a getter method for the IssueLimit property.

  @precon  None.
  @postcon Returns the numerical limit for the given limit type.

  @param   LimitType as a TLimitType as a constant
  @return  an Integer

**)
Function TBADIOptions.GetIssueLimit(Const LimitType: TLimitType): Integer;

Begin
  Result := FIssueLimits[LimitType];
End;

(**

  This is a getter method for the MenuShortcut property.

  @precon  None.
  @postcon Returns the string representation of the enumerated menu shortcut.

  @param   iBADIMenu as a TBADIMenu as a constant
  @return  a String

**)
Function TBADIOptions.GetMenuShortcut(Const iBADIMenu: TBADIMenu): String;

Begin
  Result := FBADIMenuShortCuts[iBADIMenu];
End;

(**

  This is a getter method for the ProfilingCode property.

  @precon  None.
  @postcon Returns the profiling code template for the given filename.

  @param   strModuleName as a String as a constant
  @return  a String

**)
Function TBADIOptions.GetProfilingCode(Const strModuleName : String): String;
var
  iModule: Integer;

Begin
  Result := StringReplace(FProfilingCode.Values[strModuleName], '|', #13#10, [rfReplaceAll]);
  If Result = '' Then
    For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
      If CompareText(TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName, strModuleName) = 0 Then
        Begin
          Result := TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.DefaultProfilingTemplate;
          Break;
        End;
End;

(**


  This is a getter method for the TokenFontInfo property.

  @precon  None.
  @postcon Retursn the record information for the token type.


  @param   ATokenType as a TBADITokenType as a constant
  @return  a TTokenFontInfo

**)
Function TBADIOptions.GetTokenFontInfo(Const ATokenType: TBADITokenType): TTokenFontInfo;
Begin
  Result := FTokenFontInfo[ATokenType];
End;

(**

  This method loads the applications settings from an ini file.

  @precon  None.
  @postcon Loads the applications settings from an ini file.

**)
Procedure TBADIOptions.LoadSettings;

Var
  sl: TStringList;
  i: TDocOption;
  j: Integer;
  iValue: Integer;
  T: TBADITokenType;
  strLine: String;
  iBADIMenu: TBADIMenu;
  iModule: Integer;

Begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) To High(TDocOption) Do
        If ReadBool('Options', DocOptionInfo[i].FDescription, DocOptionInfo[i].FEnabled) Then
          FOptions := FOptions + [i]
        Else
          FOptions := FOptions - [i];
      sl := TStringList.Create;
      Try
        ReadSection('SpecialTagNames', sl);
        If sl.Count > 0 Then
          FSpecialTags.Clear;
        For j := 0 To sl.Count - 1 Do
          FSpecialTags.AddObject(Format('%s=%s', [sl[j], ReadString('SpecialTagNames', sl[j], '')]),
            TObject(ReadInteger('SpecialTags', sl[j], 0)));
        ReadSection('ManagedExpandedNodes', sl);
        For j := 0 To sl.Count - 1 Do
          Begin
            iValue := ReadInteger('ManagedExpandedNodes', sl[j], 0);
            FExpandedNodes.AddObject(StringReplace(sl[j], '|', '=', [rfReplaceAll]),
              TObject(iValue));
          End;
      Finally
        sl.Free;
      End;
      FUpdateInterval := ReadInteger('ModuleExplorer', 'UpdateInterval', 1000);
      FScopesToRender := TScopes(Byte(ReadInteger('ModuleExplorer', 'ScopesToRender',
        Byte(FScopesToRender))));
      FBrowsePosition := TBrowsePosition(ReadInteger('Setup', 'BrowsePosition',
        Integer(bpIdentifierCentreShowAllComment)));
      FFontName := ReadString('ModuleExplorer', 'Name', 'MS Sans Serif');
      FFontSize := ReadInteger('ModuleExplorer', 'Size', 8);
      For T := Low(TBADITokenType) To High(TBADITokenType) Do
        Begin
          FTokenFontInfo[T].FForeColour :=
            StringToColor(ReadString('TokenFontinfo', Format('%s.Colour', [strTokenType[T]]),
            ColorToString(strTokenTypeInfo[T].FForeColour)));
          FTokenFontInfo[T].FStyles :=
            TFontStyles(Byte(ReadInteger('TokenFontinfo', Format('%s.Styles', [strTokenType[T]]),
            Byte(strTokenTypeInfo[T].FStyles))));
          FTokenFontInfo[T].FBackColour :=
            StringToColor(ReadString('TokenFontinfo', Format('%s.BackColour', [strTokenType[T]]),
            ColorToString(strTokenTypeInfo[T].FBackColour)));
        End;
      FExcludeDocFiles.Text := StringReplace(ReadString('Setup', 'ExcludeDocFiles', ''), '|',
        #13#10, [rfReplaceAll]);
      sl := TStringList.Create;
      Try
        ReadSection('MethodDescriptions', sl);
        For j := 0 To sl.Count - 1 Do
          FMethodDescriptions.Add(Format('%s=%s', [sl[j], ReadString('MethodDescriptions',
            sl[j], '')]));
      Finally
        sl.Free;
      End;
      FScopesToDocument :=
        TScopes(Byte(ReadInteger('Documentation', 'Scopes', Byte(FScopesToDocument))));
      FModuleExplorerBGColour := StringToColor(ReadString('ModuleExplorer', 'BGColour',
        ColorToString(clWindow)));
      FTokenLimit := ReadInteger('ModuleExplorer', 'TokenLimit', 50);
      FMaxDocOutputWidth := ReadInteger('Documentation', 'MaxDocOutputWidth', 80);
      FManagedNodesLife := ReadInteger('ModuleExplorer', 'ManagedNodesLife', 90);
      FTreeColour := StringToColor(ReadString('ModuleExplorer', 'TreeColour', 'clGray'));
      sl := TStringList.Create;
      Try
        ReadSection('ProfilingCode', sl);
        For j := 0 To sl.Count - 1 Do
          Begin
            strLine := ReadString('ProfilingCode', sl[j], '');
            If strLine <> '' Then
              FProfilingCode.Values[sl[j]] := strLine;
          End;
      Finally
        sl.Free;
      End;
      FIssueLimits[ltErrors] := ReadInteger('Issues Limits', 'Errors', 10);
      FIssueLimits[ltWarnings] := ReadInteger('Issues Limits', 'Warnings', 10);
      FIssueLimits[ltHints] := ReadInteger('Issues Limits', 'Hints', 10);
      FIssueLimits[ltConflicts] := ReadInteger('Issues Limits', 'Conflicts', 10);
      For iBADIMenu := Low(TBADImenu) To High(TBADIMenu) Do
        FBADIMenuShortCuts[iBADIMenu] := ReadString('BADIMenuShortcuts',
          BADIMenus[iBADIMenu].FName,
          BADIMenus[iBADIMenu].FShortCut);
      For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
        TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions :=
          ReadString(
            'ModuleExtensions',
            TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
            TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions
          );
    Finally
      Free;
    End;
End;

(**

  This method saves the applications settings to an ini file.

  @precon  None.
  @postcon Saves the applications settings to an ini file.

**)
Procedure TBADIOptions.SaveSettings;

Var
  i: TDocOption;
  j: Integer;
  T: TBADITokenType;
  iBADIMenu : TBADIMenu;
  iModule: Integer;

Begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) To High(TDocOption) Do
        WriteBool('Options', DocOptionInfo[i].FDescription, i In FOptions);
      EraseSection('SpecialTags');
      EraseSection('SpecialTagNames');
      For j := 0 To FSpecialTags.Count - 1 Do
        Begin
          WriteInteger('SpecialTags', FSpecialTags.Names[j], Integer(FSpecialTags.Objects[j]));
          WriteString('SpecialTagNames', FSpecialTags.Names[j],
            FSpecialTags.Values[FSpecialTags.Names[j]]);
        End;
      EraseSection('ManagedExpandedNodes');
      For j := 0 To ExpandedNodes.Count - 1 Do
        WriteInteger('ManagedExpandedNodes', StringReplace(FExpandedNodes[j], '=', '|',
          [rfReplaceAll]), Integer(FExpandedNodes.Objects[j]));
      WriteInteger('ModuleExplorer', 'UpdateInterval', FUpdateInterval);
      WriteInteger('ModuleExplorer', 'ScopesToRender', Byte(FScopesToRender));
      WriteInteger('Setup', 'BrowsePosition', Integer(FBrowsePosition));
      WriteString('ModuleExplorer', 'Name', FFontName);
      WriteInteger('ModuleExplorer', 'Size', FFontSize);
      For T := Low(TBADITokenType) To High(TBADITokenType) Do
        Begin
          WriteString('TokenFontinfo', Format('%s.Colour', [strTokenType[T]]),
            ColorToString(FTokenFontInfo[T].FForeColour));
          WriteInteger('TokenFontinfo', Format('%s.Styles', [strTokenType[T]]),
            Byte(FTokenFontInfo[T].FStyles));
          WriteString('TokenFontinfo', Format('%s.BackColour', [strTokenType[T]]),
            ColorToString(FTokenFontInfo[T].FBackColour));
        End;
      WriteString('Setup', 'ExcludeDocFiles', StringReplace(FExcludeDocFiles.Text, #13#10, '|',
        [rfReplaceAll]));
      EraseSection('MethodDescriptions');
      For j := 0 To FMethodDescriptions.Count - 1 Do
        {$IFDEF D0006}
        WriteString('MethodDescriptions', FMethodDescriptions.Names[j],
          FMethodDescriptions.ValueFromIndex[j]);
        {$ELSE}
        WriteString('MethodDescriptions', FMethodDescriptions.Names[j],
          FMethodDescriptions.Values[FMethodDescriptions.Names[j]]);
      {$ENDIF}
      WriteInteger('Documentation', 'Scopes', Byte(FScopesToDocument));
      WriteString('ModuleExplorer', 'BGColour', ColorToString(FModuleExplorerBGColour));
      WriteInteger('ModuleExplorer', 'TokenLimit', FTokenLimit);
      WriteInteger('Documentation', 'MaxDocOutputWidth', FMaxDocOutputWidth);
      WriteInteger('ModuleExplorer', 'ManagedNodesLife', FManagedNodesLife);
      WriteString('ModuleExplorer', 'TreeColour', ColorToString(FTreeColour));
      EraseSection('ProfilingCode');
      For j := 0 To FProfilingCode.Count - 1 Do
        If FProfilingCode.Names[j] <> '' Then
          {$IFDEF D0006}
          WriteString('ProfilingCode', FProfilingCode.Names[j], FProfilingCode.ValueFromIndex[j]);
          {$ELSE}
          WriteString('ProfilingCode', FProfilingCode.Names[j],
            FProfilingCode.Values[FProfilingCode.Names[j]]);
      {$ENDIF}
      WriteInteger('Issues Limits', 'Errors', FIssueLimits[ltErrors]);
      WriteInteger('Issues Limits', 'Warnings', FIssueLimits[ltWarnings]);
      WriteInteger('Issues Limits', 'Hints', FIssueLimits[ltHints]);
      WriteInteger('Issues Limits', 'Conflicts', FIssueLimits[ltConflicts]);
      For iBADIMenu := Low(TBADImenu) To High(TBADIMenu) Do
        WriteString('BADIMenuShortcuts',
          BADIMenus[iBADIMenu].FName,
          FBADIMenuShortCuts[iBADIMenu]);
      For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
        WriteString(
          'ModuleExtensions',
          TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
          TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions
        );
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This is a setter method for the IssueLimit property.

  @precon  None.
  @postcon Sets the numerical limit for the given limit type.

  @param   LimitType as a TLimitType as a constant
  @param   iValue as an Integer as a constant

**)
Procedure TBADIOptions.SetIssueLimit(Const LimitType: TLimitType; Const iValue: Integer);

Begin
  FIssueLimits[LimitType] := iValue;
End;

(**

  This is a setter method for the MenuShortcut property.

  @precon  None.
  @postcon Sets the string representation of the enumerated menu shortcut.

  @param   iBADIMenu   as a TBADIMenu as a constant
  @param   strShortcut as a String as a constant

**)
Procedure TBADIOptions.SetMenuShortcut(Const iBADIMenu: TBADIMenu;
  Const strShortcut: String);

Begin
  If FBADIMenuShortCuts[iBADIMenu] <> strShortcut Then
    FBADIMenuShortCuts[iBADIMenu] := strShortcut;
End;

(**

  This is a setter method for the ProfilingCode property.

  @precon  None.
  @postcon saves the profiling code for the given filename.

  @param   strModuleName as a String as a constant
  @param   strValue      as a String as a constant

**)
Procedure TBADIOptions.SetProfilingCode(Const strModuleName : String; Const strValue: String);

Begin
  FProfilingCode.Values[strModuleName] := StringReplace(strValue, #13#10, '|', [rfReplaceAll]);
End;

(**


  This is a setter method for the TokenFontInfo property.

  @precon  None.
  @postcon Sets the indexed Token Font Information record.


  @param   ATokenType     as a TBADITokenType as a constant
  @param   ATokenFontInfo as a TTokenFontInfo as a constant

**)
Procedure TBADIOptions.SetTokenFontInfo(Const ATokenType: TBADITokenType;
  Const ATokenFontInfo: TTokenFontInfo);

Begin
  FTokenFontInfo[ATokenType] := ATokenFontInfo;
End;

End.
