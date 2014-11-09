(**
  
  This module contains the code to intialise all the open tools API interfaces for the
  project.

  @Version 1.0
  @Author  David Hoyle
  @Date    23 Apr 2013
  
**)
Unit BADIInitialiseOTAInterfaces;

Interface

Uses
  ToolsAPI;

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

  Procedure Register;
  
  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
    RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation

Uses
  Forms,
  BrowseAndDocItWizard,
  DockableModuleExplorer,
  EditorNotifier,
  KeyboardBindings,
  BNFHighlighter,
  EidolonHighlighter,
  Windows,
  SysUtils;

Type
  (** An enumerate to define the type of wizard. **)
  TWizardType = (wtPackageWizard, wtDLLWizard);

Const
  (** A constant to represent the initial (failed) position of a wizard reference. **)
  iWizardFailState = -1;
  
{$IFDEF D2005}
Resourcestring
  (** This is a text string of revision from nil and a to z. **)
  strRevision = ' abcdefghijklmnopqrstuvwxyz';
  {$IFDEF VER170}
  (** This is a message string to appear in the BDS 2005 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2005';
  {$ENDIF}
  {$IFDEF VER180}
  (** This is a message string to appear in the BDS 2006 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2006';
  {$ENDIF}
  {$IFDEF VER190}
  (** This is a message string to appear in the CDS 2007 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for CodeGear RAD Studio 2007';
  {$ENDIF}
  {$IFDEF VER200}
  (** This is a message string to appear in the CRS 2009 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for CodeGear RAD Studio 2009';
  {$ENDIF}
  {$IFDEF VER210}
  (** This is a message string to appear in the ERS 2010 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio 2010';
  {$ENDIF}
  {$IFDEF VER220}
  (** This is a message string to appear in the ERS XE splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE';
  {$ENDIF}
  {$IFDEF VER230}
  (** This is a message string to appear in the ERS XE2 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE2';
  {$ENDIF}
  {$IFDEF VER240}
  (** This is a message string to appear in the ERS XE3 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE3';
  {$ENDIF}
  {$IFDEF VER250}
  (** This is a message string to appear in the ERS XE4 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE4';
  {$ENDIF}
  {$IFDEF VER260}
  (** This is a message string to appear in the ERS XE5 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE5';
  {$ENDIF}
  {$IFDEF VER270}
  (** This is a message string to appear in the ERS XE6 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE6';
  {$ENDIF}
  {$IFDEF VER280}
  (** This is a message string to appear in the ERS XE7 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Embarcadero RAD Studio XE7';
  {$ENDIF}
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'Freeware by David Hoyle (Build %d.%d.%d.%d)';

Var
  (** This is a handle for the splash screen bitmap resource **)
  bmSplashScreen : HBITMAP;
  (** This is a variable to hold the major version number for the package. **)
  iMajor : Integer;
  (** This is a variable to hold the minor version number for the package. **)
  iMinor : Integer;
  (** This is a variable to hold the bug fix version number for the package. **)
  iBugFix : Integer;
  (** This is a variable to hold the build number for the package. **)
  iBuild : Integer;
{$ENDIF}

Var
  (** This is an index for the wizard when register with the ide. Its required
      in order to remove it from memory. **)
  iWizardIndex : Integer = iWizardFailState;
  {$IFDEF D2005}
  (** This is an index for the editor notifier required when the package is
      unloaded **)
  iEditorIndex : Integer = iWizardFailState;
  {$ENDIF}
  (** This is a private reference for the Editor Notifier class when not
      registered with the IDE. **)
  objEditorNotifier : TEditorNotifier;
  (** An index for the keybinding nofitier - required when the package is
      unloaded **)
  iKeyBinding: Integer = iWizardFailState;
  (** An index for the BNF Highlighter notifier - required for unloading the
      highlighter. **)
  iBNFHighlighter : Integer = iWizardFailState;
  (** An index for the Eidolon Highlighter notifier - required for unloading the
      highlighter. **)
  iEidolonHighlighter : Integer = iWizardFailState;
  {$IFDEF D2005}
  (** An index for the About Box Plugin. - required for unloading the interface. **)
  iAboutPlugin : Integer;
  {$ENDIF}

(**

  This method initialise the wizard interfaces for both a Package and DLL expert.

  @precon  None.
  @postcon Returns the initialised main wizard interface.

  @param   WizardType as a TWizardType
  @return  a TBrowseAndDocItWizard

**)
Function InitialiseWizard(WizardType : TWizardType) : TBrowseAndDocItWizard;

Var
  Svcs: IOTAServices;

Begin
  Svcs := BorlandIDEServices As IOTAServices;
  ToolsAPI.BorlandIDEServices := BorlandIDEServices;
  Application.Handle := Svcs.GetParentHandle;
  TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
  Result := TBrowseAndDocItWizard.Create;
  If WizardType = wtPackageWizard Then
    iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Result);
  {$IFDEF D2005}
  objEditorNotifier := TEditorNotifier.Create;
  iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
    objEditorNotifier);
  {$ELSE}
  objEditorNotifier := TEditorNotifier.Create;
  {$ENDIF}
  Result.EditorNotifier := objEditorNotifier;
  iKeyBinding := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeyboardBinding.Create(Result));
  iBNFHighlighter := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TBNFHighlighter.Create);
  iEidolonHighlighter := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TEidolonHighlighter.Create);
  {$IFDEF D2010}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap');
  iAboutPlugin := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1)]),
    'An IDE expert to browse and document your source code.',
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
    Format('SKU Build %d.%d.%d.%d', [iMajor, iMinor, iBugfix, iBuild]));
  {$ENDIF}
End;

(**

  This is the modules registry procedure so that the Delphi IDE can registry
  the wizard.

  @precon  None.
  @postcon Creates the following wizards and notifiers:
           1) Browse And Doc It Wizard
           2) Editor Notifier
           3) Keyboard Binding Notifier

**)
Procedure Register();

Begin
  InitialiseWizard(wtPackageWizard);
End;

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Begin
  Result := BorlandIDEServices <> Nil;
  If Result Then
    RegisterProc(InitialiseWizard(wtDLLWizard));
End;

{$IFDEF D2005}
(**

  This is a method which obtains information about the package from is
  version information with the package resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer
  @param   iMinor  as an Integer
  @param   iBugFix as an Integer
  @param   iBuild  as an Integer

**)
Procedure BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strBuffer : Array[0..MAX_PATH] Of Char;

Begin
  { Build Number }
  GetModuleFilename(hInstance, strBuffer, MAX_PATH);
  VerInfoSize := GetFileVersionInfoSize(strBuffer, Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(strBuffer, 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        With VerValue^ Do
          Begin
            iMajor := dwFileVersionMS shr 16;
            iMinor := dwFileVersionMS and $FFFF;
            iBugFix := dwFileVersionLS shr 16;
            iBuild := dwFileVersionLS and $FFFF;
          End;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;


{$ENDIF}

(** This initialization section installs an IDE Splash Screen item. **)
Initialization
  {$IFDEF D2005}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap');
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1)]),
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]), ''
    );
  {$ENDIF}
(** This finalization section removes this wizard from the IDE when the package
    is unloaded. **)
Finalization
  If iEidolonHighlighter > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(iEidolonHighlighter);
  If iBNFHighlighter > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(iBNFHighlighter);
  If iKeyBinding > iWizardFailState Then
    (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBinding);
  {$IFDEF D2005}
  If iEditorIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAEditorServices).RemoveNotifier(iEditorIndex);
  {$ELSE}
  objEditorNotifier.Free;
  {$ENDIF}
  If iWizardIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  {$IFDEF D2010}
  If iAboutPlugin > iWizardFailState Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPlugin);
  {$ENDIF}
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End.
