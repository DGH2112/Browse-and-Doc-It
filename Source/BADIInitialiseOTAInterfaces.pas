(**

  This module contains the code to intialise all the open tools API interfaces for the
  project.

  @Version 1.0
  @Author  David Hoyle
  @Date    30 Sep 2016

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
  SysUtils,
  Forms,
  Windows,
  BrowseAndDocItWizard,
  DockableModuleExplorer,
  BNFHighlighter,
  EidolonHighlighter,
  BADICommon,
  BADISplashScreen,
  BADIAboutBox;

Type
  (** An enumerate to define the type of wizard. **)
  TWizardType = (wtPackageWizard, wtDLLWizard);

Var
  (** This is an index for the wizard when register with the ide. Its required
      in order to remove it from memory. **)
  iWizardIndex : Integer = iWizardFailState;
  (** An index for the BNF Highlighter notifier - required for unloading the
      highlighter. **)
  iBNFHighlighter : Integer = iWizardFailState;
  (** An index for the Eidolon Highlighter notifier - required for unloading the
      highlighter. **)
  iEidolonHighlighter : Integer = iWizardFailState;

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
  iBNFHighlighter := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TBNFHighlighter.Create);
  iEidolonHighlighter := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TEidolonHighlighter.Create);
  {$IFDEF D2005}
  AddAboutBoxEntry;
  {$ENDIF}
End;

(**

  This is the modules registry procedure so that the Delphi IDE can registry
  the wizard.

  @precon  None.
  @postcon Creates the wizards and notifiers.

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

(** This initialization section installs an IDE Splash Screen item. **)
Initialization
  {$IFDEF D2005}
  AddSplashScreen;
  {$ENDIF}
(** This finalization section removes this wizard from the IDE when the package
    is unloaded. **)
Finalization
  If iEidolonHighlighter > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(iEidolonHighlighter);
  If iBNFHighlighter > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(iBNFHighlighter);
  If iWizardIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  {$IFDEF D2005}
  RemoveAboutBoxEntry;
  {$ENDIF}
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End.
